{-# LANGUAGE
ScopedTypeVariables
, GeneralizedNewtypeDeriving
, FlexibleContexts
, DataKinds
, TypeOperators
#-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module ChooseFinite (
chooseAndSwapIndicesToK
, indexPermutation
) where

import System.Random (Random(..))
import Data.Finite
import GHC.List (take)
import GHC.TypeLits

import Data.Traversable
import Control.Monad (forM_)
import Control.Monad.Random (MonadInterleave, getRandomR, interleave)
import Control.Monad.Primitive (PrimMonad, PrimState)

import qualified Data.Vector.Generic.Mutable.Base as V
import qualified Data.Vector.Generic.Mutable.Sized as VS


newtype RFinite a = RFinite { getFinite :: Finite a }
  deriving (Eq, Ord, Num, Real, Integral, Enum, Bounded)

instance (KnownNat n) => Random (RFinite n) where
  randomR (a, b) g = (RFinite $ modulo ri, g')
    where (ri, g') =
            randomR (fromIntegral a, fromIntegral b) g

  random = randomR (minBound, maxBound)


chooseAndSwapIndicesToK :: forall v n m m'.
  (MonadInterleave m
  , PrimMonad m'
  , KnownNat n
  , V.MVector v (Finite n)) =>
  Finite (n + 1) -> m (m' (VS.MVector v n (PrimState m') (Finite n)))
chooseAndSwapIndicesToK k = interleave $ do
  (indexSwapPairs :: [(RFinite n, RFinite n)]) <-
    sequence
    . take (fromIntegral k)
    . map (\i -> (,) i <$> getRandomR (i, maxBound))
    $ enumFrom minBound
  return $ do
    (v :: VS.MVector v n (PrimState m') (Finite n)) <- VS.new
    forM_ (enumFrom minBound) (\i -> VS.write v i i)
    forM_ indexSwapPairs (\(RFinite i1, RFinite i2) -> VS.swap v i1 i2)
    return v

indexPermutation :: forall v n m m'.
  (MonadInterleave m
  , PrimMonad m'
  , KnownNat n
  , V.MVector v (Finite n)) => m (m' (VS.MVector v n (PrimState m') (Finite n)))
indexPermutation = chooseAndSwapIndicesToK (maxBound :: Finite (n + 1))


{-
randSwapRight :: forall v n m m' a.
  (MonadRandom m
  , PrimMonad m'
  , V.MVector v a
  , KnownNat n) =>
  VS.MVector v n (PrimState m') a -> Finite n -> m (m' (Finite n))
randSwapRight v i = do
  j <- getRandomR (i, maxBound)
  return $ do
    VS.swap v i j
    return j

-- choose random permutation of K from vector,
-- swap them into the first K elements of the vector (indices 0 .. K-1)
shuffleFirstK :: forall v n m m' a.
  (MonadRandom m
  , PrimMonad m'
  , V.MVector v a
  , KnownNat n) =>
  VS.MVector v n (PrimState m') a -> Finite (n + 1) -> m (m' ())
shuffleFirstK v k = case unshift k of
  Nothing -> return $ return ()
  Just k' -> fmap sequence_ $ forM [minBound..k'] $ randSwapRight v

shuffle :: forall v n m m' a.
  (MonadRandom m
  , PrimMonad m'
  , V.MVector v a
  , KnownNat n) =>
  VS.MVector v n (PrimState m') a -> m (m' ())
shuffle = flip shuffleFirstK maxBound


randChooseGridCoordIndices :: forall g n. (RandomGen g, KnownNat n) =>
  g -> F.Finite (n + 1) -> [F.Finite n]
randChooseGridCoordIndices gen k = runST st
  where
    st :: (forall s. ST s [F.Finite n])
    st = do
      (v :: VGMS.MVector VM.MVector n s (F.Finite n)) <- VGMS.new
      forM_ (enumFrom minBound) (\i -> VGMS.write v i i) --TODO make better with lenses
      evalRand (shuffleFirstK v k) gen
      (vf :: VGS.Vector V.Vector n (F.Finite n)) <- VGS.freeze v
      return $ take (fromIntegral k) $ toList vf

-}
