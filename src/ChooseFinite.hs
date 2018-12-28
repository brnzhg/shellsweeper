{-# LANGUAGE
ScopedTypeVariables
, GeneralizedNewtypeDeriving
, FlexibleContexts
, DataKinds
, TypeOperators
#-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module ChooseFinite (
RFinite(..)
, indexSwapPairsChooseK
, indexSwapPairsPermuteAll
, vectorSwapPairs
) where

import System.Random (Random(..))
import Data.Finite
import GHC.List (take)
import GHC.TypeLits

import Data.Traversable
import Control.Monad (forM_)
import Control.Monad.Random (MonadInterleave, getRandomR, interleave)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (ST, runST)

--import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Sized as VS
--import qualified Data.Vector.Mutable.Sized as VMS
import qualified Data.Vector.Generic.Mutable.Base as  VGMB
import qualified Data.Vector.Generic.Mutable.Sized as VGMS

newtype RFinite a = RFinite { getFin :: Finite a }
  deriving (Eq, Ord, Num, Real, Integral, Enum, Bounded)

instance (KnownNat n) => Random (RFinite n) where
  randomR (a, b) g = (RFinite $ modulo ri, g')
    where (ri, g') =
            randomR (fromIntegral a, fromIntegral b) g

  random = randomR (minBound, maxBound)

indexSwapPairsChooseK :: forall n m.
  (MonadInterleave m
  , KnownNat n) =>
  Finite (n + 1) -> m [(Finite n, Finite n)]
indexSwapPairsChooseK k = sequence
  . take (fromIntegral k)
  . map indexToSwapPair
  $ enumFrom minBound
  where
    indexToSwapPair ri@(RFinite i) =
      ((,) i . getFin) <$> (getRandomR (ri, maxBound))

indexSwapPairsPermuteAll :: forall n m.
  (MonadInterleave m
  , KnownNat n) => m [(Finite n, Finite n)]
indexSwapPairsPermuteAll =
  indexSwapPairsChooseK (maxBound :: Finite (n + 1))

vectorSwapPairs :: forall v n m a.
  (KnownNat n, PrimMonad m, VGMB.MVector v a) =>
  VGMS.MVector v n (PrimState m) a
  -> [(Finite n, Finite n)]
  -> m ()
vectorSwapPairs v indexSwapPairs =
  forM_ indexSwapPairs $ uncurry $ VGMS.swap v


{-
vectorFromIndexSwapPairsChooseK :: forall n.
  KnownNat n =>
  Finite (n + 1)
  -> [(Finite n, Finite n)]
  -> VS.Vector n Bool
vectorFromIndexSwapPairsChooseK k indexPairs =
  runST st
  where
    st :: (forall s. ST s (VS.Vector n Bool))
    st = do
      v :: VMS.MVector n s Bool <- VMS.new
      forM_ (take (fromIntegral k) $ enumFrom minBound)
        (\i -> VMS.write v i True)
      forM_ indexPairs $ uncurry $ VMS.swap v
      VS.freeze v
      --return v'
-}

