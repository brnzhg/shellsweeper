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
import qualified GHC.List
import GHC.TypeLits

import Data.Traversable
import Control.Monad (forM_)
import Control.Monad.Random (MonadInterleave, getRandomR, interleave)
import Control.Monad.Primitive (PrimMonad, PrimState)

import Data.Vector.Generic.Mutable.Base as V
import Data.Vector.Generic.Mutable.Sized as VS


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
    . GHC.List.take (fromIntegral k)
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

