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
, swapIndexPairs
, indexPairsChooseK
, indexPairsPermuteAll
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


newtype RFinite a = RFinite { getFin :: Finite a }
  deriving (Eq, Ord, Num, Real, Integral, Enum, Bounded)

instance (KnownNat n) => Random (RFinite n) where
  randomR (a, b) g = (RFinite $ modulo ri, g')
    where (ri, g') =
            randomR (fromIntegral a, fromIntegral b) g

  random = randomR (minBound, maxBound)


swapIndexPairs :: forall v n m a.
  (PrimMonad m
  , KnownNat n
  , V.MVector v a) =>
  [(Finite n, Finite n)]
  -> VS.MVector v n (PrimState m) a
  -> m ()
swapIndexPairs = (. uncurry . VS.swap) . forM_

indexPairsChooseK :: forall n m.
  (MonadInterleave m
  , KnownNat n) =>
  Finite (n + 1) -> m [(Finite n, Finite n)]
indexPairsChooseK k = sequence
  . take (fromIntegral k)
  . map indexToSwapPair
  $ enumFrom minBound
  where
    indexToSwapPair ri@(RFinite i) =
      ((,) i . getFin) <$> (getRandomR (ri, maxBound))

indexPairsPermuteAll :: forall n m.
  (MonadInterleave m
  , KnownNat n) => m [(Finite n, Finite n)]
indexPairsPermuteAll = indexPairsChooseK (maxBound :: Finite (n + 1))
