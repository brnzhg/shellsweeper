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
, indexPairsChooseK
, indexPairsPermuteAll
) where

import System.Random (Random(..))
import Data.Finite
import GHC.List (take)
import GHC.TypeLits

import Data.Traversable
import Control.Monad.Random (MonadInterleave, getRandomR, interleave)


newtype RFinite a = RFinite { getFin :: Finite a }
  deriving (Eq, Ord, Num, Real, Integral, Enum, Bounded)

instance (KnownNat n) => Random (RFinite n) where
  randomR (a, b) g = (RFinite $ modulo ri, g')
    where (ri, g') =
            randomR (fromIntegral a, fromIntegral b) g

  random = randomR (minBound, maxBound)

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
