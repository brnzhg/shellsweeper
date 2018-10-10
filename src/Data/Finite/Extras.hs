module Data.Finite.Extras (
packFiniteDefault
) where

import Data.Finite
import Data.Hashable (Hashable(..))
import Data.Maybe (fromMaybe)

import GHC.TypeLits

instance (KnownNat n) => Hashable (Finite n) where
  hashWithSalt i x = hashWithSalt i (fromIntegral x :: Integer)

packFiniteDefault :: (KnownNat n) => Finite n -> Integer -> Finite n
packFiniteDefault = (. packFinite) . fromMaybe



