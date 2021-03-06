module Old.RandomFinite.Finite where

import System.Random (Random(..))
import Data.Finite
import GHC.TypeLits

instance (KnownNat n) => Random (Finite n) where
  randomR (a, b) g = (modulo ri, g')
    where (ri, g') =
            randomR (fromIntegral a, fromIntegral b) g

  random = randomR (minBound, maxBound)
