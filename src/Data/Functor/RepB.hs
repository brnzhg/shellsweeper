{-# LANGUAGE FlexibleInstances
, FlexibleContexts
, ScopedTypeVariables #-}

module Data.Functor.RepB (
  BoardFunctorKey(..)
  , BoardFunctor(..)
  , FoldableBoard(..)
) where

import Data.Proxy
import Data.Functor.Rep
import Data.Hashable (Hashable(..))

import Control.Arrow ((&&&))

import Numeric.Natural

--import GHC.TypeLits
--import qualified Data.Singletons.Prelude as SP
--import qualified Data.Singletons.TypeLits as STL

--import qualified Data.Vector.Generic.Sized as VGS
--import qualified Data.Vector as V
class (Eq k, Hashable k, Bounded k, Enum k) => BoardFunctorKey k where
  domainSize :: Proxy k -> Natural
  domainSize _ = fromIntegral $ length [(minBound :: k)..]

class (Representable f, BoardFunctorKey (Rep f)) => BoardFunctor f where
  update :: f a -> [(Rep f, a)] -> f a
  update fa = updateOver fa id . map fst

  updateOver :: f a -> (a -> a) -> [Rep f] -> f a
  updateOver fa f = update fa . map (id &&& index fa)

newtype FoldableBoard f a = FB { unFB :: f a }

instance BoardFunctor f => Foldable (FoldableBoard f) where
  foldMap f (FB b) = foldMap (f . index b) [minBound..]

{-
instance KnownNat n => BoardFunctor (VGS.Vector V.Vector n) where
  size _ = SP.fromSing nsing
    where nsing :: STL.SNat n
          nsing = SP.sing
  update = (VGS.//)
-}
