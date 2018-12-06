{-# LANGUAGE FlexibleInstances #-}

module Data.Functor.RepB (
  RepresentableB(..)
) where

import Data.Functor.Rep

import GHC.TypeLits

import qualified Data.Vector.Generic.Sized as VGS
import qualified Data.Vector as V
import Control.Arrow ((&&&))

class Representable f => RepresentableB f where
  update :: f a -> [(Rep f, a)] -> f a
  update fa = updateOver fa id . map fst

  updateOver :: f a -> (a -> a) -> [Rep f] -> f a
  updateOver fa f = update fa . map (id &&& index fa)

instance KnownNat n => RepresentableB (VGS.Vector V.Vector n) where
  update = (VGS.//)
