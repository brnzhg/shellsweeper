module Data.Functor.RepB (
  RepresentableB(..)
) where

import Data.Functor.Rep

class Representable f => RepresentableB f where
  update :: f a -> [(Rep f, a)] -> f a
  update fa = updateOver fa id . map fst

  updateOver :: f a -> (a -> a) -> [Rep f] -> f a
  updateOver fa f = update fa . map (\x -> (x, fa `index` x))
