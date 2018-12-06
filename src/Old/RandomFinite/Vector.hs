{-# LANGUAGE
ScopedTypeVariables
, FlexibleContexts
, DataKinds
, TypeOperators
#-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- random operations on sized vectors
module RandomFinite.Vector
  ( randSwapRight
  , shuffleFirstK
  )
where

import Data.Traversable
import Control.Monad.Random (MonadRandom, getRandomR)
import Control.Monad.Primitive (PrimMonad, PrimState)

import Data.Finite (Finite, unshift)
import Data.Vector.Generic.Mutable.Base as V
import Data.Vector.Generic.Mutable.Sized as VS

import GHC.TypeLits

import RandomFinite.Finite


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
--TODO could be more clever here? somehow traverse both monads at once?
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
