{-# LANGUAGE
ScopedTypeVariables
, GeneralizedNewtypeDeriving
, DataKinds
, TypeOperators
, KindSignatures
, UndecidableInstances
, AllowAmbiguousTypes
#-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Grid (
Grid(..)
, GridCoord(..)
, GridIndex(..)
, gridIndexCoord
, squareAdjacentCoords
, gridToVecOfVec
, gridToListOfList
) where

import Data.List (mapAccumL, splitAt)
import Data.Foldable (toList)
import Data.Finite (Finite, getFinite, modulo, combineProduct)
import Data.Bifunctor (bimap)

import Control.Monad (replicateM)
import Control.Monad.State.Strict (State(..), get, state, evalState)

import qualified Data.Vector as V
import qualified Data.Vector.Sized as VS

import GHC.TypeLits
import qualified Data.Singletons.Prelude as SP
import qualified Data.Singletons.TypeLits as STL

import qualified Control.Lens as L
import qualified Control.Lens.Iso as LI
--import qualified Control.Lens.Traversal as LT

--import ChooseFinite (chooseAndSwapIndicesToK, indexPermutation)
import Data.Finite.Extras (packFiniteDefault)


type Grid (n :: Nat) (n' :: Nat) = VS.Vector (n * n')
--  deriving (Functor, FR.Representable)

type GridCoord (n :: Nat) (n' :: Nat) = (Finite n, Finite n')
type GridIndex (n :: Nat) (n' :: Nat) = Finite (n * n')

gridIndexCoord :: forall n n'. (KnownNat n, KnownNat n') =>
  LI.Iso' (GridIndex n n') (GridCoord n n')
gridIndexCoord = LI.iso getCoord combineProduct
  where nsing' :: STL.SNat n'
        nsing' = SP.sing
        getCoord :: Finite (n * n') -> GridCoord n n'
        getCoord = bimap (modulo . fromIntegral) (modulo . fromIntegral)
                   . (`divMod` (fromIntegral . SP.fromSing $ nsing'))
                   . getFinite


squareAdjacentCoords :: forall n n'. (KnownNat n, KnownNat n') =>
  GridCoord n n' -> [GridCoord n n']
squareAdjacentCoords (r, c) = [(r', c')
                              | r' <- getAdjOneD r,
                                c' <- getAdjOneD c,
                                (r', c') /= (r, c)]
  where getAdjOneD x = [packFiniteDefault minBound (fromIntegral x - 1)
                        ..
                        packFiniteDefault maxBound (fromIntegral x + 1)]

gridToVecOfVec :: forall n n' a. (KnownNat n, KnownNat n') =>
  Grid n n' a -> VS.Vector n (VS.Vector n' a)
gridToVecOfVec gr =
  VS.generate (\i ->
                  VS.generate (\j ->
                                  VS.index gr
                                  $ L.view (LI.from gridIndexCoord) (i, j)))

gridToListOfList :: forall n n' a. (KnownNat n, KnownNat n') => Grid n n' a -> [[a]]
gridToListOfList = evalState
                   (replicateM (fromIntegral (maxBound :: Finite n)) m)
                   . toList
  where
    m = state $ splitAt $ fromIntegral (maxBound :: Finite n')


{-
(//) :: forall n n' a. (KnownNat n, KnownNat n') =>
  Grid n n' a -> [(Finite n, [(Finite n', a)])] -> Grid n n' a
(//) (Compose gr) updates =
  Compose $ gr VS.// updates'
  where updates' = flip map updates
                   (\(rowIdx, updatesForRow) ->
                      (rowIdx, gr `VS.index` rowIdx VS.// updatesForRow))
-}
