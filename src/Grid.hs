{-# LANGUAGE
ScopedTypeVariables
, DataKinds
, TypeOperators
, KindSignatures
#-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Grid (
Grid(..)
, GridCoord(..)
, GridIndex(..)
, gridCoordIndex
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

--import ChooseFinite (chooseAndSwapIndicesToK, indexPermutation)
import Data.Finite.Extras (packFiniteDefault)


newtype Grid (n :: Nat) (n' :: Nat) a = Grid (VS.Vector (n * n') a)

type GridCoord (n :: Nat) (n' :: Nat) = (Finite n, Finite n')
type GridIndex (n :: Nat) (n' :: Nat) = Finite (n * n')

gridCoordIndex :: forall n n'. (KnownNat n, KnownNat n') =>
  LI.Iso' (GridCoord n n') (GridIndex n n')
gridCoordIndex = LI.iso combineProduct getCoord
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
gridToVecOfVec (Grid gr) =
  VS.generate (\i ->
                 VS.generate (\j ->
                                VS.index gr
                                $ L.view gridCoordIndex (i, j)))

gridToListOfList :: forall n n' a. (KnownNat n, KnownNat n') => Grid n n' a -> [[a]]
gridToListOfList (Grid gr) = evalState
                             . replicateM (fromIntegral (maxBound :: Finite n)) m
                             $ toList gr
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
