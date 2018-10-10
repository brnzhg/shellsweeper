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
) where

import Data.Functor.Compose
import Data.Finite (Finite, getFinite, modulo, combineProduct)
import Data.Bifunctor (bimap)

import qualified Data.Vector as V
import qualified Data.Vector.Generic.Sized as VS

import GHC.TypeLits
import qualified Data.Singletons.Prelude as SP
import qualified Data.Singletons.TypeLits as STL

import qualified Control.Lens as L
import qualified Control.Lens.Iso as LI

import ChooseFinite (chooseAndSwapIndicesToK, indexPermutation)
import Data.Finite.Extras (packFiniteDefault)


type Grid (n :: Nat) (n' :: Nat) = VS.Vector V.Vector (n * n')

type GridCoord (n :: Nat) (n' :: Nat) = (Finite n, Finite n')
type GridIndex (n :: Nat) (n' :: Nat) = Finite (n * n')

gridCoordIndex :: forall n n'. (KnownNat n, KnownNat n') =>
  LI.Iso' (GridCoord n n') (GridIndex n n')
gridCoordIndex = LI.iso combineProduct getCoord
  where msing :: STL.SNat n'
        msing = SP.sing
        getCoord :: Finite (n * n') -> GridCoord n n'
        getCoord = bimap (modulo . fromIntegral) (modulo . fromIntegral)
                   . (`divMod` (fromIntegral . SP.fromSing $ msing))
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

--TODO grid to vec of vec
--TODO grid to list of list


{-
(//) :: forall n n' a. (KnownNat n, KnownNat n') =>
  Grid n n' a -> [(Finite n, [(Finite n', a)])] -> Grid n n' a
(//) (Compose gr) updates =
  Compose $ gr VS.// updates'
  where updates' = flip map updates
                   (\(rowIdx, updatesForRow) ->
                      (rowIdx, gr `VS.index` rowIdx VS.// updatesForRow))
-}
