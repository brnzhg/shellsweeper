{-# LANGUAGE
ScopedTypeVariables
, GeneralizedNewtypeDeriving
, DataKinds
, TypeOperators
, KindSignatures
, UndecidableInstances
, AllowAmbiguousTypes
, GeneralizedNewtypeDeriving
, TypeFamilies
#-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Grid (
Grid(..)
, GridCoord(..)
, GridIndex(..)
, gridIndexCoord
, gridCoordIndex
, squareAdjacentCoords
, gridToVecOfVec
, gridToListOfList
) where

import Data.Proxy
import Data.List (mapAccumL, splitAt)
import Data.Foldable (toList)
import Data.Finite (Finite, getFinite, modulo)
import Data.Bifunctor (bimap)
import Data.Functor.Rep as FR
import Data.Functor.RepB as FRB
import Data.Distributive as FD

import Control.Monad (replicateM)
import Control.Monad.State.Strict (State(..), get, state, evalState)
import Control.Monad.Random (MonadInterleave, interleave)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Arrow ((&&&))

import Data.Hashable (Hashable(..))
import qualified Data.Vector as V
import qualified Data.Vector.Sized as VS

import GHC.TypeLits
import qualified Data.Singletons.Prelude as SP
import qualified Data.Singletons.TypeLits as STL

import qualified Control.Lens as L
import qualified Control.Lens.Iso as LI
import qualified Control.Lens.Traversal as LT

import Data.Functor.RepB (BoardKey(..), BoardFunctor(..))
import Data.Finite.Extras (packFiniteDefault)
import ChooseFinite (indexSwapPairsChooseK
                    , vectorFromIndexSwapPairsChooseK)
import Board (SingleMineTile(..)
             , MultiMineTile(..)
             , RepBoardState(..)
             , startSingleMineRepBoardStateFromMines)


newtype Grid (n :: Nat) (n' :: Nat) a =
  Grid { getVector :: VS.Vector (n * n') a }
  deriving (Functor, Applicative, Monad, Foldable)

--TODO use deriving via to get other gridcoords possibly
newtype GridCoord (n :: Nat) (n' :: Nat) = GridCoord { unCoord :: (Finite n, Finite n')}
  deriving (Eq, Show)
type GridIndex (n :: Nat) (n' :: Nat) = Finite (n * n')

gridIndexCoord :: forall n n'. (KnownNat n, KnownNat n') =>
  LI.Iso' (GridIndex n n') (GridCoord n n')
gridIndexCoord = LI.iso getCoord getIndex
  where n'int :: Integer
        n'int = fromIntegral . SP.FromSing $ (SP.sing :: STL.SNat n')
        getIndex :: GridCoord n n' -> Finite (n * n')
        getIndex (GridCoord (fn, fn')) =
          modulo . fromIntegral $ (getFinite fn) * n'int + (getFinite fn')
        getCoord :: Finite (n * n') -> GridCoord n n'
        getCoord = GridCoord
                   . bimap (modulo . fromIntegral) (modulo . fromIntegral)
                   . (`divMod` n'int)
                   . getFinite

gridCoordIndex :: forall n n'. (KnownNat n, KnownNat n') =>
  LI.Iso' (GridCoord n n') (GridIndex n n')
gridCoordIndex = LI.from gridIndexCoord

instance (KnownNat n, KnownNat n') => Hashable (GridCoord n n') where
  hashWithSalt i = hashWithSalt i . L.view gridCoordIndex

instance (KnownNat n, KnownNat n') => Bounded (GridCoord n n') where
  minBound = L.view gridIndexCoord minBound
  maxBound = L.view gridIndexCoord maxBound

instance (KnownNat n, KnownNat n') => Enum (GridCoord n n') where
  toEnum = L.view gridIndexCoord . toEnum
  fromEnum = fromEnum . L.view gridCoordIndex

instance (KnownNat n, KnownNat n') => BoardKey (GridCoord n n') where
  domainSize _ = SP.fromSing nsing * SP.fromSing nsing'
    where
      nsing :: STL.SNat n
      nsing = SP.sing
      nsing' :: STL.SNat n'
      nsing' = SP.sing

instance (KnownNat n, KnownNat n') => FD.Distributive (Grid n n') where
  distribute = FR.distributeRep

instance (KnownNat n, KnownNat n') => FR.Representable (Grid n n') where
  type Rep (Grid n n') = GridCoord n n'
  index (Grid v) = VS.index v . L.view gridCoordIndex
  tabulate f = Grid $ tabulate (f . L.view gridIndexCoord)


--TODO use wrap unwrap lenses for this grid to vec stuff
instance (KnownNat n, KnownNat n') => FRB.BoardFunctor (Grid n n') where
  update (Grid v) = Grid
                    . (v VS.//)
                    . (L.mapped . L._1 L.%~ L.view (gridCoordIndex))


squareAdjacentCoords :: forall n n'. (KnownNat n, KnownNat n') =>
  GridCoord n n' -> [GridCoord n n']
squareAdjacentCoords (GridCoord (r, c)) =
  [GridCoord (r', c')
  | r' <- getAdjOneD r,
   c' <- getAdjOneD c,
    (r', c') /= (r, c)]
  where getAdjOneD x = [packFiniteDefault minBound (fromIntegral x - 1)
                        ..
                        packFiniteDefault maxBound (fromIntegral x + 1)]

--TODO rename when everything has good namespace
--TODO consider using MonadReader for nummines and adj
startSingleMineBoardFromSwapPairs :: (KnownNat n, KnownNat n', Monoid mrk) =>
  Finite (n * n' + 1)
  -> (GridCoord n n' -> [GridCoord n n'])
  -> [(GridIndex n n', GridIndex n n')]
  -> RepBoardState SingleMineTile mrk (Grid n n')
startSingleMineBoardFromSwapPairs mines adj =
  startSingleMineRepBoardStateFromMines adj
  . Grid
  . vectorFromIndexSwapPairsChooseK mines
-- . L.over (L.mapped . L.both) (L.view gridCoordIndex)

randomStartSingleMineBoard ::
  (KnownNat n, KnownNat n', Monoid mrk, MonadInterleave m) =>
  Finite (n * n' + 1)
  -> (GridCoord n n' -> [GridCoord n n'])
  -> m (RepBoardState SingleMineTile mrk (Grid n n'))
randomStartSingleMineBoard mines adj =
  startSingleMineBoardFromSwapPairs mines adj
  <$> (indexSwapPairsChooseK mines)


gridToVecOfVec :: forall n n' a. (KnownNat n, KnownNat n') =>
  Grid n n' a -> VS.Vector n (VS.Vector n' a)
gridToVecOfVec gr =
  VS.generate (\i ->
                  VS.generate (\j ->
                                  index gr $ GridCoord (i, j)))

gridToListOfList :: forall n n' a. (KnownNat n, KnownNat n') =>
  Grid n n' a -> [[a]]
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
