{-# LANGUAGE
ScopedTypeVariables
, DataKinds
, TypeOperators
, KindSignatures
, RankNTypes
, UndecidableInstances
, AllowAmbiguousTypes
, MultiParamTypeClasses
, TypeFamilies
, TemplateHaskell
, FlexibleContexts
, FlexibleInstances
, FunctionalDependencies
, DuplicateRecordFields
#-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Board (
 ) where

import Data.Proxy
import Data.Monoid (Sum(..))
import Data.Group (Group(..), Abelian(..))

import Data.Finite (Finite)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Rep as FR
import Data.Monoid (Any(..))
import Data.Traversable
import Data.Hashable(Hashable(..))
--import Control.Monad (forM_)

import Control.Monad.Reader (ReaderT(..), MonadReader(..))
import Control.Monad.State (MonadState(..), modify)
import Control.Comonad (Comonad(..))
import Control.Comonad.Representable.Store
  (Store, StoreT(..), ComonadStore, store, experiment, runStore)
import Control.Arrow ((&&&))

import Numeric.Natural

import GHC.TypeLits

import Control.Lens
import qualified Control.Lens.Iso as LI
import qualified Control.Lens.Traversal as LT

import qualified Data.Functor.RepB as FRB

{-
startRepBoardStateFromTiles :: forall f tl mrk.
  (FR.Representable f, HasTile tl, Monoid mrk) =>
  f tl -> RepBoardState tl mrk f
startRepBoardStateFromTiles tiles =
  RepBoardState { _tileStates = makeStartTileState <$> tiles , _numRevealed = 0
                , _markSum = mempty
                }
  where
    makeStartTileState t =
      TileState { _tile = t
                , _isRevealed = False
                , _mark = mempty
                }
-}
{-
    (indicesToReveal, revealCount) =
      id &&& (getSum
              . foldMap Sum
              . map (numMines . view tile . FR.index ts))
      $ dfsUnrevealedNonMines ts adj start
    ts = bs^.tileStates
-}
{-
startBoardStateFromCoordPairs :: forall n n' e.
  (KnownNat n, KnownNat n', HasBoardEnv e n n') =>
  e n n'-> [(GridCoord n n', GridCoord n n')] -> BoardState n n'
startBoardStateFromCoordPairs boardEnv =
  startBoardStateFromTileGrid
  . Grid
  . tileRepresentableFromMines getAdj'
  . mineVectorFromIndexPairs (numMines boardEnv)
  . over (mapped . both) (view gridCoordIndex)
  where
    getAdj' = LT.traverseOf gridIndexCoord . getAdj $ boardEnv
-}

--make gameenv more modular like the font
{-
revealCoords :: forall n n'. (KnownNat n, KnownNat n') =>
  Grid n n' BoardTileState -> [GridCoord n n'] -> Grid n n' BoardTileState
revealCoords gr coords = Grid
  $ getVector gr VS.// (zip gridIndices tilesAtCoords')
  where
     tilesAtCoords' = (isRevealed .~ True) . FR.index gr <$> coords
     gridIndices = (mapped %~ view gridCoordIndex) coords
-}

{-
-x---x---x---
/2\*/2\1/1\1/
---x---x---x-
\2/*\2/1\*/1\
-x---x---x---x
-}
