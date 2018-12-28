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
  SingleMineTile(..)
  , MultiMineTile(..)
  , HasRepGetAdj(..)
  , HasBoardNumMines(..)
  , HasRepBoardEnv(..)
  , TileState(..)
  , RepBoardState(..)
  , HasTile(..)
  , HasMark(..)
  , startSingleMineRepBoardStateFromMines
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
import Graph (unfoldDfs)
import Game(MonadBoard(..))

data SingleMineTile = SingleMineTile
  { _isMine :: Bool
  , _numAdjMines :: Natural
  }

data MultiMineTile = MultiMineTile
  { _numMines :: Natural
  , _numAdjMines :: Natural
  }

data TileState tl mrk = TileState
  { _tile :: tl
  , _isRevealed :: Bool
  , _mark :: mrk
  }

data RepBoardState tl mrk f = RepBoardState
  { _tileStates :: f (TileState tl mrk)
  , _numRevealed :: Natural
  , _markSum :: mrk
  }

--makeLenses ''HasTile
makeLenses ''TileState
makeLenses ''RepBoardState


class HasTile env where
  isMine :: env -> Bool
  isMineAdj :: env -> Bool
  numMines :: env -> Natural

instance HasTile SingleMineTile where
  isMine = _isMine
  isMineAdj = (> 0) . (_numAdjMines :: SingleMineTile -> Natural)
  numMines t = if _isMine t then 1 else 0

instance HasTile MultiMineTile where
  isMine = (> 0) . _numMines
  isMineAdj = (> 0) . (_numAdjMines :: MultiMineTile -> Natural)
  numMines = _numMines

class (Abelian mrk) => HasMark mrk where
  marksMine :: mrk -> Bool
  numMarkedMines :: mrk -> Natural


class FR.Representable f => HasRepGetAdj f e | e -> f where
  getAdj :: e -> FR.Rep f -> [FR.Rep f]

class HasBoardNumMines e where
  boardNumMines :: e -> Natural

class (HasRepGetAdj f e, HasBoardNumMines e) => HasRepBoardEnv f e

--TODO what to do for thread state?
class (HasTile tl, HasMark mrk, MonadState (RepBoardState tl mrk f) m) =>
      HasRepBoardState tl mrk f m

--TODO add lives
--TODO rename from board to game
instance (FRB.BoardFunctor f
         , HasRepBoardEnv f e
         , MonadReader e m
         , HasRepBoardState tl mrk f m
         , k ~ (FR.Rep f)) =>
  MonadBoard k mrk m where
  revealBoardTile k = do
    rbs <- get
    if marksMine . view mark $ FR.index (rbs^.tileStates) k
      then return (Nothing, [])
      else revealBoardTile' rbs k
    where
      revealBoardTile' rbs k = do
        env <- ask
        let indicesToReveal =
              dfsUnrevealedNonMines (rbs^.tileStates) (getAdj env) k
            newBoardState = revealIndices rbs indicesToReveal
        put newBoardState
        return (getNext env newBoardState, indicesToReveal)
      getNext env newBoardState
        | isMine . view tile
          $ (`FR.index` k)
          $ newBoardState^.tileStates = Just False
        | newBoardState^.numRevealed
          == getRepBoardNumSpaces (Proxy :: Proxy k) env = Just True
        | otherwise = Nothing

  markBoardTile changeMark k = modify $ (\s -> markIndex s changeMark k)

getRepBoardNumSpaces :: (HasBoardNumMines e, FRB.BoardKey k) =>
  Proxy k -> e -> Natural
getRepBoardNumSpaces p env = FRB.domainSize p - boardNumMines env

--TODO fix this to be good over both tile types
singleMineTileFromMineStore :: (Functor f, Foldable f, ComonadStore s w) =>
  (s -> f s) -> w Bool -> SingleMineTile
singleMineTileFromMineStore getAdj mineStore =
  SingleMineTile { _isMine = isMine'
                 , _numAdjMines = numAdjMines'
                 }
  where
    getNumAdjMines =  fromIntegral
                      . getSum
                      . foldMap (\b -> if b then Sum 1 else Sum 0)
                      . experiment getAdj
    (isMine', numAdjMines') = (&&&) extract getNumAdjMines mineStore

singleMineTilesFromMines :: forall f g.
  (Functor f, Foldable f, FR.Representable g) =>
  (FR.Rep g -> f (FR.Rep g)) -> g Bool -> g SingleMineTile
singleMineTilesFromMines getAdj mineRepbl = tileRepbl
  where
    (StoreT (Identity tileRepbl) _) =
      extend (singleMineTileFromMineStore getAdj)
      $ StoreT (Identity mineRepbl) undefined

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

--TODO make better generic over multiMine
--TODO rename so RepBoard is module and dont need to specify Rep
startSingleMineRepBoardStateFromMines :: forall f tl mrk.
  (FR.Representable f, Monoid mrk) =>
  (FR.Rep f -> [FR.Rep f]) -> f Bool -> RepBoardState SingleMineTile mrk f
startSingleMineRepBoardStateFromMines =
  (startRepBoardStateFromTiles .) . singleMineTilesFromMines


dfsUnrevealedNonMines :: forall f tl mrk.
  (FRB.BoardFunctor f
  , HasTile tl
  , HasMark mrk) =>
  f (TileState tl mrk)
  -> (FR.Rep f -> [FR.Rep f])
  -> FR.Rep f -> [FR.Rep f]
dfsUnrevealedNonMines g adj =
  filter (not . marksMine . view mark . FR.index g)
  . unfoldDfs getAdjFiltered
  where
    getAdjFiltered i
      | getAny . foldMap Any
        $ fmap ($ g `FR.index` i)
        [ (^.isRevealed)
        , (isMine . view tile) --(^.tile.isMine)
        , (isMineAdj . view tile)
        , (marksMine . view mark)] = [] --(> 0) . (^.tile.numAdjMines)] = []
      | otherwise =
        filter (not . (isMine . view tile) . FR.index g) . adj $ i

revealIndices :: forall f tl mrk.
  (FRB.BoardFunctor f
  , HasTile tl
  , HasMark mrk) =>
  RepBoardState tl mrk f
  -> [FR.Rep f] -> RepBoardState tl mrk f
revealIndices bs indicesToReveal = bs
  & tileStates %~ revealTileStates
  & numRevealed +~ (fromIntegral $ length indicesToReveal)
  & markSum %~ (<> invert revealMarkSum) --all marks get cleared if revealed
  where
    revealTileStates ts = FRB.updateOver ts (isRevealed .~ True) indicesToReveal
    revealMarkSum = mconcat . map (view mark . FR.index (bs^.tileStates))
                    $ indicesToReveal

{-
    (indicesToReveal, revealCount) =
      id &&& (getSum
              . foldMap Sum
              . map (numMines . view tile . FR.index ts))
      $ dfsUnrevealedNonMines ts adj start
    ts = bs^.tileStates
-}

markIndex :: forall f tl mrk.
  (FRB.BoardFunctor f
  , HasTile tl
  , Abelian mrk) =>
  RepBoardState tl mrk f -> (mrk -> mrk) -> FR.Rep f -> RepBoardState tl mrk f
markIndex bs changeMark i = bs
  & tileStates %~ flip FRB.update [(i, ts & mark.~newMark)]
  & markSum %~ (<> invert currentMark <> newMark)
  where
    newMark = changeMark currentMark
    currentMark = view mark ts
    ts = FR.index (bs^.tileStates) i

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
