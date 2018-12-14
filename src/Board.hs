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
  ) where

import Data.Monoid (Sum(..))
import Data.Group (Group(..), Abelian(..))

import Data.Finite (Finite)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Rep as FR
import Data.Monoid (Any(..))
import Data.Traversable
import Data.Hashable(Hashable(..))
--import Control.Monad (forM_)

--import Control.Monad.ST (ST, runST)
--import Control.Monad.Random (MonadInterleave, interleave)
--import Control.Monad.Primitive (PrimMonad, PrimState)
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
import Game(MonadBoardGen(..), MonadBoard(..))

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
  , _marks :: mrk
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

instance HasTile tl => HasTile (TileState tl mrk) where
  isMine = isMine . view tile
  isMineAdj = isMineAdj . view tile
  numMines = numMines . view tile


class (Eq k, Hashable k) => RepBoardKey k

class FR.Representable f => HasRepGetAdj f e | e -> f where
  getAdj :: e -> FR.Rep f -> [FR.Rep f]

--TODO what to do for thread state?
class (HasTile tl, Abelian mrk, MonadState (RepBoardState tl mrk f) m) =>
      HasRepBoardState tl mrk f m

instance (FRB.RepresentableB f
         , HasRepGetAdj f e
         , MonadReader e m
         , HasRepBoardState tl mrk f m
         , RepBoardKey k
         , k ~ (FR.Rep f)) =>
  MonadBoard k mrk m where
  revealBoardTile k = do
    rbs <- get
    if isMine . view tile $ (`FR.index` k) $ rbs^.tileStates
      then return False
      else (do
               env <- ask
               put $ revealNonMinesFromIndex rbs (getAdj env) k
               return True)

  markBoardTile changeMark k = modify $ (\s -> markIndex s changeMark k)


--TODO fix this to be good over both tile types
tileFromMineStore :: (Functor f, Foldable f, ComonadStore s w) =>
  (s -> f s) -> w Bool -> SingleMineTile
tileFromMineStore getAdj mineStore =
  SingleMineTile { _isMine = isMine'
                 , _numAdjMines = numAdjMines'
                 }
  where
    getNumAdjMines =  fromIntegral
                      . getSum
                      . foldMap (\b -> if b then Sum 1 else Sum 0)
                      . experiment getAdj
    (isMine', numAdjMines') = (&&&) extract getNumAdjMines mineStore

tileRepresentableFromMines :: forall f g.
  (Functor f, Foldable f, FR.Representable g) =>
  (FR.Rep g -> f (FR.Rep g)) -> g Bool -> g SingleMineTile
tileRepresentableFromMines getAdj mineRepbl = tileRepbl
  where
    (StoreT (Identity tileRepbl) _) =
      extend (tileFromMineStore getAdj)
      $ StoreT (Identity mineRepbl) undefined

startBoardStateFromTileRepresentable :: forall f tl mrk.
  (FR.Representable f, HasTile tl, Group mrk) =>
  f tl -> f (TileState tl mrk)
startBoardStateFromTileRepresentable =
  fmap makeStartTileState
  where
    makeStartTileState t =
      TileState { _tile = t
                , _isRevealed = False
                , _mark = mempty
                }

{-
startBoardStateFromMineRepresentable :: forall f e.
  FR.Representable f => (FR.Rep f -> [FR.Rep f]) -> f Bool -> BoardState f
startBoardStateFromMineRepresentable =
  (startBoardStateFromTileRepresentable .) . tileRepresentableFromMines
-}

--TODO change this to be part of typeclass or environment
--this way we can get rid of the minecontxt typeclass
dfsUnrevealedNonMines :: forall f tl mrk.
  (Representable f
  , RepBoardKey (FR.Rep f)
  , HasTile tl) =>
  f (TileState tl mrk)
  -> (FR.Rep f -> [FR.Rep f])
  -> FR.Rep f -> [FR.Rep f]
dfsUnrevealedNonMines g adj = unfoldDfs getAdjFiltered
  where
    getAdjFiltered i
      | getAny . foldMap Any
        $ fmap ($ g `FR.index` i)
        [ (^.isRevealed)
        , (isMine . view tile) --(^.tile.isMine)
        , (isMineAdj . view tile)] = [] --(> 0) . (^.tile.numAdjMines)] = []
      | otherwise =
        filter (not . (isMine . view tile) . FR.index g) . adj $ i

revealIndices :: forall f tl mrk. FRB.RepresentableB f =>
  f (TileState tl mrk) -> [FR.Rep f] -> f (TileState tl mrk)
revealIndices = flip FRB.updateOver (isRevealed .~ True)

revealNonMinesFromIndex :: forall f tl mrk.
  (FRB.RepresentableB f
  , RepBoardKey (FR.Rep f)
  , HasTile tl) =>
  RepBoardState tl mrk f
  -> (FR.Rep f -> [FR.Rep f]) -> FR.Rep f -> RepBoardState tl mrk f
revealNonMinesFromIndex bs adj start = bs
  & tileStates %~ flip revealIndices indicesToReveal
  & numRevealed +~ revealCount
  where
    (indicesToReveal, revealCount) =
      id &&& (getSum
              . foldMap Sum
              . map (numMines . view tile . FR.index ts))
      $ dfsUnrevealedNonMines ts adj start
    ts = bs^.tileStates


markIndex :: forall f tl mrk.
  (FRB.RepresentableB f
  , RepBoardKey (FR.Rep f)
  , HasTile tl
  , Abelian mrk) =>
  RepBoardState tl mrk f -> (mrk -> mrk) -> FR.Rep f -> RepBoardState tl mrk f
markIndex bs changeMark i = bs
  & tileStates %~ flip FRB.update [(i, ts & mark.~newMark)]
  & marks %~ (<> invert currentMark <> newMark)
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
randomStartBoardState :: forall n n' e m.
  (KnownNat n, KnownNat n', HasBoardEnv e n n', MonadInterleave m) =>
  e n n'-> m (BoardState n n')
randomStartBoardState boardEnv =
  startBoardStateFromCoordPairs boardEnv
  . over (mapped . both) (view gridIndexCoord)
  <$> (indexPairsChooseK $ numMines boardEnv)
-}
{-
revealCoords :: forall n n'. (KnownNat n, KnownNat n') =>
  Grid n n' BoardTileState -> [GridCoord n n'] -> Grid n n' BoardTileState
revealCoords gr coords = Grid
  $ getVector gr VS.// (zip gridIndices tilesAtCoords')
  where
     tilesAtCoords' = (isRevealed .~ True) . FR.index gr <$> coords
     gridIndices = (mapped %~ view gridCoordIndex) coords

revealNonMinesFromCoord :: forall n n'. (KnownNat n, KnownNat n') =>
  BoardState n n'
  -> (GridCoord n n' -> [GridCoord n n'])
  -> GridCoord n n'
  -> BoardState n n'
revealNonMinesFromCoord bs f coord = undefined
  where
    gr' = revealCoords tg . dfsUnrevealedNonMines tg f
    tg = bs ^. tileGrid
-}

{-
   _________
  /        /|
 /________/ |
 |        | |
 |        | |
 |        | /
 |________|/

 / \ / \ /
| 1 | * |
 \ / \ / \
  | 3 | * |
 / \ / \ /
| * | 1 |

-x---x---x---
/2\*/2\1/1\1/
---x---x---x-
\2/*\2/1\*/1\
-x---x---x---x
-}
