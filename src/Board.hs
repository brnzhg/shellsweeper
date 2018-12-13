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
import Control.Monad.State (MonadState(..))
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
import Game(GameEndState(..), GameRequest(..), MonadGame(..), MonadBoardGen(..), MonadBoard(..))

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


class HasTileMineCtxt env where
  isMine :: env -> Bool
  isMineAdj :: env -> Bool

instance HasTileMineCtxt SingleMineTile where
  isMine = _isMine
  isMineAdj = (> 0) . (_numAdjMines :: SingleMineTile -> Natural)

instance HasTileMineCtxt MultiMineTile where
  isMine = (> 0) . _numMines
  isMineAdj = (> 0) . (_numAdjMines :: MultiMineTile -> Natural)

{-
class (FR.Representable f) => HasBoardEnv e f where
  numMines :: e f -> Natural
  getAdj :: e f -> FR.Rep f -> [FR.Rep f]
-}

class FR.Representable f => HasRepresentableGetAdj f e | e -> f where
  getAdj :: e -> FR.Rep f -> [FR.Rep f]


instance (FRB.RepresentableB f
         , HasRepresentableGetAdj f e
         , Monoid mrk
         , MonadReader e m
         , HasTileMineCtxt tl
         , MonadState (RepBoardState tl mrk f) m
         , Eq k
         , Hashable k
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

  markBoardTile = undefined


--TODO think if there's something more general here
--foldExperiment :: (Functor f, Foldable f, Monoid b, ComonadStore s w) =>
--  (s -> f s) -> (a -> b) -> w a -> b
--foldExperiment f g = foldMap g . experiment f

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
  (FR.Representable f, HasTileMineCtxt tl, Monoid mrk) =>
  f tl -> f (TileState tl mrk)
startBoardStateFromTileRepresentable =
  fmap makeStartTileState
  where
    makeStartTileState t =
      TileState { _tile = t
                , _isRevealed = False
                , _mark = mempty
                }
    --startTileStates = makeStartTileState <$> g

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
  , Eq (FR.Rep f)
  , Hashable (FR.Rep f)
  , HasTileMineCtxt tl) =>
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
  , Eq (FR.Rep f)
  , Hashable (FR.Rep f)
  , HasTileMineCtxt tl) =>
  RepBoardState tl mrk f
  -> (FR.Rep f -> [FR.Rep f]) -> FR.Rep f -> RepBoardState tl mrk f
revealNonMinesFromIndex bs adj start = bs
  & tileStates %~ flip revealIndices indicesToReveal
  & numRevealed +~ revealCount
  where
    (indicesToReveal, revealCount) =
      id &&& (fromIntegral . length)
      $ dfsUnrevealedNonMines (bs^.tileStates) adj start




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

--TODO goto grid or choose
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
--  bs
--TODO in the env create the board!!
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
