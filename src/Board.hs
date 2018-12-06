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
#-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Board (
  BoardTile(..)
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
import Control.Comonad (Comonad(..))
import Control.Comonad.Representable.Store
  (Store, StoreT(..), ComonadStore, store, experiment, runStore)
import Control.Arrow ((&&&))

import Numeric.Natural

--import qualified Data.Vector as V
--import qualified Data.Vector.Mutable as VM
--import qualified Data.Vector.Sized as VS
--import qualified Data.Vector.Mutable.Sized as VMS

import GHC.TypeLits
--import qualified Data.Singletons.Prelude as SP
--import qualified Data.Singletons.TypeLits as STL

--import Control.Lens ((.~))
--import qualified Control.Lens as L
import Control.Lens
import qualified Control.Lens.Iso as LI
import qualified Control.Lens.Traversal as LT

--import ChooseFinite (indexSwapPairsChooseK)
--import Grid (Grid(..), GridCoord(..), GridIndex(..)
--            , gridIndexCoord, gridCoordIndex)
import qualified Data.Functor.RepB as FRB
import Graph (unfoldDfs)

data BoardTile = BoardTile
  { _isMine :: Bool
  , _numAdjMines :: Natural
  } --TODO need lenses

data BoardTileState = BoardTileState
  { _tile :: BoardTile
  , _isRevealed :: Bool
  , _isMarked :: Bool
  }
--this could be thing in State Monad
--add numAdjMineMarked

data BoardState f = BoardState
  { _tileStates :: f BoardTileState
  , _numRevealed :: Natural
  , _numMarked :: Natural
  }

{-
class (FR.Representable f) => HasBoardEnv e f where
  numMines :: e f -> Natural
  getAdj :: e f -> FR.Rep f -> [FR.Rep f]
-}

makeLenses ''BoardTile
makeLenses ''BoardTileState
makeLenses ''BoardState


--TODO think if there's something more general here
--foldExperiment :: (Functor f, Foldable f, Monoid b, ComonadStore s w) =>
--  (s -> f s) -> (a -> b) -> w a -> b
--foldExperiment f g = foldMap g . experiment f

tileFromMineStore :: (Functor f, Foldable f, ComonadStore s w) =>
  (s -> f s) -> w Bool -> BoardTile
tileFromMineStore getAdj mineStore =
  BoardTile { _isMine = isMine'
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
  (FR.Rep g -> f (FR.Rep g)) -> g Bool -> g BoardTile
tileRepresentableFromMines getAdj mineRepbl = tileRepbl
  where
    (StoreT (Identity tileRepbl) _) =
      extend (tileFromMineStore getAdj)
      $ StoreT (Identity mineRepbl) undefined

startBoardStateFromTileRepresentable :: forall f.
  FR.Representable f => f BoardTile -> BoardState f
startBoardStateFromTileRepresentable g =
  BoardState { _tileStates = startTileStates
             , _numRevealed = 0
             , _numMarked = 0
             }
  where
    makeStartTileState t =
      BoardTileState { _tile = t
                     , _isRevealed = False
                     , _isMarked = False
                     }
    startTileStates = makeStartTileState <$> g

startBoardStateFromMineRepresentable :: forall f e.
  FR.Representable f => (FR.Rep f -> [FR.Rep f]) -> f Bool -> BoardState f
startBoardStateFromMineRepresentable =
  (startBoardStateFromTileRepresentable .) . tileRepresentableFromMines

dfsUnrevealedNonMines :: forall f.
  (Representable f, Eq (FR.Rep f), Hashable (FR.Rep f)) =>
  f BoardTileState
  -> (FR.Rep f -> [FR.Rep f])
  -> FR.Rep f -> [FR.Rep f]
dfsUnrevealedNonMines g adj = unfoldDfs getAdjFiltered
  where
    getAdjFiltered i
      | getAny . foldMap Any
        $ fmap ($ g `FR.index` i)
        [ (^.isRevealed)
        , (^.tile.isMine)
        , (> 0) . (^.tile.numAdjMines)] = []
      | otherwise =
        filter (not . (^.tile.isMine) . FR.index g) . adj $ i

revealIndices :: forall f. FRB.RepresentableB f =>
  f BoardTileState -> [FR.Rep f] -> f BoardTileState
revealIndices = flip FRB.updateOver (isRevealed .~ True)

revealNonMinesFromIndex :: forall f.
  (FRB.RepresentableB f, Eq (FR.Rep f), Hashable (FR.Rep f)) =>
  BoardState f -> (FR.Rep f -> [FR.Rep f]) -> FR.Rep f -> BoardState f
revealNonMinesFromIndex bs adj start = bs
  & tileStates %~ flip revealIndices indicesToReveal
  & numRevealed +~ revealCount
  where
    (indicesToReveal, revealCount) =
      id &&& (fromIntegral . length)
      $ dfsUnrevealedNonMines (bs ^. tileStates) adj start
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
