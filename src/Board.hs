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
#-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Board (
  BoardTile(..)
  , tileVectorFromMines
  , mineVectorFromIndexPairs
  ) where

import Data.Monoid (Sum(..))

import Data.Finite (Finite)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Rep as FR
import Data.Monoid (Any(..))
import Data.Traversable
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Control.Monad.Random (MonadInterleave, interleave)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Comonad (Comonad(..))
import Control.Comonad.Representable.Store
  (Store, StoreT(..), ComonadStore, store, experiment, runStore)
import Control.Arrow ((&&&))

import Numeric.Natural

--import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Sized as VS
import qualified Data.Vector.Mutable.Sized as VMS


import GHC.TypeLits
--import qualified Data.Singletons.Prelude as SP
--import qualified Data.Singletons.TypeLits as STL

import qualified Control.Lens as L
import qualified Control.Lens.Iso as LI
import qualified Control.Lens.Traversal as LT

import ChooseFinite (indexPairsChooseK)
import Grid (Grid(..), GridCoord(..), GridIndex(..), gridIndexCoord)
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

data BoardState (n :: Nat) (n' :: Nat) = BoardState
  { _tileGrid :: Grid n n' BoardTileState
  , _numRevealed :: Finite (n * n' + 1)
  , _numMarked :: Finite (n * n' + 1)
  }


class HasBoardEnv e (n :: Nat) (n' :: Nat) where
  numMines :: e n n' -> Finite ((n * n') + 1)
  getAdj :: e n n' -> GridCoord n n' -> [GridCoord n n']


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

tileVectorFromMines :: forall f n.
  (Functor f, Foldable f, KnownNat n) =>
  (Finite n -> f (Finite n))
  -> VS.Vector n Bool
  -> VS.Vector n BoardTile
tileVectorFromMines getAdj mineV = tileV
  where
    (StoreT (Identity tileV) _) = extend (tileFromMineStore getAdj)
                                  $ StoreT (Identity mineV) minBound

mineVectorFromIndexPairs :: forall n.
  KnownNat n =>
  Finite (n + 1)
  -> [(Finite n, Finite n)]
  -> VS.Vector n Bool
mineVectorFromIndexPairs numMines indexPairs =
  runST st
  where
    st :: (forall s. ST s (VS.Vector n Bool))
    st = do
      v :: VMS.MVector n s Bool <- VMS.new
      forM_ (take (fromIntegral numMines) $ enumFrom minBound)
        (\i -> VMS.write v i True)
      forM_ indexPairs $ uncurry $ VMS.swap v
      v' <- VS.freeze v
      return v'

startBoardStateFromTileGrid :: forall n n'.
  (KnownNat n, KnownNat n') =>
  Grid n n' BoardTile -> BoardState n n'
startBoardStateFromTileGrid g =
  BoardState { _tileGrid = startGridState
             , _numRevealed = 0
             , _numMarked = 0
             }
  where
    makeStartTileState t =
      BoardTileState { _tile = t
                     , _isRevealed = False
                     , _isMarked = False
                     }
    startGridState = makeStartTileState <$> g

startBoardStateFromCoordPairs :: forall n n' e.
  (KnownNat n, KnownNat n', HasBoardEnv e n n') =>
  e n n'-> [(GridCoord n n', GridCoord n n')] -> BoardState n n'
startBoardStateFromCoordPairs boardEnv =
  startBoardStateFromTileGrid
  . tileVectorFromIndexPairs'
  . L.over (L.mapped . L.both) (L.view $ LI.from gridIndexCoord)
  where
    tileVectorFromIndexPairs' = tileVectorFromMines getAdj'
                                . mineVectorFromIndexPairs'
    getAdj' = LT.traverseOf gridIndexCoord . getAdj $ boardEnv
    mineVectorFromIndexPairs' = mineVectorFromIndexPairs
                                . numMines $ boardEnv

--TODO use monadreader herer
--make gameenv more modular like the font
randomStartBoardState :: forall n n' e m.
  (KnownNat n, KnownNat n', HasBoardEnv e n n', MonadInterleave m) =>
  e n n'-> m (BoardState n n')
randomStartBoardState boardEnv =
  startBoardStateFromCoordPairs boardEnv
  . L.over (L.mapped . L.both) (L.view gridIndexCoord)
  <$> (indexPairsChooseK $ numMines boardEnv)


dfsUnrevealedNonMines :: forall n n'. (KnownNat n, KnownNat n') =>
  Grid n n' BoardTileState
  -> (GridCoord n n' -> [GridCoord n n'])
  -> GridCoord n n' -> [GridCoord n n']
dfsUnrevealedNonMines gr f = unfoldDfs getAdjFiltered
  where
    tileStateAtCoord gr' = FR.index gr' . (L.view $ LI.from gridIndexCoord)
    getAdjFiltered coord
      | getAny . foldMap Any
        $ fmap ($ tileStateAtCoord gr coord)
        [_isRevealed
        , _isMine . _tile
        , (> 0) . _numAdjMines . _tile] = []
      | otherwise = filter (not . _isMine . _tile . tileStateAtCoord gr)
                    . f $ coord

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
