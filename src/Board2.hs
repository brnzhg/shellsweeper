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

module Board2 (
  SingleMineTile(..)
  , MultiMineTile(..)
  , HasBoardNumMines(..)
  , HasTile(..)
  , HasMark(..)
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

import Control.Monad ((<=<), filterM)
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
import Graph (unfoldDfs, unfoldDfsM)
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

data BoardSum mrk =
  BoardSum {
  _numRevealed :: Natural
  , _markSum :: mrk
  }

makeLenses ''TileState
makeLenses ''BoardSum


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


class HasBoardGetAdj k e | e -> k where
  getAdj :: e -> k -> [k]

class HasBoardNumMines e where
  boardNumMines :: e -> Natural

class (HasBoardGetAdj f e, HasBoardNumMines e) => HasBoardEnv f e


class Monad m => MonadBoardState k tls bs m | m -> k, m -> tls, m -> bs where
  getTile :: k -> m tls
  modifyBoardTile :: (tls -> tls) -> k -> m ()
  modifyAllBoardTiles :: ((k, tls) -> tls) -> m ()
  getBoardSum :: m bs
  modifyBoardSum :: (bs -> bs) -> m ()


--TODO add lives
--TODO rename from board to game
{-
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
-}

{-
getRepBoardNumSpaces :: (HasBoardNumMines e, FRB.BoardFunctorKey k) =>
  Proxy k -> e -> Natural
getRepBoardNumSpaces p env = FRB.domainSize p - boardNumMines env
-}

dfsUnrevealedNonMines :: forall k e tl mrk bs m.
  (FRB.BoardKey k
  , HasBoardGetAdj k e
  , MonadReader e m
  , HasTile tl
  , HasMark mrk
  , MonadBoardState k (TileState tl mrk) bs m) =>
  k -> m [k]
dfsUnrevealedNonMines =
  filterM (fmap (not . marksMine . view mark) . getTile)
  <=< unfoldDfsM getAdjFiltered
  where
    getAdjFiltered k = do
      tls <- getTile k
      env <- ask
      if getAny . foldMap Any
         $ fmap ($ tls)
         [ (^.isRevealed)
         , (isMine . view tile)
         , (isMineAdj . view tile)]
         --, (marksMine . view mark)]
        then return []
        else filterM (fmap (not . isMine . view tile) . getTile)
             $ getAdj env k


--TODO assumes all indices are unmarked
--TODO need StateT on top of m, do some magic count what changed and shit, cool shit
revealIndices :: forall k tl mrk m.
  (FRB.BoardKey k
  , HasTile tl
  , HasMark mrk
  , MonadBoardState k (TileState tl mrk) (BoardSum mrk) m) =>
  [k] -> m ()
revealIndices indicesToReveal = do
  modifyBoardTiles
    ((isRevealed .~ True) . view _2)
    indicesToReveal
  tileStates <- traverse getTile indicesToReveal
  let numRevealedInc = fromIntegral $ length indicesToReveal
      revealMarkSum = mconcat $ map (view mark) tileStates
  modifyBoardSum (\bs -> bs
                         & numRevealed +~ numRevealedInc
                         & markSum %~ (<> invert revealMarkSum))

{-
  & tileStates %~ revealTileStates
  & numRevealed +~ (fromIntegral $ length indicesToReveal)
  & markSum %~ (<> invert revealMarkSum) --all marks get cleared if revealed
  where
    revealTileStates ts = FRB.updateOver ts (isRevealed .~ True) indicesToReveal
    revealMarkSum = mconcat . map (view mark . FR.index (bs^.tileStates))
                    $ indicesToReveal
-}
{-
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
-}
