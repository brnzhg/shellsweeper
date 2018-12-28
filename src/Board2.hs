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
--import Data.Functor.Rep as FR
import Data.Monoid (Any(..))
import Data.Traversable
import Data.Hashable(Hashable(..))
--import Control.Monad (forM_)

import Control.Monad ((<=<), filterM)
import Control.Monad.Reader (ReaderT(..), MonadReader(..))
--import Control.Monad.State (MonadState(..), modify)
--import Control.Monad.State.Strict (StateT(..), get, put, lift, evalStateT)

--import Control.Comonad (Comonad(..))
--import Control.Comonad.Representable.Store
--  (Store, StoreT(..), ComonadStore, store, experiment, runStore)
--import Control.Arrow ((&&&))

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
  putTile :: k -> tls -> m ()
  modifyAllBoardTiles :: ((k, tls) -> tls) -> m ()
  getBoardSum :: m bs
  modifyBoardSum :: (bs -> bs) -> m ()


--TODO add lives?

getBoardNumSpaces :: (HasBoardNumMines e, FRB.BoardKey k) =>
  Proxy k -> e -> Natural
getBoardNumSpaces p env = FRB.domainSize p - boardNumMines env

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

revealIndex :: forall k tl mrk m.
  (FRB.BoardKey k
  , HasTile tl
  , HasMark mrk
  , MonadBoardState k (TileState tl mrk) (BoardSum mrk) m) =>
  k -> m ()
revealIndex k = do
  tls <- getTile k
  let numRevealedInc = if tls^.isRevealed then 0 else 1
      tlsMark = tls^.mark
  putTile k $ tls
    & isRevealed .~ True
    & mark .~ mempty
  modifyBoardSum (\bs -> bs
                         & numRevealed +~ numRevealedInc
                         & markSum %~ (mappend . invert $ tls^.mark))

revealBoardTile :: forall k e tl mrk m.
  (FRB.BoardKey k
  , HasBoardEnv k e
  , MonadReader e m
  , HasTile tl
  , HasMark mrk
  , MonadBoardState k (TileState tl mrk) (BoardSum mrk) m) =>
  k -> m (Maybe Bool, [k])
revealBoardTile k = (marksMine . view mark <$> tlsM)
  >>= (\b -> if b then revealCancel else revealSuccess)
  where
    tlsM = getTile k
    revealCancel = return (Nothing, [])
    revealSuccess = do
      indicesToReveal <- dfsUnrevealedNonMines k
      traverse revealIndex indicesToReveal
      revealedMine <- isMine . view tile <$> tlsM
      endFlag <- if revealedMine
                 then return $ Just False
                 else (do
                          bs <- getBoardSum
                          env <- ask
                          let numSpaces =
                                getBoardNumSpaces (Proxy :: Proxy k) env
                          return $ if bs^.numRevealed == numSpaces
                                   then Just True
                                   else Nothing)
      return (endFlag, indicesToReveal)

modifyBoardTileMark :: forall k e tl mrk m.
  (FRB.BoardKey k
  , HasTile tl
  , HasMark mrk
  , MonadBoardState k (TileState tl mrk) (BoardSum mrk) m) =>
  (mrk -> mrk) -> k -> m ()
modifyBoardTileMark f k = do
  tls <- getTile k
  let currentMark = tls ^. mark
      newMark = f currentMark
  putTile k $ tls & mark .~ newMark
  modifyBoardSum (\bs -> bs
                         & markSum %~ (<> invert currentMark
                                       <> newMark))
