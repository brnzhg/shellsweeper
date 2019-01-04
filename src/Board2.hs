{-# LANGUAGE
ScopedTypeVariables
, DataKinds
, TypeOperators
, KindSignatures
, RankNTypes
, MultiParamTypeClasses
, TemplateHaskell
, FlexibleContexts
, FlexibleInstances
, FunctionalDependencies
, DuplicateRecordFields
, TypeFamilies
#-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Board2 (
  SingleMineTile(..)
  , MultiMineTile(..)
  , TileState(..)
  , BoardSum(..)
  , HasBoardGetAdj(..)
  , HasBoardNumMines(..)
  , HasBoardEnv(..)
  , HasTile(..)
  , HasMark(..)
  , MonadBoardState(..)
  , getEmptyBoardSum
  , getBoardNumSpaces
  , singleMineTilesFromMines
  , clearBoard
  , revealBoardTile
  , modifyBoardTileMark
  ) where

import Data.Proxy
import Data.Monoid (Sum(..))
import Data.Group (Group(..), Abelian(..))

import Data.Finite (Finite)
--import Data.Functor.Identity (Identity(..))
import qualified Data.Functor.Rep as FR
import Data.Monoid (Any(..))
import Data.Traversable
import Data.Hashable (Hashable(..))

import Control.Monad ((<=<), filterM)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Primitive (PrimMonad, PrimState)
--import Control.Monad.State (MonadState(..), modify)
--import Control.Monad.State.Strict (StateT(..), get, put, lift, evalStateT)
import Control.Arrow ((&&&))
import Control.Comonad (Comonad(..))
import Control.Comonad.Representable.Store
  (Store, StoreT(..), ComonadStore, store, experiment, runStore)

import Numeric.Natural

import GHC.TypeLits

import Control.Lens
import qualified Control.Lens.Iso as LI
import qualified Control.Lens.Traversal as LT

import qualified Data.Functor.RepB as FRB
import Graph (unfoldDfs, unfoldDfsPrim)


data SingleMineTile = SingleMineTile
  { _isMine :: !Bool
  , _numAdjMines :: !Natural
  }

data MultiMineTile = MultiMineTile
  { _numMines :: !Natural
  , _numAdjMines :: !Natural
  }

data TileState tl mrk = TileState
  { _tile :: !tl
  , _isRevealed :: !Bool
  , _mark :: !mrk
  }

data BoardSum mrk = BoardSum
  { _numSpacesRevealed :: !Natural
  , _markSum :: !mrk
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

class (HasBoardGetAdj k e, HasBoardNumMines e) => HasBoardEnv k e


class (Monad m
      , FRB.BoardKey (BSKey m)
      , HasTile (BSTile m)
      , HasMark (BSMark m)) => MonadBoardState m where
  type BSKey m :: *
  type BSTile m :: *
  type BSMark m :: *
  getTile :: BSKey m -> m (TileState (BSTile m) (BSMark m))
  putTile :: BSKey m -> TileState (BSTile m) (BSMark m) -> m ()
  modifyAllBoardTiles :: ((BSKey m, TileState (BSTile m) (BSMark m)) -> TileState (BSTile m) (BSMark m)) -> m ()
  getBoardSum :: m (BoardSum (BSMark m))
  modifyBoardSum :: ((BoardSum (BSMark m)) -> (BoardSum (BSMark m))) -> m ()


getEmptyBoardSum :: Monoid mrk => BoardSum mrk
getEmptyBoardSum = BoardSum { _numSpacesRevealed = 0
                            , _markSum = mempty
                            }

getBoardNumSpaces :: (HasBoardNumMines e, FRB.BoardKey k) =>
  Proxy k -> e -> Natural
getBoardNumSpaces p env = FRB.domainSize p - boardNumMines env



singleMineTileFromMineStore :: (Functor f, Foldable f, ComonadStore s w) =>
  (s -> f s) -> w Bool -> SingleMineTile
singleMineTileFromMineStore adj mineStore =
  SingleMineTile { _isMine = isMine'
                 , _numAdjMines = numAdjMines'
                 }
  where getNumAdjMines =  fromIntegral
                      . getSum
                      . foldMap (\b -> if b then Sum 1 else Sum 0)
                      . experiment adj
        (isMine', numAdjMines') = (&&&) extract getNumAdjMines mineStore

singleMineTilesFromMines :: forall f g.
  (Functor f, Foldable f, FR.Representable g) =>
  (FR.Rep g -> f (FR.Rep g)) -> g Bool -> g SingleMineTile
singleMineTilesFromMines adj mineRepbl = tileRepbl
  where
    (StoreT (Identity tileRepbl) _) =
      extend (singleMineTileFromMineStore adj)
      $ StoreT (Identity mineRepbl) undefined

clearBoard :: MonadBoardState m => m ()
clearBoard = do
  modifyAllBoardTiles (\(_, tls) -> tls
                                    & isRevealed .~ False
                                    & mark .~ mempty)
  modifyBoardSum (const getEmptyBoardSum)

dfsUnrevealedNonMines ::
  (HasBoardGetAdj (BSKey m) e
  , MonadReader e m
  , MonadBoardState m
  , PrimMonad m) =>
  BSKey m -> m [BSKey m]
dfsUnrevealedNonMines =
  filterM (fmap (not . marksMine . view mark) . getTile)
  <=< unfoldDfsPrim getAdjFiltered
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

revealIndex :: MonadBoardState m => BSKey m -> m ()
revealIndex k = do
  tls <- getTile k
  let numRevealedInc =
        if tls^.isRevealed || (isMine $ tls^.tile) then 0 else 1
      tlsMark = tls^.mark
  putTile k $ tls
    & isRevealed .~ True
    & mark .~ mempty
  modifyBoardSum (\bs -> bs
                         & numSpacesRevealed +~ numRevealedInc
                         & markSum %~ (mappend . invert $ tls^.mark))

revealBoardTile :: forall e m.
  (FRB.BoardKey (BSKey m)
  , HasBoardEnv (BSKey m) e
  , MonadReader e m
  , MonadBoardState m
  , PrimMonad m) =>
  BSKey m -> m (Maybe Bool, [BSKey m])
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
                                getBoardNumSpaces (Proxy :: Proxy (BSKey m)) env
                          return $ if bs^.numSpacesRevealed == numSpaces
                                   then Just True
                                   else Nothing)
      return (endFlag, indicesToReveal)

modifyBoardTileMark :: MonadBoardState m => (BSMark m -> BSMark m) -> BSKey m -> m ()
modifyBoardTileMark f k = do
  tls <- getTile k
  let currentMark = tls ^. mark
      newMark = f currentMark
  putTile k $ tls & mark .~ newMark
  modifyBoardSum (\bs -> bs
                         & markSum %~ (<> invert currentMark
                                       <> newMark))


