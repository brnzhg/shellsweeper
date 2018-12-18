{-# LANGUAGE
ScopedTypeVariables
, DataKinds
, TypeOperators
, KindSignatures
, RankNTypes
, UndecidableInstances
, AllowAmbiguousTypes
, MultiParamTypeClasses
, FlexibleContexts
, TypeFamilies
, FunctionalDependencies
#-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

--or gamestate? all nonui aspects of game state transformations
--where cursor is doesnt matter for example
module Game (
MonadBoard(..)) where


import Data.Foldable --
import Data.Traversable --
--import Data.Bifunctor (bimap)

--import Control.Monad.State.Lazy (StateT(..), get, put, lift, evalStateT)
--import Control.Monad.Random (MonadInterleave)
--import Control.Monad.Request.Lazy (RequestT(..))
--import Control.Monad.Request.Class (MonadRequest(..))

--import Control.Lens
--import qualified Control.Lens.Iso as LI
--import qualified Control.Lens.Traversal as LT

{-
data GameEndState = WinState | LoseState

data GameRequest i mrk =
  RevealTileRequest i
  | MarkTileRequest i mrk
  | ResetRequest
class MonadRequest (GameRequest i mrk) (Maybe GameEndState) m => MonadGame i mrk m
-}

class Monad m => MonadBoard k mrk m | m -> k where
  revealBoardTile :: k -> m (Maybe Bool, [k])
  markBoardTile :: (mrk -> mrk) -> k -> m ()


{-
class MonadBoard m where
  data BoardKey m :: *
  data BoardMark m :: *
  revealBoardTile :: (BoardKey m) -> m Bool
  markeBoardTile :: (BoardKey m) -> (BoardMark m) -> m ()
-}
--render board MonadGame -> IO

--TODO doesn't require anythign from current game env
--might later, better to split env by interface


