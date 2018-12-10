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
#-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

--or gamestate? all nonui aspects of game state transformations
--where cursor is doesnt matter for example
module Game() where

import Data.Foldable --
import Data.Traversable --
import Data.Bifunctor (bimap)

import Control.Monad.State.Lazy (StateT(..), get, put, lift, evalStateT)
import Control.Monad.Random (MonadInterleave)
import Control.Monad.Request.Lazy (RequestT(..))
import Control.Monad.Request.Class (MonadRequest(..))
  
import Data.Finite
import GHC.TypeLits

import Control.Lens
import qualified Control.Lens.Iso as LI
import qualified Control.Lens.Traversal as LT


--
--import Grid (Grid(..), GridCoord(..), GridIndex(..))
import Board ()

data GameEndState = WinState | LoseState | DnfState

data GameRequest i mrk =
  RevealTileRequest i
  | MarkTileRequest i mrk
  | ResetRequest

class MonadRequest (GameRequest i mrk) (Maybe GameEndState) m => MonadGame i mrk m


--class HasTileState s where
--


{-
data GameState (n :: Nat) (n' :: Nat) = GameState
  { _board :: BoardState n n'
  , _gameEnd :: Maybe GameEndState
  }

data GameEnv (n :: Nat) (n' :: Nat) = GameEnv
  { _numMines :: Finite (n * n' + 1)
  , _getAdj :: GridCoord n n' -> [GridCoord n n']
  }
-}
--gamestate has score? parameterize it later?
--has win or lose or something (maybe bool iswin)

--reset (dont regenerate)
--generate start
--reveal tile
--mark tile




--TODO doesn't require anythign from current game env
--might later, better to split env by interface


