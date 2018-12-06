{-# LANGUAGE
ScopedTypeVariables
, DataKinds
, TypeOperators
, KindSignatures
, RankNTypes
, UndecidableInstances
, AllowAmbiguousTypes
, MultiParamTypeClasses
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

import Data.Finite
import GHC.TypeLits

import Control.Lens ((^.), (%~))
import qualified Control.Lens as L
import qualified Control.Lens.Iso as LI
import qualified Control.Lens.Traversal as LT

import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Sized as VS
import qualified Data.Vector.Generic.Mutable.Base as VGM
import qualified Data.Vector.Generic.Mutable.Sized as VGMS

import ChooseFinite (indexPairsChooseK)
import Grid (Grid(..), GridCoord(..), GridIndex(..), gridIndexCoord)
import Board (BoardTile(..)
              , mineVectorFromIndexPairs)

data GameEndState = WinState | LoseState

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


