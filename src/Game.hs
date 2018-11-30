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
              , tileVectorFromMines
              , mineVectorFromIndexPairs
              , randomMineVector
              , randomTileVector)


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

data GameEndState = WinState | LoseState

data GameState (n :: Nat) (n' :: Nat) = GameState
  { _board :: BoardState n n'
  , _gameEnd :: Maybe GameEndState
  }

data GameEnv (n :: Nat) (n' :: Nat) = GameEnv
  { _numMines :: Finite (n * n' + 1)
  --, _getAdj :: Finite (n * n') -> [Finite (n * n')]
  , _getAdj :: GridCoord n n' -> [GridCoord n n']
  }

--gamestate has score? parameterize it later?
--has win or lose or something (maybe bool iswin)

--reset (dont regenerate)
--generate start
--reveal tile
--mark tile

--TODO doesn't require anythign from current game env
--might later, better to split env by interface
startGameStateFromTileGrid :: forall n n'.
  (KnownNat n, KnownNat n') =>
  Grid n n' BoardTile -> GameState n n'
startGameStateFromTileGrid g =
  GameState { _board = startBoardState
            , _gameEnd = Nothing
            }
  where
    makeStartTileState t =
      BoardTileState { _tile = t
                     , _isRevealed = False
                     , _isMarked = False
                     }
    startGridState = makeStartTileState <$> g
    startBoardState =
      BoardState { _tileGrid = startGridState
                 , _numRevealed = 0
                 , _numMarked = 0
                 }


startGameStateFromCoordPairs :: forall n n'.
  (KnownNat n, KnownNat n') =>
  GameEnv n n'
  -> [(GridCoord n n', GridCoord n n')]
  -> GameState n n'
startGameStateFromCoordPairs gameEnv =
  startGameStateFromTileGrid
  . tileVectorFromIndexPairs'
  . L.over (L.mapped . L.both) (L.view $ LI.from gridIndexCoord)
  where
    tileVectorFromIndexPairs' = tileVectorFromMines getAdj'
                                . mineVectorFromIndexPairs'
    getAdj' = LT.traverseOf gridIndexCoord . _getAdj $ gameEnv
    mineVectorFromIndexPairs' = mineVectorFromIndexPairs
                                . _numMines
                                $ gameEnv

--TODO use monadreader herer
--make gameenv more modular like the font
randomStartGameState :: forall n n' m.
  (KnownNat n, KnownNat n', MonadInterleave m) =>
  GameEnv n n' -> m (GameState n n')
randomStartGameState gameEnv =
  startGameStateFromCoordPairs gameEnv
  . L.over (L.mapped . L.both) (L.view gridIndexCoord)
  <$> (indexPairsChooseK $ _numMines gameEnv)
