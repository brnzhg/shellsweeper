{-# LANGUAGE
ScopedTypeVariables
, DataKinds
, TypeOperators
, KindSignatures
, RankNTypes
, UndecidableInstances
, AllowAmbiguousTypes
#-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

--or gamestate? all nonui aspects of game state transformations
--where cursor is doesnt matter for example
module Game() where

import Data.Foldable --
import Data.Traversable --

import Control.Monad.ST (ST, runST)
import Control.Monad.State.Lazy (StateT(..), get, put, lift, evalStateT)
import Control.Monad.Random (MonadInterleave)

import Data.Finite
import GHC.TypeLits

import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Sized as VS
import qualified Data.Vector.Generic.Mutable.Base as VGM
import qualified Data.Vector.Generic.Mutable.Sized as VGMS

import ChooseFinite (swapIndexPairs, indexPairsChooseK)
import Grid (Grid(..), GridCoord(..), GridIndex(..), gridIndexCoord)
import Board (BoardTile(..), makeTileVector)


data BoardTileState = BoardTileState
  { tile :: BoardTile
  , isRevealed :: Bool
  , isMarked :: Bool
  }
--this could be thing in State Monad
--add numAdjMineMarked

data BoardState (n :: Nat) (n' :: Nat) = BoardState
  { tileGrid :: Grid n n' BoardTileState
  , numRevealed :: Finite (n * n' + 1)
  , numMarked :: Finite (n * n' + 1)
  }

data GameEndState = WinState | LoseState

data GameState (n :: Nat) (n' :: Nat) = GameState
  { board :: BoardState n n'
  , gameEnd :: Maybe GameEndState
  }
--gamestate has score? parameterize it later?
--has win or lose or something (maybe bool iswin)


--reset (dont regenerate)
--generate start
--reveal tile
--mark tile


--exchangeAndSwap :: forall v n m a. (PrimMonad m )

--TODO move this to board
--board needs to change from taking mine indices
--(replacement index, mine index)
{-
mineVectorFromIndexPairs :: forall v n m.
  (MonadInterleave m
  , KnownNat n) =>
  Finite (n + 1)
  -> [(Finite n, Finite n)]
  -> VS.Vector n Bool
mineVectorFromIndexPairs numMines indexPairs =
  runST st
  where
    st :: (forall s. ST s (VS.Vector n Bool))
    st = do
      let (v :: VS.MVector VM.MVector n s Bool) =
            VS.generate (\i -> (shift i) <= numMInes)
      swapIndexPairs indexPairs v
-}
