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

module Game() where

import Data.Foldable --
import Data.Traversable --

import Control.Monad.State.Lazy (StateT(..), get, put, lift, evalStateT)

data BoardTileGST = BoardTileGST
  { isRevealed :: Bool
  , isMarked :: Bool
  }

--data BoardGST


