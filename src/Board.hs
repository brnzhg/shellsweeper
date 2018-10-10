{-# LANGUAGE
ScopedTypeVariables
, DataKinds
, TypeOperators
, KindSignatures
#-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Board () where


import Data.Monoid (Sum(..))
import Control.Comonad (Comonad(..))
import Control.Comonad.Representable.Store (Store, StoreT(..), ComonadStore, store, experiment, runStore)

import Numeric.Natural

import qualified Data.Vector as V
import qualified Data.Vector.Sized as VS

import GHC.TypeLits
import qualified Data.Singletons.Prelude as SP
import qualified Data.Singletons.TypeLits as STL

import ChooseFinite (chooseAndSwapIndicesToK)
import Data.Finite.Extras (packFiniteDefault)
import qualified Grid


data BoardTile = BoardTile
  { isMine :: Bool
  , numAdjMines :: Natural
  , revealed :: Bool
  , flagged :: Bool
  } --TODO need lenses (raw tile, vs gamestatetile)


--TODO generate random new board

--TODO seems awkward
mineToTileGrid :: (Functor f, Foldable f, ComonadStore s w) =>
  (s -> f s) -> w Bool -> w BoardTile
mineToTileGrid getAdj = extend mineGridToTile
  where
    countAdjMines = foldMap (\b -> if b then Sum 1 else Sum 0) . experiment getAdj
    mineGridToTile mineGrid =
          BoardTile
          { isMine = extract mineGrid
          , numAdjMines = fromIntegral . getSum $ countAdjMines mineGrid
          , revealed = False
          , flagged = False
          }

--TODO pretty print BoardTile and Board
