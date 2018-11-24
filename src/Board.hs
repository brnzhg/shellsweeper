{-# LANGUAGE
ScopedTypeVariables
, DataKinds
, TypeOperators
, KindSignatures
, RankNTypes
, UndecidableInstances
, AllowAmbiguousTypes
#-}

module Board (
  BoardTile(..)
  , makeTileVector
  ) where

import Data.Monoid (Sum(..))

import Data.Finite (Finite)
import Data.Functor.Identity (Identity(..))
import Control.Comonad (Comonad(..))
import Control.Comonad.Representable.Store
  (Store, StoreT(..), ComonadStore, store, experiment, runStore)
import Control.Arrow ((&&&))

import Numeric.Natural

--import qualified Data.Vector as V
import qualified Data.Vector.Sized as VS

import GHC.TypeLits
--import qualified Data.Singletons.Prelude as SP
--import qualified Data.Singletons.TypeLits as STL

import qualified Control.Lens as L
--import qualified Control.Lens.Iso as LI
--import qualified Control.Lens.Traversal as LT


data BoardTile = BoardTile
  { isMine :: Bool
  , numAdjMines :: Natural
  } --TODO need lenses


--TODO think if there's something more general here
--foldExperiment :: (Functor f, Foldable f, Monoid b, ComonadStore s w) =>
--  (s -> f s) -> (a -> b) -> w a -> b
--foldExperiment f g = foldMap g . experiment f


tileFromMineStore :: (Functor f, Foldable f, ComonadStore s w) =>
  (s -> f s) -> w Bool -> BoardTile
tileFromMineStore getAdj mineStore =
  BoardTile { isMine = isMine'
            , numAdjMines = numAdjMines'
            }
  where
    getNumAdjMines =  fromIntegral
                      . getSum
                      . foldMap (\b -> if b then Sum 1 else Sum 0)
                      . experiment getAdj
    (isMine', numAdjMines') = (&&&) extract getNumAdjMines mineStore

makeTileVector :: forall f n. (Functor f, Foldable f, KnownNat n) =>
  (Finite n -> f (Finite n)) -> [Finite n] -> VS.Vector n BoardTile
makeTileVector getAdj mineIndices = tileV
  where
    mineV = VS.replicate False VS.// (zip mineIndices $ repeat True)
    (StoreT (Identity tileV) _) = extend (tileFromMineStore getAdj)
                                  $ StoreT (Identity mineV) minBound



--TODO pretty print BoardTile and Board

{-
   _________
  /        /|
 /________/ |
 |        | |
 |        | |
 |        | /
 |________|/

 / \ / \ /
| 1 | * |
 \ / \ / \
  | 3 | * |
 / \ / \ /
| * | 1 |

-x---x---x---
/2\*/2\1/1\1/
---x---x---x-
\2/*\2/1\*/1\
-x---x---x---x

-}
