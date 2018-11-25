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

module Board (
  BoardTile(..)
  , tileVectorFromMines
  , mineVectorFromIndexPairs
  , randomMineVector
  , randomTileVector
  ) where

import Data.Monoid (Sum(..))

import Data.Finite (Finite)
import Data.Functor.Identity (Identity(..))
import Data.Traversable
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Control.Monad.Random (MonadInterleave, interleave)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Comonad (Comonad(..))
import Control.Comonad.Representable.Store
  (Store, StoreT(..), ComonadStore, store, experiment, runStore)
import Control.Arrow ((&&&))

import Numeric.Natural

--import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Sized as VS
import qualified Data.Vector.Mutable.Sized as VMS


import GHC.TypeLits
--import qualified Data.Singletons.Prelude as SP
--import qualified Data.Singletons.TypeLits as STL

import qualified Control.Lens as L
--import qualified Control.Lens.Iso as LI
--import qualified Control.Lens.Traversal as LT

import ChooseFinite (indexPairsChooseK)


data BoardTile = BoardTile
  { _isMine :: Bool
  , _numAdjMines :: Natural
  } --TODO need lenses


--TODO think if there's something more general here
--foldExperiment :: (Functor f, Foldable f, Monoid b, ComonadStore s w) =>
--  (s -> f s) -> (a -> b) -> w a -> b
--foldExperiment f g = foldMap g . experiment f


tileFromMineStore :: (Functor f, Foldable f, ComonadStore s w) =>
  (s -> f s) -> w Bool -> BoardTile
tileFromMineStore getAdj mineStore =
  BoardTile { _isMine = isMine'
            , _numAdjMines = numAdjMines'
            }
  where
    getNumAdjMines =  fromIntegral
                      . getSum
                      . foldMap (\b -> if b then Sum 1 else Sum 0)
                      . experiment getAdj
    (isMine', numAdjMines') = (&&&) extract getNumAdjMines mineStore

tileVectorFromMines :: forall f n.
  (Functor f, Foldable f, KnownNat n) =>
  (Finite n -> f (Finite n))
  -> VS.Vector n Bool
  -> VS.Vector n BoardTile
tileVectorFromMines getAdj mineV = tileV
  where
    --mineV = VS.replicate False VS.// (zip mineIndices $ repeat True)
    (StoreT (Identity tileV) _) = extend (tileFromMineStore getAdj)
                                  $ StoreT (Identity mineV) minBound

mineVectorFromIndexPairs :: forall n.
  KnownNat n =>
  Finite (n + 1)
  -> [(Finite n, Finite n)]
  -> VS.Vector n Bool
mineVectorFromIndexPairs numMines indexPairs =
  runST st
  where
    st :: (forall s. ST s (VS.Vector n Bool))
    st = do
      v :: VMS.MVector n s Bool <- VMS.new
      forM_ (take (fromIntegral numMines) $ enumFrom minBound)
        (\i -> VMS.write v i True)
      forM_ indexPairs $ uncurry $ VMS.swap v
      v' <- VS.freeze v
      return v'

randomMineVector :: forall n m.
  (KnownNat n, MonadInterleave m) =>
  Finite (n + 1) -> m (VS.Vector n Bool)
randomMineVector numMines =
  mineVectorFromIndexPairs numMines
  <$> (indexPairsChooseK numMines)

randomTileVector :: forall f n m.
  (Functor f, Foldable f, KnownNat n, MonadInterleave m) =>
  (Finite n -> f (Finite n))
  -> Finite (n + 1)
  -> m (VS.Vector n BoardTile)
randomTileVector getAdj =
  (tileVectorFromMines getAdj <$>) . randomMineVector
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
