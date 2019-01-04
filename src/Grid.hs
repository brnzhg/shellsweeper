{-# LANGUAGE
ScopedTypeVariables
, GeneralizedNewtypeDeriving
, FlexibleInstances
, FlexibleContexts
, DataKinds
, TypeOperators
, KindSignatures
, TypeFamilies
#-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Grid (
  MGrid(..)
, Grid(..)
, GridCoord(..)
, GridIndex(..)
, MGridBoardState(..)
, MGridBoardReader(..)
, gridIndexCoord
, gridCoordIndex
, squareAdjacentCoords
, singleMineMGridFromSwapPairs
, randomSingleMineMGrid
, singleMineTileGridFromMines
, singleMineMGridBoardStateFromTiles
, randomSingleMineMGridBoardState
, gridToVecOfVec
, gridToListOfList
) where

import Data.STRef
import Data.Primitive.MutVar
import Data.Proxy
import Data.List (mapAccumL, splitAt)
import Data.Foldable (toList)
import Data.Traversable
import Data.Finite (Finite, getFinite, modulo)
import Data.Bifunctor (bimap)
import Data.Functor.Rep as FR
import Data.Functor.RepB as FRB
import Data.Distributive as FD

import Control.Monad (replicateM, forM_)
import Control.Monad.Base (MonadBase(..), liftBase)
import Control.Monad.ST (ST(..), runST)
import Control.Monad.State.Strict (State(..), get, state, evalState)
import Control.Monad.Reader (MonadReader(..), ReaderT(..), lift, runReaderT)
import Control.Monad.Random (MonadInterleave, interleave)
import Control.Monad.Primitive (PrimMonad(..), PrimState)
import Control.Arrow ((&&&))

import Data.Hashable (Hashable(..))
import qualified Data.Vector as V
import qualified Data.Vector.Sized as VS
import qualified Data.Vector.Mutable.Sized as VMS

import GHC.TypeLits
import qualified Data.Singletons.Prelude as SP
import qualified Data.Singletons.TypeLits as STL

import qualified Control.Lens as L
import qualified Control.Lens.Iso as LI
import qualified Control.Lens.Traversal as LT

import Data.Functor.RepB (BoardKey(..), BoardFunctor(..))
import Data.Finite.Extras (packFiniteDefault)
import ChooseFinite (indexSwapPairsChooseK
                    , vectorSwapPairs)
import Board2 (SingleMineTile(..)
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
              , singleMineTilesFromMines)


newtype Grid (n :: Nat) (n' :: Nat) a =
  Grid { getVector :: VS.Vector (n * n') a }
  deriving (Functor, Applicative, Monad, Foldable)

newtype MGrid (n :: Nat) (n' :: Nat) s a =
  MGrid { getMVector :: VMS.MVector (n * n') s a }

--TODO use deriving via to get other gridcoords possibly
newtype GridCoord (n :: Nat) (n' :: Nat) = GridCoord { unCoord :: (Finite n, Finite n')}
  deriving (Eq, Show)
type GridIndex (n :: Nat) (n' :: Nat) = Finite (n * n')


data MGridBoardState (n :: Nat) (n' :: Nat) s tl mrk = MGridBoardState
  { _boardTileGrid :: !(MGrid n n' s (TileState tl mrk))
  , _boardSum :: !(MutVar s (BoardSum mrk))
  }

newtype MGridBoardReader (n :: Nat) (n' :: Nat) s tl mrk a =
  MGridBoardReader { unMGridBoardReader :: ReaderT (MGridBoardState n n' s tl mrk) (ST s) a }
  deriving (Functor
           , Applicative
           , Monad
           , MonadReader (MGridBoardState n n' s tl mrk)
           , MonadBase (ST s))

gridIndexCoord :: forall n n'. (KnownNat n, KnownNat n') =>
  LI.Iso' (GridIndex n n') (GridCoord n n')
gridIndexCoord = LI.iso getCoord getIndex
  where n'int :: Integer
        n'int = fromIntegral . SP.FromSing $ (SP.sing :: STL.SNat n')
        getIndex :: GridCoord n n' -> Finite (n * n')
        getIndex (GridCoord (fn, fn')) =
          modulo . fromIntegral $ (getFinite fn) * n'int + (getFinite fn')
        getCoord :: Finite (n * n') -> GridCoord n n'
        getCoord = GridCoord
                   . bimap (modulo . fromIntegral) (modulo . fromIntegral)
                   . (`divMod` n'int)
                   . getFinite

gridCoordIndex :: forall n n'. (KnownNat n, KnownNat n') =>
  LI.Iso' (GridCoord n n') (GridIndex n n')
gridCoordIndex = LI.from gridIndexCoord

--TODO these are temporary i think, better way to do this
freezeGrid :: forall n n' m a. (KnownNat n, KnownNat n', PrimMonad m) =>
  MGrid n n' (PrimState m) a -> m (Grid n n' a)
freezeGrid (MGrid v) = Grid <$> VS.freeze v

thawGrid :: forall n n' m a. (KnownNat n, KnownNat n', PrimMonad m) =>
  Grid n n' a -> m (MGrid n n' (PrimState m) a)
thawGrid (Grid v) = MGrid <$> VS.thaw v


instance (KnownNat n, KnownNat n') => Hashable (GridCoord n n') where
  hashWithSalt i = hashWithSalt i . L.view gridCoordIndex

instance (KnownNat n, KnownNat n') => Bounded (GridCoord n n') where
  minBound = L.view gridIndexCoord minBound
  maxBound = L.view gridIndexCoord maxBound

instance (KnownNat n, KnownNat n') => Enum (GridCoord n n') where
  toEnum = L.view gridIndexCoord . toEnum
  fromEnum = fromEnum . L.view gridCoordIndex

instance (KnownNat n, KnownNat n') => BoardKey (GridCoord n n') where
  domainSize _ = SP.fromSing nsing * SP.fromSing nsing'
    where
      nsing :: STL.SNat n
      nsing = SP.sing
      nsing' :: STL.SNat n'
      nsing' = SP.sing

instance (KnownNat n, KnownNat n') => FD.Distributive (Grid n n') where
  distribute = FR.distributeRep

instance (KnownNat n, KnownNat n') => FR.Representable (Grid n n') where
  type Rep (Grid n n') = GridCoord n n'
  index (Grid v) = VS.index v . L.view gridCoordIndex
  tabulate f = Grid $ tabulate (f . L.view gridIndexCoord)


--TODO use wrap unwrap lenses for this grid to vec stuff
instance (KnownNat n, KnownNat n') => FRB.BoardFunctor (Grid n n') where
  update (Grid v) = Grid
                    . (v VS.//)
                    . (L.mapped . L._1 L.%~ L.view (gridCoordIndex))

--TODO make this shit better with lenses
instance (KnownNat n
         , KnownNat n'
         , HasTile tl
         , HasMark mrk) =>
  MonadBoardState (MGridBoardReader n n' s tl mrk) where
  type BSKey (MGridBoardReader n n' s tl mrk) = (GridCoord n n')
  type BSTile (MGridBoardReader n n' s tl mrk) = tl
  type BSMark (MGridBoardReader n n' s tl mrk) = mrk
  getTile k = do
    (MGrid v) <- _boardTileGrid <$> ask
    liftBase $ VMS.read v $ k L.^. gridCoordIndex
  putTile k tls = do
    (MGrid v) <- _boardTileGrid <$> ask
    liftBase $ VMS.write v (k L.^. gridCoordIndex) tls
  modifyAllBoardTiles f = do
    (MGrid v) <- _boardTileGrid <$> ask
    liftBase $ forM_ [minBound..maxBound]
      (\k -> VMS.modify v (\tls -> f (k, tls)) (k L.^. gridCoordIndex))
  getBoardSum = ask >>= (liftBase . readMutVar . _boardSum)
  modifyBoardSum f = ask >>= (liftBase . flip modifyMutVar' f . _boardSum)

{-
instance (KnownNat n
         , KnownNat n'
         , HasTile tl
         , HasMark mrk
         , PrimMonad m
         , PrimState m ~ s) =>
  MonadBoardState (ReaderT (MGridBoardState n n' s tl mrk) m) where
  type BSKey (ReaderT (MGridBoardState n n' s tl mrk) m) = (GridCoord n n')
  type BSTile (ReaderT (MGridBoardState n n' s tl mrk) m) = tl
  type BSMark (ReaderT (MGridBoardState n n' s tl mrk) m) = mrk
  getTile k = do
    (MGrid v) <- _boardTileGrid <$> ask
    lift $ VMS.read v $ k L.^. gridCoordIndex
  putTile k tls = do
    (MGrid v) <- _boardTileGrid <$> ask
    lift $ VMS.write v (k L.^. gridCoordIndex) tls
  modifyAllBoardTiles f = do
    (MGrid v) <- _boardTileGrid <$> ask
    lift $ forM_ [minBound..maxBound]
      (\k -> VMS.modify v (\tls -> f (k, tls)) (k L.^. gridCoordIndex))
  getBoardSum = ask >>= (lift . readMutVar . _boardSum)
  modifyBoardSum f = ask >>= (lift . flip modifyMutVar' f . _boardSum)
-}

squareAdjacentCoords :: forall n n'. (KnownNat n, KnownNat n') =>
  GridCoord n n' -> [GridCoord n n']
squareAdjacentCoords (GridCoord (r, c)) =
  [GridCoord (r', c')
  | r' <- getAdjOneD r,
   c' <- getAdjOneD c,
    (r', c') /= (r, c)]
  where getAdjOneD x = [packFiniteDefault minBound (fromIntegral x - 1)
                        ..
                        packFiniteDefault maxBound (fromIntegral x + 1)]


--TODO better use of newtype iso
singleMineMGridFromSwapPairs :: (KnownNat n, KnownNat n', PrimMonad m) =>
  Finite (n * n' + 1)
  -> [(GridIndex n n', GridIndex n n')]
  -> m (MGrid n n' (PrimState m) Bool)
singleMineMGridFromSwapPairs mines indexSwapPairs = do
  mg@(MGrid v) <- MGrid <$> VMS.new
  forM_ (take (fromIntegral mines) $ enumFrom minBound)
    (\i -> VMS.write v i True)
  vectorSwapPairs v indexSwapPairs
  return mg
-- . L.over (L.mapped . L.both) (L.view gridCoordIndex)

randomSingleMineMGrid :: (KnownNat n
                         , KnownNat n'
                         , HasBoardNumMines e
                         , MonadReader e m
                         , MonadInterleave m
                         , PrimMonad m') =>
  m (m' (MGrid n n' (PrimState m') Bool))
randomSingleMineMGrid = do
  env <- ask
  let mines = packFiniteDefault maxBound
              $ fromIntegral
              $ boardNumMines env
  indexSwapPairs <- indexSwapPairsChooseK mines
  return $ singleMineMGridFromSwapPairs mines indexSwapPairs


singleMineTileGridFromMines ::
  (KnownNat n
  , KnownNat n'
  , HasBoardGetAdj (GridCoord n n') e
  , MonadReader e m) =>
  Grid n n' Bool -> m (Grid n n' SingleMineTile)
singleMineTileGridFromMines g =
  (\env -> singleMineTilesFromMines (getAdj env) g) <$> ask


singleMineMGridBoardStateFromTiles ::
  (KnownNat n
  , KnownNat n'
  , HasMark mrk
  , PrimMonad m) =>
  Grid n n' SingleMineTile -> m (MGridBoardState n n' (PrimState m) SingleMineTile mrk)
singleMineMGridBoardStateFromTiles tlGrid = do
  let fzTlsGrid = fmap (\tl -> TileState { _tile = tl
                                         , _isRevealed = False
                                         , _mark = mempty
                                         }) tlGrid
  tlsGrid <- thawGrid fzTlsGrid
  emptyBoardSum <- newMutVar getEmptyBoardSum
  return MGridBoardState { _boardTileGrid = tlsGrid
                         , _boardSum = emptyBoardSum
                         }

randomSingleMineMGridBoardState :: forall n n' e m mrk m'.
  (KnownNat n
  , KnownNat n'
  , HasBoardEnv (GridCoord n n') e
  , MonadReader e m
  , MonadInterleave m
  , HasMark mrk
  , PrimMonad m') =>
  m (m' (MGridBoardState n n' (PrimState m') SingleMineTile mrk))
randomSingleMineMGridBoardState = do
  (stMnGrid :: m' (MGrid n n' (PrimState m') Bool)) <- randomSingleMineMGrid
  fzMnGrid <- return $ do
    mnGrid <- stMnGrid
    freezeGrid mnGrid
  env <- ask
  return $ fzMnGrid
    >>= flip runReaderT env . singleMineTileGridFromMines
    >>= singleMineMGridBoardStateFromTiles


gridToVecOfVec :: forall n n' a. (KnownNat n, KnownNat n') =>
  Grid n n' a -> VS.Vector n (VS.Vector n' a)
gridToVecOfVec gr =
  VS.generate (\i ->
                  VS.generate (\j ->
                                  index gr $ GridCoord (i, j)))

gridToListOfList :: forall n n' a. (KnownNat n, KnownNat n') =>
  Grid n n' a -> [[a]]
gridToListOfList = evalState
                   (replicateM (fromIntegral (maxBound :: Finite n)) m)
                   . toList
  where
    m = state $ splitAt $ fromIntegral (maxBound :: Finite n')

