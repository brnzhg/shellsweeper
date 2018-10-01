{-# LANGUAGE
ScopedTypeVariables
, DuplicateRecordFields
, FlexibleContexts
, FlexibleInstances
, FunctionalDependencies
, MultiParamTypeClasses
, RankNTypes
, KindSignatures
, DataKinds
, TemplateHaskell
, OverloadedStrings
, TypeApplications
, TypeOperators
, TypeFamilies #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Main where

import Data.Foldable
import Data.Traversable
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(..))
import Data.Functor.Identity (Identity(..))
import qualified Data.Bifunctor as BF
import qualified Data.Functor.Rep as FR
import Data.Functor.Compose
import Control.Monad.ST (ST, runST)
import Control.Monad.IO.Class
import Control.Monad.Primitive (PrimMonad, PrimState, primToST)
import Control.Monad.Reader (ask, runReader, runReaderT, Reader, ReaderT, MonadReader)
import Control.Comonad (Comonad(..))
import Control.Comonad.Representable.Store (Store, StoreT(..), ComonadStore, store, experiment, runStore)
import Numeric.Natural
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Finite as F

import qualified Data.Singletons.Prelude as SP
import qualified Data.Singletons.TypeLits as STL
import qualified Data.Vector.Generic.Sized as VGS
import qualified Data.Vector.Generic.Mutable.Sized as VGMS
--import GHC.Generics

import qualified System.Random as SR
import Control.Monad.Random (getRandomR, getRandom, MonadSplit, MonadRandom, RandomGen, evalRandT, evalRand, getSplit)

import GHC.TypeLits

import qualified Control.Lens as L
import qualified Control.Lens.Iso as LI

import RandomFinite.Vector (shuffleFirstK)


type Grid (n :: Nat) (m :: Nat) =
  Compose (VGS.Vector V.Vector n) (VGS.Vector V.Vector m)

type GridCoord (n :: Nat) (m :: Nat) = (F.Finite n, F.Finite m)


data BoardSettings = BoardSettings
  { height:: Natural
  , width  :: Natural
  , mines :: Natural
  }

data BoardTile = BoardTile
  { isMine :: Bool
  , numAdjMines :: Natural
  } --TODO need lenses (raw tile, vs gamestatetile)


data GameSettings = GameSettings
  { boardSettings :: BoardSettings }

--need GADTs
--data TestType = forall n n'. Test

-- iso from GridCoord to coord index
gridCoordIndex :: forall n m. (KnownNat n, KnownNat m) => LI.Iso' (GridCoord n m) (F.Finite (n * m))
gridCoordIndex = LI.iso F.combineProduct getCoord
  where msing :: STL.SNat m
        msing = SP.sing
        getCoord :: F.Finite (n * m) -> GridCoord n m
        getCoord = BF.bimap (F.modulo . fromIntegral) (F.modulo . fromIntegral)
                   . (`divMod` (fromIntegral . SP.fromSing $ msing))
                   . F.getFinite

--should really go the freeze unfreeze route
--TODO generalize to map index to applicative functor, take what want? maybe even more?
randChooseGridCoordIndices :: forall g n. (RandomGen g, KnownNat n) =>
  g -> F.Finite (n + 1) -> [F.Finite n]
randChooseGridCoordIndices gen k = runST st
  where
    st :: (forall s. ST s [F.Finite n])
    st = do
      (v :: VGMS.MVector VM.MVector n s (F.Finite n)) <- VGMS.new
      forM_ (enumFrom minBound) (\i -> VGMS.write v i i) --TODO make better with lenses
      evalRand (shuffleFirstK v k) gen
      (vf :: VGS.Vector V.Vector n (F.Finite n)) <- VGS.freeze v
      return $ take (fromIntegral k) $ toList vf


packFiniteDefault :: (KnownNat n) => F.Finite n -> Integer -> F.Finite n
packFiniteDefault defFinite = fromMaybe defFinite . F.packFinite


getAdjacent :: forall n n'. (KnownNat n, KnownNat n') =>
  GridCoord n n' -> [GridCoord n n']
getAdjacent (r, c) = [(r', c')
                     | r' <- getAdjOneD r,
                       c' <- getAdjOneD c,
                       (r', c') /= (r, c)]
  where getAdjOneD x = [packFiniteDefault minBound (fromIntegral x - 1)
                        ..
                        packFiniteDefault maxBound (fromIntegral x + 1)]


prettyPrintTile :: BoardTile -> String
prettyPrintTile (BoardTile { isMine = isMn, numAdjMines = numAdjMns })
  | isMn = "*"
  | numAdjMns == 0 = " "
  | otherwise = show numAdjMns


mineToTileGrid :: (Functor f, Foldable f, ComonadStore s w) =>
  (s -> f s) -> w Bool -> w BoardTile
mineToTileGrid getAdj = extend mineGridToTile
  where
    countAdjMines = foldMap (\b -> if b then Sum 1 else Sum 0) . experiment getAdj
    mineGridToTile mineGrid =
          BoardTile
          { isMine = extract mineGrid
          , numAdjMines = fromIntegral . getSum $ countAdjMines mineGrid
          }



gridToList :: Grid n m a -> [[a]]
gridToList = VGS.toList . VGS.map VGS.toList . getCompose

--will dim markers
--eavl rand should probably be in thing before, sets up everything
--this will be buildboard
goWithBoardDims :: forall n n' m. (MonadReader GameSettings m, MonadIO m) =>
  STL.SNat n -> STL.SNat n' -> Integer -> m ()
goWithBoardDims sn sn' numMines =
  STL.withKnownNat sn
  $ STL.withKnownNat sn'
  -- $ STL.withKnownNat (sn SP.%* sn' SP.%+ (SP.sing :: STL.SNat 1))
  $ do
  --    gridLists = gridToList dg
  let numMinesF = packFiniteDefault maxBound numMines :: F.Finite (n * n' + 1)
  gr <- liftIO SR.getStdGen
  mineIndices <- flip evalRandT gr $ do
    gr' <- getSplit
    return $ randChooseGridCoordIndices gr' numMinesF
    --return $ (gridChooseK gr' numMinesF :: Grid n n' Bool)
  let mineCoords = L.view (L.from gridCoordIndex) <$> mineIndices
      (StoreT (Identity gt) _) = mineToTileGrid getAdjacent (store (`elem` mineCoords) (minBound, minBound) :: Store (Grid n n') Bool)
  liftIO $ traverse_ (putStrLn . unwords . map prettyPrintTile) (gridToList $ gt) -- map then sequence

go :: (MonadReader GameSettings m, MonadIO m) => m ()
go = do
  gs <- ask
  let bs = (boardSettings gs :: BoardSettings)
  go' (height bs) (width bs) (fromIntegral $ mines bs)
  where go' (SP.FromSing singHeight) (SP.FromSing singWidth) =
          goWithBoardDims singWidth singHeight

--TODO look at applicative parse to see where to put grid pretty print
--TODO have board settings and TypedNat board settings, make sure mines less
--bring printing out of main into go
main :: IO ()
main = do
  putStrLn "Enter dims:"
  (h :: Natural) <- readLn --should restrict to positive
  (w :: Natural) <- readLn
  putStrLn "Enter num mines:"
  (numMines :: Natural) <- readLn
  let gs = GameSettings {
        boardSettings = BoardSettings
        { height = h
        , width = w
        , mines = numMines
        }}
  runReaderT go gs
  putStrLn "FINISHED!"

