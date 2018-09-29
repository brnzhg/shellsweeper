{-# LANGUAGE
ScopedTypeVariables
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
-- import Data.Functor.Identity (runIdentity)
import qualified Data.Bifunctor as BF
import qualified Data.Functor.Rep as FR
import Data.Functor.Compose
import Control.Monad.ST (ST, runST)
import Control.Monad.IO.Class
import Control.Monad.Primitive (PrimMonad, PrimState, primToST)
import Control.Monad.Reader (ask, runReader, runReaderT, Reader, ReaderT, MonadReader)
import Numeric.Natural
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Finite as F

import qualified Data.Singletons.Prelude as SP
import qualified Data.Singletons.TypeLits as STL
import qualified Data.Vector.Generic.Sized as VGS
import qualified Data.Vector.Generic.Mutable.Sized as VGMS
import qualified Data.Vector.Generic.Base as VGB
import qualified Data.Vector.Generic.Mutable.Base as VGMB
--import GHC.Generics

import qualified System.Random as SR
import Control.Monad.Random (getRandomR, getRandom, MonadRandom, RandomGen, evalRandT, evalRand, getSplit)

import GHC.TypeLits

import qualified Control.Lens as L
import qualified Control.Lens.Iso as LI

import RandomFinite.Vector (shuffleFirstK)


type Grid rv cv (n :: Nat) (m :: Nat) =
  Compose (VGS.Vector rv n) (VGS.Vector cv m)

type GridCoord (n :: Nat) (m :: Nat) = (F.Finite n, F.Finite m)


data BoardShape = Rect Natural Natural

data GameSettings = GameSettings {
  boardShape :: BoardShape }

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


dumbGrid :: forall n m. (KnownNat n, KnownNat m) => Grid V.Vector V.Vector n m Integer
dumbGrid = Compose
  $ VGS.generate (\i -> VGS.generate (\j -> (fromIntegral i) * (fromIntegral . SP.fromSing $ numCols) + (fromIntegral j)))
  where numCols :: STL.SNat m
        numCols = SP.Sing
  --Compose $ VGS.generate (const $ VGS.generate fromIntegral)

gridToList :: Grid V.Vector V.Vector n m Integer -> [[Integer]]
gridToList = VGS.toList . VGS.map VGS.toList . getCompose


--TODO check withSomeSing withKnownNat work for regular Vector n
printRandomCoord :: forall m n n' a.
  (MonadRandom m, MonadIO m, KnownNat n, KnownNat n', Show a) =>
  Grid V.Vector V.Vector n n' a -> m ()
printRandomCoord g = do
  randnat <- getRandom
  let randCoord :: GridCoord n n'
      randCoord = L.view (L.from gridCoordIndex) randnat
  liftIO $ do
    putStrLn $ show randnat
    putStrLn $ show randCoord
    putStrLn $ show $ FR.index g randCoord


--will dim markers
--eavl rand should probably be in thing before, sets up everything
--this will be buildboard
goWithBoardDims :: forall n n' m. (MonadReader GameSettings m, MonadIO m) =>
  STL.SNat n -> STL.SNat n' -> m ()
goWithBoardDims sn sn' =
  STL.withKnownNat sn
  $ STL.withKnownNat sn'
  -- $ STL.withKnownNat (sn SP.%* sn' SP.%+ (SP.sing :: STL.SNat 1))
  $ do
  let dg = (dumbGrid :: Grid V.Vector V.Vector n n' Integer)
      gridLists = gridToList dg
  gr <- liftIO SR.getStdGen
  randCoordIndices <- evalRandT (do
    printRandomCoord dg
    gr' <- getSplit
    return $
      randChooseGridCoordIndices gr'
      (fromMaybe maxBound $ F.packFinite 4 :: F.Finite (n * n' + 1)))
    gr
  liftIO $ traverse_ (putStrLn . unwords . map show) gridLists -- map then sequence
  liftIO $ traverse_ (putStrLn . show) randCoordIndices

go :: (MonadReader GameSettings m, MonadIO m) => m ()
go = do
  gs <- ask
  let (Rect numRows numCols) = boardShape gs
  SP.withSomeSing numRows
    (\sn -> SP.withSomeSing numCols
            (\sn' -> goWithBoardDims sn sn'))

--TODO look at applicative parse to see where to put grid pretty print

--bring printing out of main into go
main :: IO ()
main = do
  putStrLn "Enter dims:"
  (numRows :: Natural) <- readLn
  (numCols :: Natural) <- readLn
  let gs = GameSettings {
        boardShape = Rect numRows numCols }
  runReaderT go gs
  putStrLn "FINISHED!"

