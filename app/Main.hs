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
, TypeFamilies
, GADTs #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Main where

import Data.Foldable
import Data.Traversable (sequence)
import Data.Maybe (fromMaybe, isJust, isNothing, catMaybes)
import Data.Monoid (Sum(..))
import Data.Functor.Identity (Identity(..))
--import qualified Data.Bifunctor as BF
import qualified Data.Functor.Rep as FR
--import Data.Functor.Compose
import Control.Monad ((<=<))
import Control.Monad.Loops (untilJust)
--import Control.Monad.ST (ST, runST)
import Control.Monad.Cont (ContT(..))
import Control.Monad.State.Lazy (StateT(..), get, put, lift, evalStateT)
import Control.Monad.IO.Class
--import Control.Monad.Primitive (PrimMonad, PrimState, primToST)
import Control.Monad.Reader (ask, runReader, runReaderT, Reader, ReaderT, MonadReader)
import Control.Monad.Trans.Maybe (MaybeT(..))

import Control.Arrow ((&&&))
import Numeric.Natural

import Text.Read (readMaybe)

import qualified Data.Vector as V
--import qualified Data.Vector.Mutable as VM
import qualified Data.Finite as F

import qualified Data.Singletons.Prelude as SP
import qualified Data.Singletons.TypeLits as STL
import qualified Data.Vector.Generic.Sized as VGS
--import qualified Data.Vector.Generic.Mutable.Sized as VGMS
--import GHC.Generics

import Data.Hashable (Hashable(..))

import qualified System.Random as SR
import Control.Monad.Random (MonadSplit, MonadRandom, RandomGen, evalRandT, evalRand, getSplit)

import GHC.TypeLits

import qualified Control.Lens as L
import qualified Control.Lens.Iso as LI

import qualified Data.Functor.RepB as FRB
import Game (MonadBoard(..))
import Board (SingleMineTile(..)
             , HasRepGetAdj(..)
             , HasBoardNumMines(..)
             , HasRepBoardEnv(..)
             , TileState(..)
             , RepBoardState(..)
             , HasTile(..)
             , HasMark(..))
import Grid (Grid(..), GridCoord(..), squareAdjacentCoords, gridToListOfList)


data GridBoardEnv (n :: Nat) (n' :: Nat) = GridBoardEnv {
  _getAdj :: GridCoord n n' -> [GridCoord n n']
  , _boardNumMines :: Natural
}

data SomeGridBoardEnv :: * where
  MkSomeGridBoardEnv :: SP.Sing n
                     -> SP.Sing n'
                     -> GridBoardEnv n n'
                     -> SomeGridBoardEnv

instance (KnownNat n, KnownNat n') =>
  HasRepGetAdj (Grid n n') (GridBoardEnv n n') where
  getAdj = _getAdj

--TODO RepBoardFunctor must be finite.. expose count and grid implements
instance (KnownNat n, KnownNat n') =>
  HasBoardNumMines (GridBoardEnv n n') where
  boardNumMines =  _boardNumMines

--TODO try IORef boardstate

promptSomeGridBoardEnv :: forall m. MonadIO m => m SomeGridBoardEnv
promptSomeGridBoardEnv = do
  liftIO $ putStrLn "Enter dims:"
  (height :: Natural) <- readDim
  (width :: Natural) <- readDim
  let boardSize = width * height
  liftIO $ putStrLn "Enter mines:"
  (mines :: Natural) <- readMines boardSize
  return $ go mines width height
  where
    go :: Natural -> Natural -> Natural -> SomeGridBoardEnv
    go mines (SP.FromSing (sw@STL.SNat)) (SP.FromSing (sh@STL.SNat)) =
      MkSomeGridBoardEnv sw sh $ GridBoardEnv { _getAdj = squareAdjacentCoords
                                              , _boardNumMines = mines
                                              }
    readDim :: (MonadIO m) => m Natural
    readDim = untilJust $ do
      maybeDim <- runMaybeT $ readLinePred (> 0)
      case maybeDim of
        Nothing -> (liftIO $ putStrLn "Dim must be >0")
        _ -> return ()
      return maybeDim
    readMines :: (MonadIO m) => Natural -> m Natural
    readMines boardSize = untilJust $ do
      maybeMines <- runMaybeT
                    $ readLinePred (\x -> (x > 0 && x < boardSize))
      case maybeMines of
        Nothing -> (liftIO $ putStrLn
                    $ "Mines must be 0< and <" ++ show boardSize)
        _ -> return ()
      return maybeMines
    readLinePred :: (Read a, MonadIO m) => (a -> Bool) -> MaybeT m a
    readLinePred pred = MaybeT . liftIO
                        $ (<$> getLine)
                        $ (\x -> if pred x then Just x else Nothing) <=< readMaybe


withGridBoardEnv :: forall r. SomeGridBoardEnv
  -> (forall n n'. (KnownNat n, KnownNat n') => GridBoardEnv n n' -> r)
  -> r
withGridBoardEnv (MkSomeGridBoardEnv (sw@STL.SNat) (sh@STL.SNat) gbe) f =
  f gbe


main :: IO ()
main = do
  gbe <- promptSomeGridBoardEnv
  withGridBoardEnv gbe (\env -> putStrLn
                                $ "Env has "
                                ++ show (_boardNumMines env)
                                ++ " mines")
  putStrLn "FINISHED!"

{-
goWithBoardDims :: forall n n' m. (MonadReader GameSettings m, MonadIO m) =>
  STL.SNat n -> STL.SNat n' -> Integer -> m ()
goWithBoardDims sn sn' numMines =
  STL.withKnownNat sn
  $ STL.withKnownNat sn'
  $ do
  let numMinesF = packFiniteDefault maxBound numMines :: F.Finite (n * n' + 1)
  gr <- liftIO SR.getStdGen
  mineIndices <- flip evalRandT gr $ do
    gr' <- getSplit
    return $ randChooseGridCoordIndices gr' numMinesF
  let mineCoords = L.view (L.from gridCoordIndex) <$> mineIndices
      storeMineGrid = store (`elem` mineCoords) (minBound, minBound) :: Store (Grid n n') Bool
      (StoreT (Identity gt) _) = mineToTileGrid getAdjacent storeMineGrid
  liftIO $ do
    traverse_ (putStrLn . unwords . map prettyPrintTile) $ gridToList gt -- map then sequence
    putStrLn "Enter point:"
    (y0 :: Natural) <- readLn
    (x0 :: Natural) <- readLn
    putStrLn "OK..."
    let parsedStartCoord = do
          fy0 <- F.packFinite . fromIntegral $ y0
          fx0 <- F.packFinite . fromIntegral $ x0
          return ((fy0, fx0) :: GridCoord n n')
    case parsedStartCoord of
      Nothing -> putStrLn "Invalid point"
      Just startCoord -> putStrLn $ unwords . fmap show $ firstDfsNonMine gt startCoord

go :: (MonadReader GameSettings m, MonadIO m) => m ()
go = do
  gs <- ask
  let bs = (boardSettings gs :: BoardSettings)
  go' (height bs) (width bs) (fromIntegral $ mines bs)
  where go' (SP.FromSing singHeight) (SP.FromSing singWidth) =
          goWithBoardDims singWidth singHeight

--bring printing out of main into go
main :: IO ()
main = do
  putStrLn "Enter dims:"
  (h :: Natural) <- readLn --should restrict to positive
  (w :: Natural) <- readLn
  putStrLn "Enter num mines:"
  (numMines :: Natural) <- readLn
  let gs = GameSettings {
        height = h
        , width = w
        , mines = numMines
        }
  runReaderT go gs
  putStrLn "FINISHED!"
-}

{-
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
  }

data GameSettings = GameSettings
  { boardSettings :: BoardSettings }

prettyPrintTile :: BoardTile -> String
prettyPrintTile (BoardTile { isMine = isMn, numAdjMines = numAdjMns })
  | isMn = "*"
  | numAdjMns == 0 = " "
  | otherwise = show numAdjMns

-}
