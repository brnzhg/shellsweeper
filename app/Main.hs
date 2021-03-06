{-# LANGUAGE
ScopedTypeVariables
, GeneralizedNewtypeDeriving
, DuplicateRecordFields
, FlexibleContexts
, FlexibleInstances
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

import System.Random (StdGen(..), newStdGen)

import Data.Foldable
import Data.Traversable (sequence)
import Data.Maybe (fromMaybe, isJust, isNothing, catMaybes)
import Data.Monoid (Sum(..))
import Data.Functor.Identity (Identity(..))
--import qualified Data.Bifunctor as BF
import qualified Data.Functor.Rep as FR
--import Data.Functor.Compose
import Control.Monad (join, (<=<), when)
import Control.Monad.Base (MonadBase(..), liftBase)
import Control.Monad.Loops (untilJust)
import Control.Monad.ST (ST, runST, stToIO)
--import Control.Monad.Cont (ContT(..))
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class
import Control.Monad.Primitive (PrimMonad, PrimState, primToST, RealWorld)
import Control.Monad.Reader (ask, runReader, runReaderT, Reader, ReaderT, MonadReader)
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Control.Concurrent.STM as STM

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

--import Data.Hashable (Hashable(..))

import qualified System.Random as SR
import qualified Control.Monad.Random as MRND
import qualified Control.Monad.Trans.Random.Strict as MTRNDS
{-
(MonadSplit
                            , MonadRandom
                            , RandomGen
                            , evalRandT
                            , evalRand
                            , getSplit)
-}

import GHC.TypeLits

import qualified Control.Lens as L
import qualified Control.Lens.Iso as LI

import qualified Data.Functor.RepB as FRB
import Board2 (SingleMineTile(..)
              , TileState(..)
              , BoardSum(..)
              , HasBoardGetAdj(..)
              , HasBoardNumMines(..)
              , HasBoardEnv(..)
              , HasTile(..)
              , HasMark(..)
              , MonadBoardState(..)
              , getEmptyBoardSum
              , getBoardNumSpaces)
import Grid (MGrid(..)
            , Grid(..)
            , GridCoord(..)
            , MGridBoardState(..)
            , MGridBoardReader(..)
            , squareAdjacentCoords
            , gridToListOfList
            , singleMineMGridFromSwapPairs
            , randomSingleMineMGrid
            , singleMineTileGridFromMines
            , singleMineMGridBoardStateFromTiles
            , randomSingleMineMGridBoardState
            , gridToVecOfVec
            , gridToListOfList)
import ChooseFinite (indexSwapPairsChooseK)

--TODO split gamestate so it has the TVar, and other stuff in it
data GameEnv be gs = GameEnv { _startSeed :: !StdGen
                             , _currentSeed :: !StdGen
                             , _boardEnv :: !be
                             , _gameState :: !gs
                             }

data GridBoardEnv (n :: Nat) (n' :: Nat) = GridBoardEnv {
  _getAdj :: !(GridCoord n n' -> [GridCoord n n'])
  , _boardNumMines :: !Natural
}

data SomeGridBoardEnv :: * where
  MkSomeGridBoardEnv :: SP.Sing n
                     -> SP.Sing n'
                     -> GridBoardEnv n n'
                     -> SomeGridBoardEnv

data MGridGameState n n' s tl mrk = MGridGameState {
  _boardState :: !(STM.TVar (MGridBoardState n n' s tl mrk))
  }

type MGridGameEnv (n :: Nat) (n' :: Nat) s tl mrk =
  GameEnv (GridBoardEnv n n') (MGridGameState n n' s tl mrk)


newtype MGridGame (n :: Nat) (n' :: Nat) tl mrk a =
  MGridGame { unMGridGame :: ReaderT (MGridGameEnv n n' RealWorld tl mrk) IO a }
  deriving (Functor, Applicative, Monad, MonadReader (MGridGameEnv n n' RealWorld tl mrk), MonadIO)


instance (KnownNat n, KnownNat n') =>
  HasBoardGetAdj (GridCoord n n') (GridBoardEnv n n') where
  getAdj = _getAdj

instance (KnownNat n, KnownNat n') =>
  HasBoardNumMines (GridBoardEnv n n') where
  boardNumMines = _boardNumMines

--TODO why do i need this? because of the constraints?
instance (KnownNat n, KnownNat n') =>
  HasBoardEnv (GridCoord n n') (GridBoardEnv n n')


--TODO make better with lenses
withMGridBoardReader :: (KnownNat n, KnownNat n', HasTile tl, HasMark mrk) =>
  MGridBoardReader n n' RealWorld tl mrk a -> MGridGame n n' tl mrk a
withMGridBoardReader g = do
  env <- ask
  liftIO
    . join
    . STM.atomically
    . fmap (\e -> stToIO
             $ (runReaderT . unMGridBoardReader) g e)
    . STM.readTVar
    . _boardState
    $ _gameState env

--TODO this shit can def be generalized, dont need all these concrete types
randomSingleMineMGridGameEnv :: (KnownNat n, KnownNat n', HasMark mrk) =>
  StdGen
  -> GridBoardEnv n n'
  -> MTRNDS.RandT StdGen IO (MGridGameEnv n n' RealWorld SingleMineTile mrk)
randomSingleMineMGridGameEnv strtSeed be = do
  currSeed <- MTRNDS.liftRandT (\g -> return (g, g))
  bsStart <- flip runReaderT be randomSingleMineMGridBoardState
  bs <- lift $ STM.newTVarIO bsStart
  return $ GameEnv { _startSeed = strtSeed
                   , _currentSeed = currSeed
                   , _boardEnv = be
                   , _gameState = MGridGameState { _boardState = bs }
                   }


promptSeed :: IO StdGen
promptSeed = do
  ln <- getLine
  if ln == []
    then return $ read ln
    else genNewSeed

genNewSeed :: IO StdGen
genNewSeed = do
  g <- newStdGen
  putStrLn $ "Generated seed: " ++ show g
  return g

promptSomeGridBoardEnv :: IO SomeGridBoardEnv
promptSomeGridBoardEnv = do
  putStrLn "Enter dims:"
  (height :: Natural) <- readDim
  (width :: Natural) <- readDim
  let boardSize = width * height
  putStrLn "Enter mines:"
  (mines :: Natural) <- readMines boardSize
  return $ makeSomeGridBoardEnv mines width height
  where
    readDim = untilJust $ do
      maybeDim <- runMaybeT $ readLinePred (> 0)
      when (isNothing maybeDim) $ putStrLn "Dim must be >0"
      return maybeDim
    readMines boardSize = untilJust $ do
      maybeMines <- runMaybeT
                    $ readLinePred (\x -> (x > 0 && x < boardSize))
      when (isNothing maybeMines)
        $ putStrLn
        $ "Mines must be 0< and <" ++ show boardSize
      return maybeMines
    readLinePred pred = MaybeT
                        $ (<$> getLine)
                        $ (\x -> if pred x then Just x else Nothing)
                        <=< readMaybe

makeSomeGridBoardEnv :: Natural -> Natural -> Natural -> SomeGridBoardEnv
makeSomeGridBoardEnv mines (SP.FromSing (sw@STL.SNat)) (SP.FromSing (sh@STL.SNat)) =
  MkSomeGridBoardEnv sw sh $ GridBoardEnv { _getAdj = squareAdjacentCoords
                                          , _boardNumMines = mines
                                          }

withGridBoardEnv :: forall r. SomeGridBoardEnv
  -> (forall n n'. (KnownNat n, KnownNat n') => GridBoardEnv n n' -> r)
  -> r
withGridBoardEnv (MkSomeGridBoardEnv (sw@STL.SNat) (sh@STL.SNat) gbe) f =
  f gbe


--IO action to generate random and create a board state

--TODO haskeline to parse command,
-- out of game (display current settings)
-- 1 change board type, adj rules
-- 2 change dims and num mines
-- 3 change seed
-- play game

-- in game

main :: IO ()
main = do
  gbe <- promptSomeGridBoardEnv
  withGridBoardEnv gbe (\env -> putStrLn
                                $ "Env has "
                                ++ show (_boardNumMines env)
                                ++ " mines")
  putStrLn "FINISHED!"

{-
prettyPrintTile :: BoardTile -> String
prettyPrintTile (BoardTile { isMine = isMn, numAdjMines = numAdjMns })
  | isMn = "*"
  | numAdjMns == 0 = " "
  | otherwise = show numAdjMns

-}
