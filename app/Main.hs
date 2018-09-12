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
, TypeApplications #-}

module Main where

import Data.Foldable
-- import Data.Functor.Identity (runIdentity)
import Data.Functor.Compose
import Control.Monad.Reader (ask, runReader, Reader, MonadReader)
import Numeric.Natural
import qualified Data.Vector as V
import qualified Data.Finite as F
import qualified Data.Singletons as S
import qualified Data.Singletons.Prelude as SP
import qualified Data.Singletons.TypeLits as STL
import qualified Data.Vector.Generic.Sized as VGS
--import GHC.Generics
import GHC.TypeLits

import Lib --TODO temp module

data BoardShape = Rect Natural Natural


type Grid rv cv (n :: Nat) (m :: Nat) =
  Compose (VGS.Vector rv n) (VGS.Vector cv m)

data GameSettings = GameSettings {
  boardShape :: BoardShape }




dumbGrid :: STL.SNat n -> STL.SNat m -> Grid V.Vector V.Vector n m Integer
dumbGrid sn sm = STL.withKnownNat sn
                 $ STL.withKnownNat sm
                 $ Compose $ VGS.generate (const $ VGS.generate fromIntegral)

gridToList :: (KnownNat n, KnownNat m) => Grid V.Vector V.Vector n m Integer -> [[Integer]]
gridToList g = VGS.toList . VGS.map VGS.toList . getCompose $ g


makeDumbGrid :: STL.SNat n -> STL.SNat m -> [[Integer]]
makeDumbGrid sn sm = STL.withKnownNat sn
                     $ STL.withKnownNat sm
                     $ gridToList $ dumbGrid sn sm 
--TODO check withSomeSing withKnownNat work for regular Vector n


go :: (MonadReader GameSettings m) => m [[Integer]]
go = do
  gs <- ask
  let (Rect numRows numCols) = boardShape gs
      gridLists =
        S.withSomeSing numRows
        (\sn -> S.withSomeSing numCols
                (\sm -> makeDumbGrid sn sm))
  return gridLists


main :: IO ()
main = do
  putStrLn "whattup"
  (numRows :: Natural) <- readLn
  (numCols :: Natural) <- readLn
  let gs = GameSettings {
        boardShape = Rect numRows numCols }
      gridLists = runReader (go :: Reader GameSettings [[Integer]]) gs -- need runReaderT if more layers
  traverse_ (putStrLn . unwords . map show) gridLists -- map then sequence
  putStrLn "FINISHED!"
