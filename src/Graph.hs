{-# LANGUAGE
ScopedTypeVariables
, FlexibleContexts
#-}

module Graph (
unfoldDfs
, unfoldDfsPrim
, unfoldDfsSTT
) where

--import Control.Monad.State (MonadState)
--import Data.STRef (STRef(..), readSTRef, writeSTRef, modifySTRef')
import Data.STRef
import Data.Hashable (Hashable(..))
import qualified Data.HashTable.Class as HTC
import qualified Data.HashTable.ST.Basic as HTB
import Data.Maybe (isJust)
import Control.Monad.Loops (unfoldM, dropWhileM, iterateUntilM)
import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Trans (STT, runSTT)
import Control.Monad.ST.Trans.Internal (liftST)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.Reader (ReaderT(..), ask, runReaderT, lift)

unfoldDfsStep :: forall h s k. (Eq k, Hashable k, HTC.HashTable h) =>
  (k -> [k]) -> ReaderT (h s k (), STRef s [k]) (ST s) (Maybe k)
unfoldDfsStep f = do
  (ht, qref) <- ask
  lift $ do
    q <- readSTRef qref
    q' <- flip dropWhileM q
          $ \x -> HTC.mutate ht x
                  $ \x' -> (Just (), isJust x') --always insert (), return value (bool to stop drop)
    case q' of
      [] -> writeSTRef qref [] >> (return Nothing)
      (x:xs) -> (writeSTRef qref $ f x ++ xs) >> (return $ Just x)

unfoldDfs :: forall k. (Eq k, Hashable k) =>
  (k -> [k]) -> k -> [k]
unfoldDfs f x = runST $ do
  ht <- HTB.new
  qref <- newSTRef [x]
  flip runReaderT (ht, qref) . unfoldM $ unfoldDfsStep f

unfoldDfsStepPrim :: (Eq k, Hashable k, HTC.HashTable h, PrimMonad m) =>
  (k -> m [k])
  -> ReaderT (h (PrimState m) k (), STRef (PrimState m) [k]) m (Maybe k)
unfoldDfsStepPrim f = do
  (ht, qref) <- ask
  q' <- lift . stToPrim $ do
    q <- readSTRef $ qref
    flip dropWhileM q
      $ \x -> HTC.mutate ht x
              $ \x' -> (Just (), isJust x')
  lift $ case q' of
           [] -> (stToPrim $ writeSTRef qref [])
                 >> return Nothing
           (x:xs) -> f x
                     >>= (\h -> stToPrim . writeSTRef qref $ h ++ xs)
                     >> return (Just x)

unfoldDfsPrim :: forall k m. (Eq k, Hashable k, PrimMonad m) =>
  (k -> m [k]) -> k -> m [k]
unfoldDfsPrim f x = stToPrim envST
                    >>= (\env -> flip runReaderT env . unfoldM
                                 $ unfoldDfsStepPrim f)
  where
    envST = do
      ht <- HTB.new
      qref <- newSTRef [x]
      return (ht, qref)

unfoldDfsStepSTT :: (Eq k, Hashable k, HTC.HashTable h, Monad m) =>
  (k -> m [k]) -> ReaderT (h s k (), STRef s [k]) (STT s m) (Maybe k)
unfoldDfsStepSTT f = do
  (ht, qref) <- ask
  lift $ do
    q' <- liftST $ do
      q <- readSTRef qref
      flip dropWhileM q
        $ \x -> HTC.mutate ht x
                $ \x' -> (Just (), isJust x')
    case q' of
      [] -> (liftST $ writeSTRef qref []) >> (return Nothing)
      (x:xs) -> (lift $ f x)
                >>= (\x' -> liftST . writeSTRef qref $ x' ++ xs)
                >> (return $ Just x)

unfoldDfsSTT :: forall k s m. (Eq k, Hashable k, Monad m) =>
  (k -> m [k]) -> k -> STT s m [k]
unfoldDfsSTT f x = liftST envST
                   >>= (\env -> flip runReaderT env . unfoldM $ unfoldDfsStepSTT f)
  where
    envST = do
      ht <- HTB.new
      qref <- newSTRef [x]
      return (ht, qref)
