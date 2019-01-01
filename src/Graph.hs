{-# LANGUAGE
ScopedTypeVariables
, FlexibleContexts
#-}

module Graph (
unfoldDfs
, unfoldDfsM
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
import Control.Monad.Reader (ReaderT(..), ask, runReaderT)
import Control.Monad.State.Strict (StateT(..), get, put, lift, evalStateT)

unfoldDfsStep :: forall h s k. (Eq k, Hashable k, HTC.HashTable h) =>
  (k -> [k]) -> StateT (h s k (), [k]) (ST s) (Maybe k)
unfoldDfsStep f = do
  (ht, q) <- get
  q' <- lift $ flip dropWhileM q
        $ \x -> HTC.mutate ht x
                $ \x' -> (Just (), isJust x') --always insert (), return value (bool to stop drop)
  case q' of
    [] -> put (ht, []) >> (return Nothing)
    (x:xs) -> put (ht, f x ++ xs) >> (return $ Just x)

unfoldDfs :: forall k. (Eq k, Hashable k) =>
  (k -> [k]) -> k -> [k]
unfoldDfs f x = runST $ do
  ht <- HTB.new
  flip evalStateT (ht, [x])
    $ unfoldM
    $ unfoldDfsStep f

--TODO tell me there's an easier way to do this...
unfoldDfsStepM :: forall h s k m. (Eq k, Hashable k, HTC.HashTable h, Monad m) =>
  (k -> m [k]) -> StateT (h s k (), [k]) (STT s m) (Maybe k)
unfoldDfsStepM f = do
  (ht, q) <- get
  q' <- lift . liftST $ flip dropWhileM q
        $ \x -> HTC.mutate ht x
                $ \x' -> (Just (), isJust x')
  case q' of
    [] -> put (ht, []) >> (return Nothing)
    (x:xs) -> (lift . lift $ f x)
              >>= (\x' -> put (ht, x' ++ xs))
              >> (return $ Just x)

unfoldDfsM :: forall k m. (Eq k, Hashable k, Monad m) =>
  (k -> m [k]) -> k -> m [k]
unfoldDfsM f x = runSTT $ do
  ht <- liftST HTB.new
  flip evalStateT (ht, [x])
    $ unfoldM
    $ unfoldDfsStepM f


unfoldDfsStepPrim :: (Eq k, Hashable k, HTC.HashTable h, PrimMonad m) =>
  (k -> m [k]) -> ReaderT ((h (PrimState m) k (), STRef (PrimState m) [k])) m (Maybe k)
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

{-
unfoldDfsStepM :: forall h s k m. (Eq k, Hashable k, HTC.HashTable h, Monad m) =>
  (k -> m [k]) -> STRef s (h s k (), [k]) -> ST s (m (Maybe k))
unfoldDfsStepM f str = do
  undefined
  where
    writeQ [] r = readSTRef r >>= \(ht, _) -> writeSTRef r (ht, [])
    writeQ (h:t) r = readSTRef r >>= \(ht, _) -> 
    modifyStr :: STRef s (h s k (), [k]) -> ST s ()
    modifyStr r = do
      (ht, q) <- readSTRef r
      q' <- modifyQ ht q
      writeSTRef r (ht, q')
    modifyQ :: h s k () -> [k] -> ST s [k]
    modifyQ ht q =  flip dropWhileM q
                    $ \x -> HTC.mutate ht x
                            $ \x' -> (Just (), isJust x')
-}
