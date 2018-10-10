{-# LANGUAGE
ScopedTypeVariables
, FlexibleContexts
#-}

module Graph (
unfoldDfs
) where

--import Control.Monad.State (MonadState)
import Data.Maybe (isJust)
import Control.Monad.Loops (unfoldM, dropWhileM)
import Control.Monad.ST (ST, runST)
import Control.Monad.State.Lazy (StateT(..), get, put, lift, evalStateT)
import Data.Hashable (Hashable(..))
import qualified Data.HashTable.Class as HTC
import qualified Data.HashTable.ST.Basic as HTB


unfoldDfsStep :: forall h s k. (Eq k, Hashable k, HTC.HashTable h) =>
  (k -> [k]) -> StateT (h s k (), [k]) (ST s) (Maybe k)
unfoldDfsStep f = do
  (ht, q) <- get
  q' <- lift $ flip dropWhileM q
        $ \x -> HTC.mutate ht x
                $ \x' -> (Just (), isJust x')
  case q' of
    [] -> put (ht, []) >> (return Nothing)
    (x:xs) -> put (ht, f x ++ xs) >> (return $ Just x)


--TODO cand do with MonadPlus version, kinda cool i guess (zipper dream?)
unfoldDfs :: forall k. (Eq k, Hashable k) =>
  (k -> [k]) -> k -> [k]
unfoldDfs f x = runST $ do
  ht <- HTB.new
  flip evalStateT (ht, [x])
    $ unfoldM
    $ unfoldDfsStep f
