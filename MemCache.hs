module MemCache where

import qualified Data.Map as M
import Data.IORef
import System.Posix.Types
import System.Posix.Time

data MemCache a b = MemCache EpochTime (IORef (M.Map a (EpochTime, b)))

newMemCache :: Int -> IO (MemCache a b)
newMemCache d = MemCache (fromIntegral d) <$> newIORef M.empty

insert :: Ord a => MemCache a b -> a -> b -> IO ()
insert (MemCache d ref) a b = do
  t <- epochTime
  modifyIORef ref $ M.insert a (t + d, b)

delete :: Ord a => MemCache a b -> a -> IO ()
delete (MemCache _ ref) a = modifyIORef ref $ M.delete a

query :: Ord a => MemCache a b -> a -> IO (Maybe b)
query mc@(MemCache _ ref) a = do
  m <- readIORef ref
  t <- epochTime
  let res = M.lookup a m
  case res of
    Nothing -> return Nothing
    Just (t', _) | t' < t -> delete mc a >> return Nothing
    Just (_, b) -> return (Just b)
