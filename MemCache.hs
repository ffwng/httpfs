module MemCache where

import qualified Data.Map.Strict as M
import Data.IORef
import System.Posix.Types
import System.Posix.Time

data MemCache k a = MemCache EpochTime (IORef (M.Map k (EpochTime, a)))

newMemCache :: Int -> IO (MemCache k a)
newMemCache d = MemCache (fromIntegral d) <$> newIORef M.empty

insertWith :: Ord k => MemCache k a -> (a -> a -> a) -> k -> a -> IO ()
insertWith (MemCache d ref) f k a = do
  t <- epochTime
  let f' (tNew, aNew) (_, aOld) = (tNew, f aNew aOld)
  modifyIORef ref $ M.insertWith f' k (t + d, a)

insert :: Ord k => MemCache k a -> k -> a -> IO ()
insert mc = insertWith mc const

delete :: Ord k => MemCache k a -> k -> IO ()
delete (MemCache _ ref) = modifyIORef ref . M.delete

query :: Ord k => MemCache k a -> k -> IO (Maybe a)
query mc@(MemCache _ ref) k = do
  m <- readIORef ref
  t <- epochTime
  let res = M.lookup k m
  case res of
    Nothing -> return Nothing
    Just (t', _) | t' < t -> delete mc k >> return Nothing
    Just (_, b) -> return (Just b)
