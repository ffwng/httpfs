module MemCache (
  MemCache,
  newMemCache,
  insert, insertWith,
  delete,
  query
) where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM
import System.Posix.Types
import System.Posix.Time

data MemCache k a = MemCache EpochTime (TVar (M.Map k (EpochTime, a)))

newMemCache :: Int -> IO (MemCache k a)
newMemCache d = MemCache (fromIntegral d) <$> newTVarIO M.empty

insertWith :: Ord k => (a -> a -> a) -> MemCache k a -> k -> a -> IO ()
insertWith f (MemCache d ref) k a = do
  t <- epochTime
  let f' (tNew, aNew) (_, aOld) = (tNew, f aNew aOld)
  atomically . modifyTVar' ref $ M.insertWith f' k (t + d, a)

insert :: Ord k => MemCache k a -> k -> a -> IO ()
insert = insertWith const

delete :: Ord k => MemCache k a -> k -> IO ()
delete mc = atomically . deleteSTM mc

deleteSTM :: Ord k => MemCache k a -> k -> STM ()
deleteSTM (MemCache _ ref) = modifyTVar' ref . M.delete

query :: Ord k => MemCache k a -> k -> IO (Maybe a)
query mc@(MemCache _ ref) k = do
  t <- epochTime
  atomically $ do
    m <- readTVar ref
    case M.lookup k m of
      Nothing -> return Nothing
      Just (t', _) | t' < t -> deleteSTM mc k >> return Nothing
      Just (_, b) -> return (Just b)
