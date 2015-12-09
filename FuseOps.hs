{-# LANGUAGE OverloadedStrings #-}
module FuseOps where

import Types
import Stat
import BufferedStream

import System.Fuse hiding (EntryType)
import System.Posix.Types
import Data.ByteString (ByteString)
import Data.Maybe

data Ops = Ops
           { getEntries :: FilePath -> IO [(EntryName, Entry)]
           , getEntry :: FilePath -> IO Entry
           , getContent :: FilePath -> IO BufferedStream
           }

myGetFileStat :: Ops -> FilePath -> IO (Either Errno FileStat)
myGetFileStat o f = do
  ctx <- getFuseContext
  if f == "/" || f == "." || f == ".."
    then return (Right $ dirStat ctx 0)
    else Right . entryToFileStat ctx <$> getEntry o f

myOpen :: Ops -> FilePath -> OpenMode -> OpenFileFlags
       -> IO (Either Errno BufferedStream)
myOpen o p m _ = case m of
  ReadOnly -> do
    bf <- getContent o p
    return $ Right bf
  _ -> return $ Left ePERM

myRead :: Ops -> FilePath -> BufferedStream -> ByteCount -> FileOffset
       -> IO (Either Errno ByteString)
myRead _ _ bf bc fo = Right <$> readBufferedStream bf bc fo

myRelease :: FilePath -> BufferedStream -> IO ()
myRelease _ = closeBufferedStream

myOpenDirectory :: Ops -> FilePath -> IO Errno
myOpenDirectory _ _ = return eOK

defaultStats :: FuseContext -> [(FilePath, FileStat)]
defaultStats ctx = [(".", dirStat ctx 0), ("..", dirStat ctx 0)]

myReadDirectory :: Ops -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
myReadDirectory o f = do
  ctx <- getFuseContext
  Right . (\l -> defaultStats ctx ++ map (fmap $ entryToFileStat ctx) l)
    <$> getEntries o f

getFileSystemStats :: String -> IO (Either Errno FileSystemStats)
getFileSystemStats _ = return $ Right FileSystemStats
  { fsStatBlockSize = 512
  , fsStatBlockCount = 1
  , fsStatBlocksFree = 1
  , fsStatBlocksAvailable = 1
  , fsStatFileCount = 5 -- IS THIS CORRECT?
  , fsStatFilesFree = 10 -- WHAT IS THIS?
  , fsStatMaxNameLength = 255 -- SEEMS SMALL?
  }

myFuseOperations :: Ops -> FuseOperations BufferedStream
myFuseOperations o = defaultFuseOps
                     { fuseGetFileStat = myGetFileStat o
                     , fuseOpen = myOpen o
                     , fuseRead = myRead o
                     , fuseRelease = myRelease
                     , fuseOpenDirectory = myOpenDirectory o
                     , fuseReadDirectory = myReadDirectory o
                     , fuseGetFileSystemStats = getFileSystemStats
                     }

entryToFileStat :: FuseContext -> Entry -> FileStat
entryToFileStat ctx = go
  where go Dir = dirStat ctx 0
        go IncompleteFile = fStat Nothing Nothing
        go (File t s) = fStat t s

        fStat t s = fileStat ctx (fromMaybe 0 t) (fromMaybe 0 s)
