{-# LANGUAGE OverloadedStrings #-}
module FuseOps where

import HTTPFS
import Stat
import BufferedStream

import System.Fuse hiding (EntryType)
import System.Posix.Types
import Data.ByteString (ByteString)
import Data.Maybe

myGetFileStat :: FS -> FilePath -> IO (Either Errno FileStat)
myGetFileStat o f = do
  ctx <- getFuseContext
  if f == "/" || f == "." || f == ".."
    then return (Right $ dirStat ctx 0)
    else Right . entryToFileStat ctx <$> getHTTPEntry o f

myOpen :: FS -> FilePath -> OpenMode -> OpenFileFlags
       -> IO (Either Errno BufferedStream)
myOpen o p m _ = case m of
  ReadOnly -> do
    bf <- getHTTPContent o p
    return $ Right bf
  _ -> return $ Left ePERM

myRead :: FS -> FilePath -> BufferedStream -> ByteCount -> FileOffset
       -> IO (Either Errno ByteString)
myRead _ _ bf bc fo = Right <$> readBufferedStream bf bc fo

myRelease :: FilePath -> BufferedStream -> IO ()
myRelease _ = closeBufferedStream

myOpenDirectory :: FS -> FilePath -> IO Errno
myOpenDirectory _ _ = return eOK

defaultStats :: FuseContext -> [(FilePath, FileStat)]
defaultStats ctx = [(".", dirStat ctx 0), ("..", dirStat ctx 0)]

myReadDirectory :: FS -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
myReadDirectory o f = do
  ctx <- getFuseContext
  Right . (\l -> defaultStats ctx ++ map (fmap $ entryToFileStat ctx) l)
    <$> getHTTPDirectoryEntries o f

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

myFuseOperations :: FS -> FuseOperations BufferedStream
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
