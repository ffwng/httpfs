{-# LANGUAGE OverloadedStrings #-}
module FuseOps where

import FS
import Stat
import BufferedStream

import System.Fuse.ByteString hiding (EntryType)
import System.Posix.Types
import Data.ByteString (ByteString)
import Data.Maybe

myGetFileStat :: FS -> ByteString -> IO (Either Errno FileStat)
myGetFileStat fs p = do
  ctx <- getFuseContext
  if p == "/" || p == "." || p == ".."
    then return (Right $ dirStat ctx 0)
    else Right . entryToFileStat ctx <$> getEntry fs Nothing p

myOpen :: FS -> ByteString -> OpenMode -> OpenFileFlags
       -> IO (Either Errno BufferedStream)
myOpen fs p m _ = case m of
  ReadOnly -> do
    bf <- getFileContent' fs p
    return $ Right bf
  _ -> return $ Left ePERM

myRead :: FS -> ByteString -> BufferedStream -> ByteCount -> FileOffset
       -> IO (Either Errno ByteString)
myRead _ _ bf bc fo = Right <$> readBufferedStream bf bc fo

myRelease :: ByteString -> BufferedStream -> IO ()
myRelease _ = closeBufferedStream

myOpenDirectory :: FS -> ByteString -> IO Errno
myOpenDirectory _ _ = return eOK

defaultStats :: FuseContext -> [(ByteString, FileStat)]
defaultStats ctx = [(".", dirStat ctx 0), ("..", dirStat ctx 0)]

myReadDirectory :: FS -> ByteString -> IO (Either Errno [(ByteString, FileStat)])
myReadDirectory fs f = do
  ctx <- getFuseContext
  Right . (\l -> defaultStats ctx ++ map (fmap $ entryTypeToFileStat ctx) l)
    <$> getDirectoryEntries fs f

getFileSystemStats :: ByteString -> IO (Either Errno FileSystemStats)
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
  where go (Dir _) = dirStat ctx 0
        go (File stats) = fStat stats

        fStat (FileStats s t) = fileStat ctx (fromMaybe 0 t) (fromMaybe 0 s)

entryTypeToFileStat :: FuseContext -> EntryType -> FileStat
entryTypeToFileStat ctx = entryToFileStat ctx . trans
  where trans DirType = Dir DirectoryStats
        trans FileType = File (FileStats Nothing Nothing)
