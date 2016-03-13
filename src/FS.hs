module FS where

import Data.ByteString (ByteString)
import Control.Exception
import System.Posix.Types

import BufferedStream

type EntryName = ByteString
type EntryDate = EpochTime
type EntrySize = FileOffset

data DirectoryStats = DirectoryStats
  deriving (Show, Eq, Ord)

data FileStats = FileStats (Maybe EntrySize) (Maybe EntryDate)
  deriving (Show, Eq, Ord)

data Entry = Dir DirectoryStats | File FileStats
  deriving (Show, Eq, Ord)

data EntryType = DirType | FileType
  deriving (Show, Eq, Ord)


data NotFoundException = NotFoundException
  deriving (Show, Eq, Ord)

instance Exception NotFoundException


data FS = FS {
  getEntry :: Maybe EntryType -> ByteString -> IO Entry,
  getDirectoryEntries :: ByteString -> IO [(EntryName, EntryType)],
  getFileContent :: (FileStats -> IO ()) -> ByteString -> IO BufferedStream
  }

getFileContent' :: FS -> ByteString -> IO BufferedStream
getFileContent' fs = getFileContent fs (const $ return ())
