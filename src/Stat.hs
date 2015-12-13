module Stat where

import System.Fuse
import System.Posix.Files
import System.Posix (EpochTime, FileOffset)

dirStat :: FuseContext -> EpochTime -> FileStat
dirStat ctx t = FileStat {
  statEntryType = Directory
  , statFileMode = foldr1 unionFileModes
    [ ownerReadMode
    , ownerExecuteMode
    , groupReadMode
    , groupExecuteMode
    , otherReadMode
    , otherExecuteMode
    ]
    , statLinkCount = 2
    , statFileOwner = fuseCtxUserID ctx
    , statFileGroup = fuseCtxGroupID ctx
    , statSpecialDeviceID = 0
    , statFileSize = 4096
    , statBlocks = 1
    , statAccessTime = t
    , statModificationTime = t
    , statStatusChangeTime = t
  }

fileStat :: FuseContext -> EpochTime -> FileOffset -> FileStat
fileStat ctx t size = FileStat {
  statEntryType = RegularFile
  , statFileMode = foldr1 unionFileModes
    [ ownerReadMode
    , groupReadMode
    , otherReadMode
    , ownerWriteMode
    ]
  , statLinkCount = 1
  , statFileOwner = fuseCtxUserID ctx
  , statFileGroup = fuseCtxGroupID ctx
  , statSpecialDeviceID = 0
  , statFileSize = size
  , statBlocks = fromIntegral $ calcBlocks size
  , statAccessTime = t
  , statModificationTime = t
  , statStatusChangeTime = t
  }
  where
    calcBlocks s = case s `quotRem` 512 of
      (b, 0) -> b
      (b, _) -> b+1
