module Types where

import System.Posix.Types

type EntryName = String
type EntryDate = EpochTime
type EntrySize = FileOffset

data EntryType = DirType | FileType
               deriving (Show, Eq, Ord)

data Entry = Dir
           | File EntryDate EntrySize
           deriving (Show, Eq, Ord)

entryType :: Entry -> EntryType
entryType Dir {} = DirType
entryType File {} = FileType

