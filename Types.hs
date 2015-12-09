module Types where

import System.Posix.Types

type EntryName = String
type EntryDate = EpochTime
type EntrySize = FileOffset

data Entry = Dir
           | IncompleteFile
           | File (Maybe EntryDate) (Maybe EntrySize)
           deriving (Show, Eq, Ord)

betterEntry :: Entry -> Entry -> Entry
betterEntry IncompleteFile e = e
betterEntry e _ = e
