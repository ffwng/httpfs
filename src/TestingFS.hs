{-# LANGUAGE OverloadedStrings #-}
module TestingFS where

import Data.Monoid
import qualified Data.ByteString.Char8 as B

import FS
import BufferedStream

testingFS :: IO FS
testingFS = return FS {
  getEntry = \_ -> return . testingEntry,
  getDirectoryEntries = return . testingDirContent,
  getFileContent = \_ _ -> makeBufferedStream (\_ -> return (return B.empty)) (return ())
  }

testingEntry :: B.ByteString -> Entry
testingEntry p = case splitPath p of
  [] -> Dir DirectoryStats
  l -> case last l of
    p' | B.head p' == 'd' -> Dir DirectoryStats
    _ -> File $ FileStats (Just 1) (Just 0)

testingDirContent :: B.ByteString -> [(EntryName, EntryType)]
testingDirContent p = if length (splitPath p) >= 3 then files else [("dir" <> B.pack (show i), DirType) | i <- [1..15::Int]] ++ files where
  files = [("file" <> B.pack (show i), FileType) | i <- [1..5::Int]]

splitPath :: B.ByteString -> [B.ByteString]
splitPath = drop 1 . B.split '/'
