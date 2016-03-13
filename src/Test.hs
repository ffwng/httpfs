{-# LANGUAGE OverloadedStrings #-}
module Main where

import FS
import TestingFS
import CachingFS

import System.Fuse.ByteString hiding (EntryType)
import Control.Exception
import Data.Monoid


traverseFS :: FS -> IO EntrySize
traverseFS fs = goDir "" where
  go p = do
    e <- getEntry fs Nothing p
    case e of
      File (FileStats (Just s) _) -> return s
      Dir -> goDir p
      _ -> return 0

  goDir p = do
    entries <- getDirectoryEntries fs p
    sizes <- mapM (\(n, _) -> go (p <> "/" <> n)) entries
    return (sum sizes)


main :: IO ()
main = do
  fs <- testingFS >>= addCache 600
  print =<< traverseFS fs

  {-let fuseArgs = ["/home/richard/remote", "-o", "direct_io", "-f"]
  progName <- getProgName
  fuseRun progName fuseArgs (myFuseOperations fs) httpExceptionHandler-}

httpExceptionHandler :: SomeException -> IO Errno
httpExceptionHandler e = case fromException e of
  Just NotFoundException -> return eNOENT
  Nothing -> print e >> return eFAULT
