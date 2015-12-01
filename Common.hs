{-# LANGUAGE OverloadedStrings #-}
module Common where

import FuseOps
import HTTPFS
import Types
import MemCache
import Parser

import Control.Monad (forM_)

stdOps :: Parser -> FS -> Ops
stdOps parser fs = Ops entries entry content where
  entries p = do
    bs <- getHTTPDirectoryHTML fs p
    l <- parseByteString parser bs
    forM_ l $ \(n, e) -> cacheBetter (cache fs) (combine p n) e
    return $ map (fmap $ either id entryType) l
  entry = getHTTPEntry fs
  content = getHTTPContent fs

combine :: FilePath -> FilePath -> FilePath
combine "" p = p
combine "/" p = '/':p
combine p1 p2 = p1 ++ '/':p2

cacheBetter :: Ord a => MemCache a (Either b c) -> a -> Either b c -> IO ()
cacheBetter mc a e@(Right _) = insert mc a e
cacheBetter mc a e@(Left _) = do
  cached <- query mc a
  case cached of
    Just (Right _) -> return ()
    _ -> insert mc a e
