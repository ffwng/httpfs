{-# LANGUAGE OverloadedStrings #-}
module Common where

import FuseOps
import HTTPFS
import Types
import MemCache
import qualified Nginx as N
import qualified Apache as A

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List hiding (insert)
import Control.Monad (forM_)
import Text.XML.HXT.Core
import Network.HTTP.Types
import Network.HTTP.Client

stdOps :: (BL.ByteString -> IO [(String, Either EntryType Entry)]) -> FS -> Ops
stdOps parse fs = Ops entries entry content where
  entries p = do
    bs <- getHTTPDirectoryHTML fs p
    l <- parse bs
    forM_ l $ \(n, e) -> cacheBetter (cache fs) (combine p n) e
    return $ map (fmap $ either id entryType) l
  entry = getHTTPEntry fs
  content = getHTTPContent fs

guessServer :: FS -> IO (BL.ByteString -> IO [(FilePath, Either EntryType Entry)])
guessServer fs = do
  let req = mkRequest fs "/"
      req' = req { method = methodHead }
  res <- httpNoBody req' (manager fs)
  let parse = case lookup "Server" (responseHeaders res) of
        Just s | "nginx" `isInfixOf` B.unpack s -> N.parse
        _ -> A.parse
  return $ parseByteString parse

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

parseByteString :: IOSLA (XIOState ()) XmlTree c -> BL.ByteString -> IO [c]
parseByteString parse bs = do
  let bs' = BL.unpack bs
      doc = readString [withValidate no, withParseHTML yes, withWarnings no] bs'

  runX (doc >>> parse)
