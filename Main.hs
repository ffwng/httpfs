{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types
import FuseOps
import qualified Nginx as N
import qualified Apache as A
import HTTPFS
import MemCache

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Network.Connection
import System.Fuse hiding (EntryType)
import System.Environment
import Data.Default
import Data.Foldable (forM_)
import Data.List (isInfixOf)
import Text.XML.HXT.Core
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

stdOps :: (BL.ByteString -> IO [(String, Either EntryType Entry)]) -> FS -> Ops
stdOps parse fs = Ops entries entry content where
  entries p = do
    html <- getHTTPDirectoryHTML fs p
    case html of
      Left e -> return $ Left e
      Right bs -> do
        l <- parse bs
        forM_ l $ \(n, e) -> cacheBetter (cache fs) (p ++ '/':n) e
        return . Right $ map (fmap $ either id entryType) l
  entry = getHTTPEntry fs
  content = getHTTPContent fs

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

guessServer :: FS -> IO (BL.ByteString -> IO [(FilePath, Either EntryType Entry)])
guessServer fs = do
  let req = mkRequest fs "/"
      req' = req { method = methodHead }
  res <- httpNoBody req' (manager fs)
  let parse = case lookup "Server" (responseHeaders res) of
        Just s | "nginx" `isInfixOf` B.unpack s -> N.parse
        _ -> A.parse
  return $ parseByteString parse

stdFS :: String -> IO FS
stdFS baseurl = do
  let settings = mkManagerSettings
                 (def { settingDisableCertificateValidation = True })
                 Nothing
  newFS baseurl settings

main :: IO ()
main = do
  (url:rest) <- getArgs
  fs <- stdFS url
  parse <- guessServer fs
  withArgs rest $
    fuseMain (myFuseOperations $ stdOps parse fs) defaultExceptionHandler
