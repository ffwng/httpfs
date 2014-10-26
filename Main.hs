module Main where

import Types
import FuseOps
import qualified Nginx as N
import HTTPFS
import MemCache

import Network.HTTP.Client.TLS
import Network.Connection
import System.Fuse hiding (EntryType)
import System.Environment
import Data.Default
import Data.Foldable (forM_)

nginxOps :: FS -> Ops
nginxOps fs = Ops entries entry content where
  entries p = do
    html <- getHTTPDirectoryHTML fs p
    case html of
      Left e -> return $ Left e
      Right bs -> do
        l <- N.parseByteString bs
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
  withArgs rest $
    fuseMain (myFuseOperations $ nginxOps fs) defaultExceptionHandler
