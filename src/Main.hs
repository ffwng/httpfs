{-# LANGUAGE OverloadedStrings #-}
module Main where

import FuseOps
import FS
import HTTPFS
import CachingFS
import CommandLine
import Parser

import Control.Monad
import Network.HTTP.Client
import Network.HTTP.Client.OpenSSL
import OpenSSL.Session
import System.Fuse.ByteString hiding (EntryType)
import System.Environment
import Options.Applicative hiding (Parser)
import Control.Exception
import qualified Data.ByteString.Char8 as B

checkServer :: FS -> IO ()
checkServer fs = void $ getDirectoryEntries fs "/" -- throws exception on error

mkContext :: Bool -> IO SSLContext
mkContext novalidate = do
  c <- context
  contextSetDefaultCiphers c
  contextSetCADirectory c "/etc/ssl/certs"
  unless novalidate $
    contextSetVerificationMode c $ VerifyPeer False False Nothing
  return c

logRequest :: Request -> IO ()
logRequest r = B.putStrLn $ method r <> " " <> path r <> case lookup "Range" (requestHeaders r) of
  Nothing -> B.empty
  Just range -> " (" <> range <> ")"

mkFS :: IO SSLContext -> Maybe BasicAuth -> Parser -> String -> Bool -> IO FS
mkFS ctx auth p url logR = do
  let settings = opensslManagerSettings ctx
      settingsWithAuth = case auth of
        Nothing -> settings
        Just (u, pw) -> settings { managerModifyRequest = return . applyBasicAuth u pw }
      settingsWithLog = if logR
        then settingsWithAuth { managerModifyRequest = \r -> logRequest r >> managerModifyRequest settingsWithAuth r }
        else settingsWithAuth

  req <- parseUrlThrow url
  let reqPath = path req
      reqPath' = if B.last reqPath == '/' then B.init reqPath else reqPath
      mkReq fp = req { path = reqPath' <> fp }

  newHTTPFS mkReq p settingsWithLog >>= addCache 600

main :: IO ()
main = withOpenSSL $ do
  let opts = info (helper <*> cmdArgs)
             ( fullDesc
               <> progDesc "A FUSE file system for webserver directory listings" )
  args <- execParser opts

  let ctx = mkContext $ disableCertificateValidation args
      p = xpathParser $ linkXPath args
  fs <- mkFS ctx (basicAuth args) p (baseUrl args) (logRequests args)

  checkServer fs

  let fuseArgs = mountPoint args : "-o" : "direct_io" : otherArgs args
  progName <- getProgName
  fuseRun progName fuseArgs (myFuseOperations fs) httpExceptionHandler

httpExceptionHandler :: SomeException -> IO Errno
httpExceptionHandler e = case fromException e of
  Just NotFoundException -> return eNOENT
  Nothing -> print e >> return eFAULT
