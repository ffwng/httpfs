module Main where

import FuseOps
import HTTPFS
import CommandLine
import Parser

import Control.Monad
import Network.HTTP.Types
import Network.HTTP.Client
import Network.HTTP.Client.OpenSSL
import OpenSSL.Session
import System.Fuse hiding (EntryType)
import System.Environment
import Options.Applicative hiding (Parser)
import Control.Exception
import qualified Data.ByteString.Char8 as B

checkServer :: FS -> IO ()
checkServer fs = do
  let req = (mkRequest fs "/") { method = methodHead }
  _ <- httpNoBody req (manager fs) -- throws exception on error
  return ()

mkContext :: Bool -> IO SSLContext
mkContext novalidate = do
  c <- context
  contextSetDefaultCiphers c
  contextSetCADirectory c "/etc/ssl/certs"
  unless novalidate $
    contextSetVerificationMode c $ VerifyPeer False False Nothing
  return c

mkFS :: IO SSLContext -> Maybe BasicAuth -> Parser -> String -> IO FS
mkFS ctx auth p url = do
  let settings = opensslManagerSettings ctx
      settingsWithAuth = case auth of
        Nothing -> settings
        Just (u, pw) -> settings { managerModifyRequest = return . applyBasicAuth u pw }

  req <- parseUrl url
  let reqPath = path req
      reqPath' = if B.last reqPath == '/' then B.init reqPath else reqPath
      mkReq fp = req { path = reqPath' <> fp }

  newFS mkReq p settingsWithAuth

main :: IO ()
main = withOpenSSL $ do
  let opts = info (helper <*> cmdArgs)
             ( fullDesc
               <> progDesc "A FUSE file system for webserver directory listings" )
  args <- execParser opts

  let ctx = mkContext $ disableCertificateValidation args
      p = xpathParser $ linkXPath args
  fs <- mkFS ctx (basicAuth args) p (baseUrl args)

  checkServer fs

  let ops = Ops (getHTTPDirectoryEntries fs) (getHTTPEntry fs) (getHTTPContent fs)

  let fuseArgs = mountPoint args : "-o" : "direct_io" : otherArgs args
  progName <- getProgName
  fuseRun progName fuseArgs (myFuseOperations ops) httpExceptionHandler

httpExceptionHandler :: SomeException -> IO Errno
httpExceptionHandler e = case fromException e of
  Just (StatusCodeException s _ _) | s == status404 -> return eNOENT
  Just _ -> return eINVAL
  Nothing -> print e >> return eFAULT
