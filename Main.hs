module Main where

import FuseOps
import HTTPFS
import Common
import CommandLine

import Control.Monad
import Network.HTTP.Types
import Network.HTTP.Client
import Network.HTTP.Client.OpenSSL
import OpenSSL.Session
import System.Fuse hiding (EntryType)
import System.Environment
import Options.Applicative

checkServer :: FS -> IO ()
checkServer fs = do
  let req = (mkRequest fs "/") { method = methodHead }
  res <- httpNoBody req (manager fs)
  if responseStatus res /= status200
    then error $ "Error: " ++ show (responseStatus res)
    else return ()

mkContext :: Bool -> IO SSLContext
mkContext novalidate = do
  c <- context
  contextSetDefaultCiphers c
  contextSetCADirectory c "/etc/ssl/certs"
  when (not novalidate) $
    contextSetVerificationMode c $ VerifyPeer False False Nothing
  return c

main :: IO ()
main = withOpenSSL $ do
  let opts = info (helper <*> cmdArgs)
             ( fullDesc
               <> progDesc "A FUSE file system for webserver directory listings" )
  args <- execParser opts

  let settings = opensslManagerSettings $ mkContext novalidate
      novalidate = disableCertificateValidation args
      requestAdj = case basicAuth args of
        Nothing -> id
        Just (u, p) -> applyBasicAuth u p

  fs <- newFS requestAdj (baseUrl args) settings
  checkServer fs
  parse <- guessServer fs

  let fuseArgs = [mountPoint args]
  p <- getProgName
  fuseRun p fuseArgs (myFuseOperations $ stdOps parse fs) defaultExceptionHandler
