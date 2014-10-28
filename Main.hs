module Main where

import FuseOps
import HTTPFS
import Common
import CommandLine

import Network.HTTP.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.Connection
import System.Fuse hiding (EntryType)
import System.Environment
import Data.Default
import Options.Applicative

checkServer :: FS -> IO ()
checkServer fs = do
  let req = (mkRequest fs "/") { method = methodHead }
  res <- httpNoBody req (manager fs)
  if responseStatus res /= status200
    then error $ "Error: " ++ show (responseStatus res)
    else return ()      

main :: IO ()
main = do
  let opts = info (helper <*> cmdArgs)
             ( fullDesc
               <> progDesc "A FUSE file system for webserver directory listings" )
  args <- execParser opts

  let tlsSettings = if disableCertificateValidation args
                    then def { settingDisableCertificateValidation = True }
                    else def
      settings = mkManagerSettings tlsSettings Nothing
      requestAdj = case basicAuth args of
        Nothing -> id
        Just (u, p) -> applyBasicAuth u p

  fs <- newFS requestAdj (baseUrl args) settings
  checkServer fs
  parse <- guessServer fs

  let fuseArgs = [mountPoint args]
  p <- getProgName
  fuseRun p fuseArgs (myFuseOperations $ stdOps parse fs) defaultExceptionHandler
