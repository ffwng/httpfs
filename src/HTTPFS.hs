{-# LANGUAGE TupleSections, OverloadedStrings #-}
module HTTPFS (
  newHTTPFS
) where

import FS
import BufferedStream
import Parser

import Data.Monoid
import Data.Word
import Data.IORef
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as B8
import Control.Monad
import Control.Exception
import Network.HTTP.Types
import Network.HTTP.Client
import Network.URI hiding (path, query)
import Text.Read
import System.Posix (EpochTime)
import Foreign.C (CTime(..))

data HTTPFS = HTTPFS {
  mkRequest :: B.ByteString -> Request,
  manager :: Manager,
  entryParser :: Parser
  }

newHTTPFS :: (B.ByteString -> Request) -> Parser -> ManagerSettings -> IO FS
newHTTPFS mkReq p manset = do
  let mkReq' = mkReq . B8.fromString . encode . B8.toString
      encode = escapeURIString (\c -> isUnreserved c || c == '/')
  man <- newManager manset { managerWrapException = exceptionWrapper }
  let fs = HTTPFS mkReq' man p
  return FS {
      getEntry = getHTTPEntry fs,
      getDirectoryEntries = getHTTPDirectoryEntries fs,
      getFileContent = getHTTPContent fs
    }

exceptionWrapper :: request -> IO a -> IO a
exceptionWrapper _ = handleJust wrap throwIO where
  wrap (HttpExceptionRequest _ StatusCodeException{}) = Just NotFoundException
  wrap _ = Nothing


getHTTPDirectoryEntries :: HTTPFS -> B.ByteString -> IO [(EntryName, EntryType)]
getHTTPDirectoryEntries fs p = do
    let p' = mkDir p
    res <- httpLbs (mkRequest fs p') (manager fs)
    parseByteString (entryParser fs) (responseBody res)

getHTTPEntry :: HTTPFS -> Maybe EntryType -> B.ByteString -> IO Entry
getHTTPEntry fs (Just FileType) p = File <$> getHTTPFileStats fs p
getHTTPEntry fs _ p = (Dir <$ getHTTPDirectoryStats fs p) `catch` \NotFoundException -> File <$> getHTTPFileStats fs p

getHTTPDirectoryStats :: HTTPFS -> B.ByteString -> IO ()
getHTTPDirectoryStats fs p = void $ requestHead fs (mkDir p)

getHTTPFileStats :: HTTPFS -> B.ByteString -> IO FileStats
getHTTPFileStats fs p = parseFileStats <$> requestHead fs p

getHTTPContent :: HTTPFS -> (FileStats -> IO ()) -> B.ByteString -> IO BufferedStream
getHTTPContent fs newStats p = do
  closeAct <- newIORef (return ())

  let close = join $ readIORef closeAct

  let gen off = do
        close -- close previous request

        let reqBase = mkRequest fs p
            req = reqBase { requestHeaders = h : requestHeaders reqBase }
            h = ("Range", "bytes=" <> B.pack (show start) <> "-")
            start = fromIntegral off :: Word64

        res <- responseOpen req $ manager fs
        writeIORef closeAct $ responseClose res
        newStats $ parseFileStats res

        return $ brRead (responseBody res)

  withAutoRestart <$> makeBufferedStream gen close

parseFileStats :: Response a -> FileStats
parseFileStats res =
  let headers = responseHeaders res
      size = lookup "Content-Length" headers >>= readMaybe . B.unpack
      time = fmap toEpochTime $ lookup "Last-Modified" headers >>= parseHTTPTime
  in FileStats size time

toEpochTime :: UTCTime -> EpochTime
toEpochTime = CTime . truncate . utcTimeToPOSIXSeconds

parseHTTPTime :: B.ByteString -> Maybe UTCTime
parseHTTPTime bs =
  msum $ map (\f -> parseTimeM True defaultTimeLocale f s) [rfc822, rfc850, ansiC]
  where
    s = B.unpack bs
    rfc822 = "%a, %d %b %Y %H:%M:%S %Z"
    rfc850 = "%A, %d-%b-y %H:%M:%S %Z"
    ansiC = "%a %b %e %H:%M:%S %Y"

requestHead :: HTTPFS -> B.ByteString -> IO (Response ())
requestHead fs p = do
  let req = mkRequest fs p
  httpNoBody (req { method = methodHead }) (manager fs)

mkDir :: B.ByteString -> B.ByteString
mkDir "/" = "/"
mkDir p = p <> "/"
