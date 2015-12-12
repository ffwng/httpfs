{-# LANGUAGE TupleSections, OverloadedStrings #-}
module HTTPFS where

import Types
import MemCache
import BufferedStream
import Parser

import Data.Monoid
import Data.Word
import Data.IORef
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Char8 as B8
import Control.Monad
import Control.Exception
import Network.HTTP.Types
import Network.HTTP.Client
import Network.URI hiding (path, query)
import Text.Read
import System.Posix (EpochTime)
import Foreign.C (CTime(..))

type Cache = MemCache FilePath Entry

defaultTimeout :: Int
defaultTimeout = 600

data FS = FS {
  mkRequest :: FilePath -> Request,
  manager :: Manager,
  entryParser :: Parser,
  cache :: Cache
  }

newFS :: (B8.ByteString -> Request) -> Parser -> ManagerSettings -> IO FS
newFS mkReq p manset = do
  let mkReq' = mkReq . B8.pack . encode
      encode = escapeURIString (\c -> isUnreserved c || c == '/')
  man <- newManager manset
  mc <- newMemCache defaultTimeout
  return $ FS mkReq' man p mc


getHTTPDirectoryEntries :: FS -> FilePath -> IO [(EntryName, Entry)]
getHTTPDirectoryEntries fs p = do
  let p' = mkDir p
  res <- httpLbs (mkRequest fs p') (manager fs)
  _ <- processDirResponse fs p res
  let content = responseBody res
  entries <- parseByteString (entryParser fs) content
  forM_ entries $ \(n, e) -> insertWith betterEntry (cache fs) (p' ++ n) e
  return entries

getHTTPEntry :: FS -> FilePath -> IO Entry
getHTTPEntry fs p = do
  cached <- query (cache fs) p
  case cached of
    Just IncompleteFile -> requestHead fs p >>= processFileResponse fs p
    Just e -> return e
    Nothing -> do
      dirRes <- tryJust is404 $ requestHead fs (mkDir p)
      case dirRes of
        Right res -> processDirResponse fs p res
        Left _ -> requestHead fs p >>= processFileResponse fs p
  where
    is404 (StatusCodeException s _ _) | s == status404 = Just ()
    is404 _ = Nothing

getHTTPContent :: FS -> FilePath -> IO BufferedStream
getHTTPContent fs p = do
  closeAct <- newIORef (return ())

  let close = join $ readIORef closeAct

  let gen off = do
        let req = mkRequest fs p
            req' = req { requestHeaders = h : requestHeaders req }
            h = ("Range", "bytes=" <> B8.pack (show start) <> "-")
            start = fromIntegral off :: Word64
        close -- close previous request
        res <- responseOpen req' (manager fs)
        _ <- processFileResponse fs p res
        writeIORef closeAct (responseClose res)

        return $ brRead (responseBody res)

  makeBufferedStream gen close

processDirResponse :: FS -> FilePath -> Response a -> IO Entry
processDirResponse fs p _ = do
  let entry = Dir
  insert (cache fs) p entry
  return entry

processFileResponse :: FS -> FilePath -> Response a -> IO Entry
processFileResponse fs p res = do
  let headers = responseHeaders res
      size = lookup "Content-Length" headers >>= readMaybe . B8.unpack
      time = fmap toEpochTime $ lookup "Last-Modified" headers >>= parseHTTPTime
      entry = File time size

  insert (cache fs) p entry
  return entry

toEpochTime :: UTCTime -> EpochTime
toEpochTime = CTime . truncate . utcTimeToPOSIXSeconds

parseHTTPTime :: B8.ByteString -> Maybe UTCTime
parseHTTPTime bs =
  msum $ map (\f -> parseTimeM True defaultTimeLocale f s) [rfc822, rfc850, ansiC]
  where
    s = B8.unpack bs
    rfc822 = "%a, %d %b %Y %H:%M:%S %Z"
    rfc850 = "%A, %d-%b-y %H:%M:%S %Z"
    ansiC = "%a %b %e %H:%M:%S %Y"

requestHead :: FS -> FilePath -> IO (Response ())
requestHead fs p = do
  --putStrLn $ "request " ++ p
  let req = mkRequest fs p
  httpNoBody (req { method = methodHead }) (manager fs)

tryActions :: (a -> Bool) -> [IO a] -> IO (Maybe a)
tryActions f = go where
  go [] = return Nothing
  go (a:as) = do
    res <- a
    if f res
      then return $ Just res
      else go as

mkDir :: String -> String
mkDir "/" = "/"
mkDir p = p ++ "/"
