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

data CacheEntry = FoundError HttpException | Found Entry
  deriving (Show)

type Cache = MemCache FilePath CacheEntry

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
  (_, res) <- cacheResponse parseDir fs p' $ httpLbs (mkRequest fs p') (manager fs)
  let content = responseBody res
  entries <- parseByteString (entryParser fs) content
  forM_ entries $ \(n, e) -> insertWith betterEntry (cache fs) (p' ++ n) (Found e)
  return entries
  where
    betterEntry (Found IncompleteFile) e = e
    betterEntry e _ = e

getHTTPEntry :: FS -> FilePath -> IO Entry
getHTTPEntry fs p = do
  cached <- query (cache fs) p
  case cached of
    Just (FoundError ex) -> throwIO ex
    Just (Found IncompleteFile) -> cacheResponse' parseFile fs p $ requestHead fs p
    Just (Found e) -> return e
    Nothing -> cacheResponse' id fs p $ do
      dirRes <- tryJust is404 $ requestHead fs (mkDir p)
      case dirRes of
        Right res -> return $ parseDir res
        Left _ -> parseFile <$> requestHead fs p
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
        (_, res) <- cacheResponse parseFile fs p $ responseOpen req' (manager fs)
        writeIORef closeAct (responseClose res)

        return $ brRead (responseBody res)

  makeBufferedStream gen close

cacheResponse' :: (a -> Entry) -> FS -> FilePath -> IO a -> IO Entry
cacheResponse' f fs p act = fst <$> cacheResponse f fs p act

cacheResponse :: (a -> Entry) -> FS -> FilePath -> IO a -> IO (Entry, a)
cacheResponse f fs p act = do
  res <- tryJust scEx act
  case res of
    Right a -> let e = f a in insert (cache fs) p (Found e) >> return (e, a)
    Left ex -> insert (cache fs) p (FoundError ex) >> throwIO ex
  where
    scEx ex@StatusCodeException {} = Just ex
    scEx _ = Nothing

parseDir :: Response a -> Entry
parseDir _ = Dir

parseFile :: Response a -> Entry
parseFile res =
  let headers = responseHeaders res
      size = lookup "Content-Length" headers >>= readMaybe . B8.unpack
      time = fmap toEpochTime $ lookup "Last-Modified" headers >>= parseHTTPTime
  in File time size

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
