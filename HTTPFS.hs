{-# LANGUAGE TupleSections, OverloadedStrings #-}
module HTTPFS where

import Types
import MemCache
import BufferedFile

import Data.Maybe
import Data.Monoid
import Data.Word
import Data.IORef
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Control.Exception
import Network.HTTP.Types
import Network.HTTP.Client
import Network.URI hiding (path, query)
import Text.Read
import System.Posix (EpochTime)
import Foreign.C (CTime(..))

type Cache = MemCache FilePath (Either EntryType Entry)

defaultTimeout :: Int
defaultTimeout = 600

data FS = FS {
  mkRequest :: FilePath -> Request,
  manager :: Manager,
  cache :: Cache
  }

newFS :: (Request -> Request) -> String -> ManagerSettings -> IO FS
newFS f baseurl manset = do
  man <- newManager manset
  reqtempl <- parseUrl baseurl
  let mkReq = makeRequest $ f reqtempl
  mc <- newMemCache defaultTimeout
  return $ FS mkReq man mc


getHTTPDirectoryHTML :: FS -> FilePath -> IO BL.ByteString
getHTTPDirectoryHTML fs p = do
  let p' = p ++ "/"
  res <- httpLbs (mkRequest fs p') (manager fs)
  _ <- processDirResponse fs p res
  return $ responseBody res

getHTTPEntry :: FS -> FilePath -> IO Entry
getHTTPEntry fs p = do
  cached <- query (cache fs) p
  case cached of
    Just (Right e) -> return e
    Just (Left t) -> do
      let (p', process) = case t of
            FileType -> (p, processFileResponse)
            DirType -> (p ++ "/", processDirResponse)
      requestHead fs p' >>= process fs p
    Nothing -> do
      dirRes <- tryJust is404 $ requestHead fs (p ++ "/")
      case dirRes of
        Right res -> processDirResponse fs p res
        Left _ -> requestHead fs p >>= processFileResponse fs p
  where
    is404 (StatusCodeException s _ _) | s == status404 = Just ()
    is404 _ = Nothing

getHTTPContent :: FS -> FilePath -> IO BufferedFile
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
        writeIORef closeAct (responseClose res)

        return $ brRead (responseBody res)

  makeBufferedFile gen close

processDirResponse :: FS -> FilePath -> Response a -> IO Entry
processDirResponse fs p _ = do
  let entry = Dir
  insert (cache fs) p (Right entry)
  return entry

processFileResponse :: FS -> FilePath -> Response a -> IO Entry
processFileResponse fs p res = do
  let headers = responseHeaders res
      size = fromMaybe 0 $ lookup "Content-Length" headers
                           >>= readMaybe . B8.unpack
  --case (,) <$> lookup "Accept-Ranges" headers
  --         <*> lookup "Last-Modified" headers of
  case lookup "Last-Modified" headers of
    Just t | Just t' <- parseHTTPTime t -> do
      let entry = File (toEpochTime t') size
      insert (cache fs) p (Right entry)
      return entry
    _ -> error "invalid file response"

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

makeRequest :: Request -> FilePath -> Request
makeRequest templ p = templ { path = path templ <> B8.pack (encode p) } where
  encode = escapeURIString (\c -> isUnreserved c || c == '/')
