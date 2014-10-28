{-# LANGUAGE TupleSections, OverloadedStrings #-}
module HTTPFS where

import Types
import MemCache
import BufferedFile

import Data.Monoid
import Data.Word
import Data.IORef
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Control.Applicative
import Control.Monad
import Network.HTTP.Types
import Network.HTTP.Client
import Network.URI hiding (path, query)
import Foreign.C
import Text.Read
import System.Locale
import System.Posix (EpochTime)

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
  let mkReq = makeRequest $ f (reqtempl { checkStatus = \_ _ _ -> Nothing })
  mc <- newMemCache defaultTimeout
  return $ FS mkReq man mc


getHTTPDirectoryHTML :: FS -> FilePath -> IO (Either Errno BL.ByteString)
getHTTPDirectoryHTML fs p = do
  let p' = p ++ "/"
  res' <- httpLbs (mkRequest fs p') (manager fs)
  responseToErrno res' $ \res -> do
    _ <- processDirResponse fs p res
    return . Right $ responseBody res

getHTTPEntry :: FS -> FilePath -> IO (Either Errno Entry)
getHTTPEntry fs p = do
  cached <- query (cache fs) p
  case cached of
    Just (Right e) -> return (Right e)
    Just (Left t) -> do
      let p' = case t of
            FileType -> p
            DirType -> p ++ "/"
      requestHead fs p' >>= go t
    Nothing -> do
      let req (t, p') = (t,) <$> requestHead fs p'
          check (_, r) = responseStatus r /= status404
      res <- tryActions check $ map req [(DirType, p ++ "/"), (FileType, p)]
      case res of
        Nothing -> return $ Left eNOENT
        Just (t, res') -> go t res'
  where
    go t res' = responseToErrno res' $ \res -> do
      let f = case t of
            DirType -> processDirResponse
            FileType -> processFileResponse
      f fs p res

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
        res' <- responseOpen req' (manager fs)
        writeIORef closeAct (responseClose res')

        responseToErrno res' $ \res -> return . Right $ brRead (responseBody res)

  makeBufferedFile gen close

processDirResponse :: FS -> FilePath -> Response a -> IO (Either Errno Entry)
processDirResponse fs p _ = do
  let entry = Dir
  insert (cache fs) p (Right entry)
  return $ Right entry

processFileResponse :: FS -> FilePath -> Response a -> IO (Either Errno Entry)
processFileResponse fs p res = do
  let headers = responseHeaders res
  case (,,) <$> lookup "Content-Length" headers
       <*> lookup "Accept-Ranges" headers
       <*> lookup "Last-Modified" headers of
    Just (s, "bytes", t) | Just s' <- readMaybe (B8.unpack s)
                         , Just t' <- parseHTTPTime t -> do
      let entry = File (toEpochTime t') s'
      insert (cache fs) p (Right entry)
      return $ Right entry
    _ -> return $ Left eINVAL

toEpochTime :: UTCTime -> EpochTime
toEpochTime = CTime . truncate . utcTimeToPOSIXSeconds

parseHTTPTime :: B8.ByteString -> Maybe UTCTime
parseHTTPTime bs =
  msum $ map (\f -> parseTime defaultTimeLocale f s) [rfc822, rfc850, ansiC]
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

responseToErrno :: Response a -> (Response a -> IO (Either Errno b))
                -> IO (Either Errno b)
responseToErrno r f = case statusCode $ responseStatus r of
  x | x >= 200 && x < 300 -> f r
  404 -> return $ Left eNOENT
  _ -> return $ Left eINVAL
