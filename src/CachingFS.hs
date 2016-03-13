module CachingFS where

import qualified Data.ByteString.Char8 as B
import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Exception
import System.Posix.Types
import System.Posix.Time

import FS
import PathMap (Path, empty)
import FSCache

import Prelude hiding (lookup)

addCache :: Int -> FS -> IO FS
addCache delay fs = do
  cache <- newMVar empty
  return FS {
    getEntry = getCachedEntry delay cache fs,
    getDirectoryEntries = getCachedDirectoryEntries delay cache fs,
    getFileContent = getFileContent fs
  }

type Time = EpochTime

getExpireTime :: Int -> IO Time
getExpireTime delay = (+ fromIntegral delay) <$> epochTime

getCurrentTime :: IO Time
getCurrentTime = epochTime

cacheInfo :: Int -> MVar (Cache Time) -> Path -> EntryInfo -> IO ()
cacheInfo delay cache p info = do
  t <- getExpireTime delay
  modifyMVar_ cache (return . insert t p info)

lookupCache :: MVar (Cache Time) -> Path -> IO (Maybe EntryInfo)
lookupCache cache p = do
  t <- getCurrentTime
  withMVar cache $ return . lookup t p

getCachedEntry :: Int -> MVar (Cache Time) -> FS -> Maybe EntryType -> B.ByteString -> IO Entry
getCachedEntry delay cache fs defaultType p = do
  res <- lookupCache cache p
  case res of
    Just NotFoundInfo -> throwIO NotFoundException
    Just (FullInfo e _) -> return e
    _ -> do
      let ti = (res >>= typeInfo) <|> defaultType
      e <- getEntry fs ti p `onNotFound` cacheInfo delay cache p NotFoundInfo
      cacheInfo delay cache p (FullInfo e Nothing)
      return e
  where
    typeInfo (TypeInfo ti) = Just ti
    typeInfo _ = Nothing

getCachedDirectoryEntries :: Int -> MVar (Cache Time) -> FS -> B.ByteString -> IO [(EntryName, EntryType)]
getCachedDirectoryEntries delay cache fs p = do
  res <- lookupCache cache p
  case res of
    Just NotFoundInfo -> throwIO NotFoundException
    Just (FullInfo (Dir _) (Just cs)) -> return cs
    _ -> do
      cs <- getDirectoryEntries fs p `onNotFound` cacheInfo delay cache p NotFoundInfo
      cacheInfo delay cache p (FullInfo (Dir DirectoryStats) (Just cs))
      return cs

onNotFound :: IO a -> IO b -> IO a
onNotFound act handler = act `catch` \NotFoundException -> handler >> throwIO NotFoundException
