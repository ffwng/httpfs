module BufferedFile where

import Data.IORef
import Data.Monoid
import System.Posix (ByteCount, FileOffset)
import Foreign.C (Errno)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder
import Control.Concurrent.MVar

data BufferedFile = BufferedFile {
  readBufferedFile :: ByteCount -> FileOffset -> IO (Either Errno ByteString),
  closeBufferedFile :: IO ()
  }

makeBufferedFile :: (FileOffset -> IO (Either Errno (IO ByteString)))
                 -> IO ()
                 -> IO BufferedFile
makeBufferedFile gen close = do
  let readSource f = go 0 where
        remaining = toStrict . toLazyByteString
        go n lo count =
          if n >= count
            then do
              let bs = remaining lo
                  (res, rest) = B.splitAt (fromIntegral count) bs
              return (byteString rest, res)
            else do
              bs <- f
              if B.null bs
                then return (mempty, remaining lo)
                else go (n + B.length bs) (lo <> byteString bs) count

  source <- gen 0 >>= newIORef
  counter <- newIORef 0
  leftover <- newIORef mempty
  
  let readCount cur count = do
        s <- readIORef source
        case s of
          Left e -> return $ Left e
          Right f -> do
            lo <- readIORef leftover
            (lo', res) <- readSource f lo (fromIntegral count)
            writeIORef leftover lo'
            writeIORef counter $ cur + fromIntegral (B.length res)
            return $ Right res

  lock <- newMVar ()
  
  let readBuffered count off = withMVar lock $ \() -> do
        cur <- readIORef counter
        if off == cur
           then readCount cur count
           else do
             gen off >>= writeIORef source
             writeIORef counter off
             writeIORef leftover mempty
             readCount off count

  return $ BufferedFile readBuffered close
