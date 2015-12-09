module BufferedStream where

import Data.IORef
import System.Posix (ByteCount, FileOffset)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Concurrent.MVar

data BufferedStream = BufferedStream {
  readBufferedStream :: ByteCount -> FileOffset -> IO ByteString,
  closeBufferedStream :: IO ()
  }

data ReadFunc = ReadFunc (ByteCount -> FileOffset -> IO (ByteString, ReadFunc))

mkReadFunc :: (FileOffset -> IO Stream) -> IO ReadFunc
mkReadFunc gen = func 0 <$> gen 0 where
  func pos stream = ReadFunc $ \count off -> do
    inputStream <- if off == pos
                   then return stream
                   else gen off
    (bs, newStream) <- readAtMost inputStream count
    return (bs, func (off + fromIntegral (B.length bs)) newStream)


data Stream = Stream { readAtMost :: ByteCount -> IO (ByteString, Stream) }

toStream :: IO ByteString -> Stream
toStream f = func B.empty where
  func lo = Stream $ \count -> do
    (bs, lo') <- readHelper f (fromIntegral count) lo
    return (bs, func lo')

readHelper :: IO ByteString -> Int -> ByteString -> IO (ByteString, ByteString)
readHelper f count lo = go (B.length lo) (lo:) where
  go n dl = if n >= count
            then return . B.splitAt count . B.concat $ dl []
            else do
              chunk <- f
              let newN = if B.null chunk then count else n + B.length chunk
              go newN (dl . (chunk:))

makeBufferedStream :: (FileOffset -> IO (IO ByteString))
                   -> IO ()
                   -> IO BufferedStream
makeBufferedStream gen close = do
  source <- mkReadFunc (fmap toStream . gen) >>= newIORef
  lock <- newMVar ()

  let readBuffered count off = withMVar lock $ \() -> do
        ReadFunc f <- readIORef source
        (bs, new) <- f count off
        writeIORef source new
        return bs

  return $ BufferedStream readBuffered close
