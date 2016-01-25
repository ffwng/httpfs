module BufferedStream where

import Data.IORef
import System.Posix (ByteCount, FileOffset)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Concurrent.MVar
import Control.Exception

data BufferedStream = BufferedStream {
  readBufferedStream :: ByteCount -> FileOffset -> IO ByteString,
  closeBufferedStream :: IO (),
  readWithRestart :: ByteCount -> FileOffset -> IO ByteString
  }

data ReadFunc = ReadFunc (ByteCount -> FileOffset -> IO (ByteString, ReadFunc))

mkReadFunc :: FileOffset -> (FileOffset -> IO Stream) -> IO ReadFunc
mkReadFunc pos0 gen = func pos0 <$> gen pos0 where
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
  let mkSource off = mkReadFunc off (fmap toStream . gen)

  source <- mkSource 0 >>= newIORef
  lock <- newMVar ()

  let readSource getSource count off = withMVar lock $ \() -> do
        ReadFunc f <- getSource
        (bs, new) <- f count off
        writeIORef source new
        return bs

      readBuffered = readSource (readIORef source)

      restartRead count off = readSource (mkSource off) count off


  return $ BufferedStream readBuffered close restartRead

withAutoRestart :: BufferedStream -> BufferedStream
withAutoRestart (BufferedStream f close restartRead) = BufferedStream f' close restartRead where
  f' count off = f count off `onException` restartRead count off
