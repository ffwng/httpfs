{-# LANGUAGE BangPatterns #-}
module BufferedFile where

import Data.IORef
import System.Posix (ByteCount, FileOffset)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Concurrent.MVar
import qualified System.IO.Streams as S
import Control.Applicative
import Control.Monad
import Control.Exception

data BufferedFile = BufferedFile {
  readBufferedFile :: ByteCount -> FileOffset -> IO ByteString,
  closeBufferedFile :: IO ()
  }

data ReadFunc = ReadFunc (ByteCount -> FileOffset -> IO (ByteString, ReadFunc))

mkReadFunc :: (FileOffset -> IO (S.InputStream ByteString)) -> IO ReadFunc
mkReadFunc gen = func 0 <$> gen 0 where
  func pos stream = ReadFunc $ \count off -> do
    newStream <- if off == pos
                 then return stream
                 else gen off
    bs <- readAtMost (fromIntegral count) newStream
    return (bs, func (off + fromIntegral count) newStream)

makeBufferedFile :: (FileOffset -> IO (IO ByteString))
                 -> IO ()
                 -> IO BufferedFile
makeBufferedFile gen close = do
  source <- mkReadFunc (gen >=> makeStream) >>= newIORef
  lock <- newMVar ()
  
  let readBuffered count off = withMVar lock $ \() -> do
        ReadFunc f <- readIORef source
        (bs, new) <- f count off
        writeIORef source new
        return bs

  return $ BufferedFile readBuffered close

makeStream :: IO ByteString -> IO (S.InputStream ByteString)
makeStream f = S.makeInputStream (trans <$> f)
  where trans bs | B.null bs = Nothing
                 | otherwise = Just bs

-- copied and adapted from io-streams
readAtMost :: Int                     -- ^ number of bytes to read
            -> S.InputStream ByteString  -- ^ input stream
            -> IO ByteString
readAtMost n input = go id n
  where
    go !dl 0  = return $! B.concat $! dl []
    go !dl k  =
        S.read input >>=
        maybe (return $! B.concat $! dl [])
              (\s -> do
                 let l = B.length s
                 if l >= k
                   then do
                     let (a,b) = B.splitAt k s
                     when (not $ B.null b) $ S.unRead b input
                     return $! B.concat $! dl [a]
                   else go (dl . (s:)) (k - l))
