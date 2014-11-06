{-# LANGUAGE BangPatterns #-}
module BufferedFile where

import Data.IORef
import System.Posix (ByteCount, FileOffset)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Concurrent.MVar
import qualified System.IO.Streams as S
import Data.Functor
import Control.Monad
import Control.Exception

data BufferedFile = BufferedFile {
  readBufferedFile :: ByteCount -> FileOffset -> IO ByteString,
  closeBufferedFile :: IO ()
  }

makeBufferedFile :: (FileOffset -> IO (IO ByteString))
                 -> IO ()
                 -> IO BufferedFile
makeBufferedFile gen' close = do
  let gen off = gen' off >>= makeStream
  
  source <- gen 0 >>= newIORef
  pos <- newIORef 0
  
  lock <- newMVar ()
  
  let readBuffered count off = withMVar lock $ \() -> do
        cur <- readIORef pos
        input <- if off == cur
                 then readIORef source
                 else do
                   sourceNew <- gen off
                   writeIORef source sourceNew
                   return sourceNew
        let count' = fromIntegral count
        bs <- readAtMost count' input `onException` writeIORef pos (-1)
        writeIORef pos $ off + fromIntegral (B.length bs)
        return bs

  return $ BufferedFile readBuffered close


makeStream :: IO ByteString -> IO (S.InputStream ByteString)
makeStream f = S.makeInputStream (trans <$> f) where
  trans bs | B.null bs = Nothing
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
