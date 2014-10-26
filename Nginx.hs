{-# LANGUAGE Arrows #-}
module Nginx where

import Types

import Data.List

import Network.URI
import Text.XML.HXT.Core
import System.Posix.Types (COff(..))
import Foreign.C.Types (CTime(..))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)

getLinkName :: String -> String
getLinkName = unEscapeString

isDirLink :: String -> Bool
isDirLink s = "/" `isSuffixOf` s

parseName :: String -> String
parseName = unEscapeString

parseSize :: String -> EntrySize
parseSize _ = COff 0

parseDate :: String -> EntryDate
parseDate _ = CTime 0

parse :: (ArrowXml a, ArrowChoice a)
         => a XmlTree (EntryName, Either EntryType Entry)
parse = proc x -> do
  row <- deep (hasName "tbody") /> hasName "tr" -< x
  l <- listA (this /> hasName "td") -< row
  let [name_, _, date_] = l
  name <- this /> hasName "a" >>> getAttrValue0 "href" >>> isA (/= "../") -< name_
  date <- this /> getText -< date_
  let name' = parseName name
      date' = parseDate date
  if isDirLink name'
    then returnA -< (init name', Right $ Dir date')
    else returnA -< (name', Left FileType)

parseByteString :: ByteString -> IO [(EntryName, Either EntryType Entry)]
parseByteString bs = do
  let bs' = unpack bs
      doc = readString [withValidate no, withParseHTML yes, withWarnings no] bs'

  runX (doc >>> parse)
