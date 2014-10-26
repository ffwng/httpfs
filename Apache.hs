{-# LANGUAGE Arrows #-}
module Apache where

import Types

import Data.List

import Network.URI
import Text.XML.HXT.Core
import Foreign.C.Types (CTime(..))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)

isDirLink :: String -> Bool
isDirLink s = "/" `isSuffixOf` s

parseDate :: String -> EntryDate
parseDate _ = CTime 0

parse :: (ArrowXml a, ArrowChoice a)
         => a XmlTree (EntryName, Either EntryType Entry)
parse = proc x -> do
  row <- deep (hasName "table") //> hasName "tr" -< x
  [_, name_, date_, _, _] <- listA (this /> hasName "td")
                             >>> isA ((==5) . length) -< row
  name <- this /> hasName "a" >>> getAttrValue0 "href"
          >>> isA noParent >>> arr unEscapeString -< name_
  date <- this /> getText >>> arr parseDate -< date_
  if isDirLink name
    then returnA -< (init name, Right $ Dir date)
    else returnA -< (name, Left FileType)

noParent :: String -> Bool
noParent ('/':_) = False
noParent _ = True

parseByteString :: ByteString -> IO [(EntryName, Either EntryType Entry)]
parseByteString bs = do
  let bs' = unpack bs
      doc = readString [withValidate no, withParseHTML yes, withWarnings no] bs'

  runX (doc >>> parse)
