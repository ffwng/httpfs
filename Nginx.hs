{-# LANGUAGE Arrows #-}
module Nginx where

import Types

import Data.List

import Network.URI
import Text.XML.HXT.Core
import Foreign.C.Types (CTime(..))

isDirLink :: String -> Bool
isDirLink s = "/" `isSuffixOf` s

parseDate :: String -> EntryDate
parseDate _ = CTime 0

parse :: (ArrowXml a, ArrowChoice a)
         => a XmlTree (EntryName, Either EntryType Entry)
parse = proc x -> do
  row <- deep (hasName "tbody") /> hasName "tr" -< x
  [name_, _, date_] <- listA (this /> hasName "td") -< row
  name <- this /> hasName "a" >>> getAttrValue0 "href"
          >>> isA (/= "../") >>> arr unEscapeString -< name_
  date <- this /> getText >>> arr parseDate -< date_
  if isDirLink name
    then returnA -< (init name, Right $ Dir date)
    else returnA -< (name, Left FileType)

