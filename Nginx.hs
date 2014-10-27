{-# LANGUAGE Arrows #-}
module Nginx where

import Types

import Data.List
import Network.URI
import Text.XML.HXT.Core

isDirLink :: String -> Bool
isDirLink s = "/" `isSuffixOf` s

parse :: (ArrowXml a, ArrowChoice a, ArrowIO a)
         => a XmlTree (EntryName, Either EntryType Entry)
parse = proc x -> do
  row <- deep (hasName "tbody") /> hasName "tr" -< x
  [name_, _, _] <- listA (this /> hasName "td") -< row
  name <- this /> hasName "a" >>> getAttrValue0 "href"
          >>> isA (/= "../") >>> arr unEscapeString -< name_
  if isDirLink name
    then returnA -< (init name, Right Dir)
    else returnA -< (name, Left FileType)

