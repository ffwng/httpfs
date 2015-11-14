{-# LANGUAGE Arrows #-}
module Nginx where

import Types

import Data.List
import Network.URI
import Text.XML.HXT.Core

isDirLink :: String -> Bool
isDirLink s = "/" `isSuffixOf` s

isParentLink :: String -> Bool
isParentLink s = s == "../" || isAbs s where
  isAbs ('/':_) = True
  isAbs _ = False

parse :: (ArrowXml a, ArrowChoice a, ArrowIO a)
         => a XmlTree (EntryName, Either EntryType Entry)
parse = proc x -> do
  td <- deep (hasName "td") -< x
  name <- this /> hasName "a" >>> getAttrValue0 "href"
          >>> isA (not . isParentLink) >>> arr unEscapeString -< td
  if isDirLink name
    then returnA -< (init name, Right Dir)
    else returnA -< (name, Left FileType)
