module Parser (
  EntryName, EntryDate, EntrySize, Entry(..), FileStats(..),
  Parser, xpathParser,
  parseByteString
) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.UTF8 as B8
import qualified Data.ByteString.Lazy.UTF8 as L8
import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows
import Network.URI

import FS

newtype Parser = Parser (IOSLA (XIOState ()) XmlTree (EntryName, EntryType))

xpathParser :: String -> Parser
xpathParser p = Parser $ getXPathTreesInDoc p >>> entries

parseByteString :: Parser -> BL.ByteString -> IO [(EntryName, EntryType)]
parseByteString (Parser parse) bs = do
  let bs' = L8.toString bs
      doc = readString [withValidate no, withParseHTML yes, withWarnings no] bs'

  runX (doc >>> parse)

entries :: ArrowXml a => a XmlTree (EntryName, EntryType)
entries = getAttrValue0 "href" >>> isA isValid >>> arr (mkEntry . unEscapeString)

mkEntry :: String -> (EntryName, EntryType)
mkEntry "" = (B.empty, FileType)
mkEntry str = case splitLast str of
  (name, '/') -> (B8.fromString name, DirType)
  _ -> (B8.fromString str, FileType)

splitLast :: [a] -> ([a], a)
splitLast [] = error "splitLast"
splitLast (x:[y]) = ([x], y)
splitLast (x:xs) = let (xs', y) = splitLast xs in (x:xs', y)

isValid :: String -> Bool
isValid link = isRelLink link && not (isParent link) && not (isSame link)

isRelLink :: String -> Bool
isRelLink ('/':_) = False
isRelLink _ = True

isParent :: String -> Bool
isParent "../" = True
isParent _ = False

isSame :: String -> Bool
isSame ('?':_) = True
isSame _ = False
