module Parser (
  Parser, xpathParser,
  parseByteString
) where

import Types

import qualified Data.ByteString.Lazy.Char8 as BL
import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows
import Network.URI


newtype Parser = Parser (IOSLA (XIOState ()) XmlTree (EntryName, Either EntryType Entry))

xpathParser :: String -> Parser
xpathParser p = Parser $ getXPathTreesInDoc p >>> entries

parseByteString :: Parser -> BL.ByteString -> IO [(EntryName, Either EntryType Entry)]
parseByteString (Parser parse) bs = do
  let bs' = BL.unpack bs
      doc = readString [withValidate no, withParseHTML yes, withWarnings no] bs'

  runX (doc >>> parse)

entries :: ArrowXml a => a XmlTree (EntryName, Either EntryType Entry)
entries = getAttrValue0 "href" >>> isA isValid >>> arr (mkEntry . unEscapeString) where

mkEntry :: String -> (EntryName, Either EntryType Entry)
mkEntry "" = ("", Left FileType)
mkEntry str = case splitLast str of
  (name, '/') -> (name, Right Dir)
  _ -> (str, Left FileType)

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
