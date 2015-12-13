module Parser (
  EntryName, EntryDate, EntrySize, Entry(..),
  Parser, xpathParser,
  parseByteString
) where

import qualified Data.ByteString.Lazy.Char8 as BL
import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows
import Network.URI
import System.Posix.Types

type EntryName = String
type EntryDate = EpochTime
type EntrySize = FileOffset

data Entry = Dir
           | IncompleteFile
           | File (Maybe EntryDate) (Maybe EntrySize)
           deriving (Show, Eq, Ord)

newtype Parser = Parser (IOSLA (XIOState ()) XmlTree (EntryName, Entry))

xpathParser :: String -> Parser
xpathParser p = Parser $ getXPathTreesInDoc p >>> entries

parseByteString :: Parser -> BL.ByteString -> IO [(EntryName, Entry)]
parseByteString (Parser parse) bs = do
  let bs' = BL.unpack bs
      doc = readString [withValidate no, withParseHTML yes, withWarnings no] bs'

  runX (doc >>> parse)

entries :: ArrowXml a => a XmlTree (EntryName, Entry)
entries = getAttrValue0 "href" >>> isA isValid >>> arr (mkEntry . unEscapeString) where

mkEntry :: String -> (EntryName, Entry)
mkEntry "" = ("", IncompleteFile)
mkEntry str = case splitLast str of
  (name, '/') -> (name, Dir)
  _ -> (str, IncompleteFile)

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
