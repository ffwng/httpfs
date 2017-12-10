module PathMap where

import qualified Data.Trie.BigEndianPatricia.Base as T
import qualified Data.Trie.BigEndianPatricia.Convenience as T
import qualified Data.Trie.BigEndianPatricia.Internal as T
import qualified Data.ByteString as B
import Data.Monoid
import Data.Word
import Data.Char (ord)

type Path = B.ByteString

pathSep :: Word8
pathSep = fromIntegral (ord '/')

pathNull :: Path -> Bool
pathNull = B.null

normalize :: Path -> Path
normalize p | B.null p || B.last p /= pathSep = p
            | otherwise = B.init p

(</>) :: Path -> Path -> Path
p1 </> p2 = normalize p1 <> B.cons pathSep p2

segments :: Path -> [Path]
segments = B.split pathSep

data PathMap a = PathMap (T.Trie a)
  deriving (Show, Eq)

empty :: PathMap a
empty = PathMap T.empty

insert :: Path -> a -> PathMap a -> PathMap a
insert p a (PathMap trie) = PathMap (T.insert (normalize p) a trie)

insertWith' :: (a -> a -> a) -> Path -> a -> PathMap a -> PathMap a
insertWith' f p a (PathMap trie) = PathMap (T.insertWith' f (normalize p) a trie)

lookup :: Path -> PathMap a -> Maybe a
lookup p (PathMap trie) = T.lookup (normalize p) trie

match :: Path -> PathMap a -> Maybe (Path, a, Path)
match p (PathMap trie) = getLast . foldMap findMatch $ T.matches trie (normalize p) where
  findMatch x@(_, _, suffix) | B.null suffix || B.head suffix == pathSep = Last (Just x)
                             | otherwise = mempty

lookupChildren :: Path -> PathMap a -> [(Path, a)]
lookupChildren p (PathMap trie) = T.lookupBy (\_ t -> children t) (normalize p) trie where
  children = T.toList . T.cut pathSep . T.lookupBy (\_ t -> t) (B.singleton pathSep)
