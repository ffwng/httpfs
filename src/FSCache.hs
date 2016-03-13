module FSCache where

import qualified Data.ByteString.Char8 as B
import Data.Maybe

import FS
import PathMap (Path, PathMap, (</>))
import qualified PathMap as PM

data CacheEntry t = NotFound !t | DirInfo !t !(Maybe t) | FileInfo !t !(Maybe (FileStats, t))
  deriving (Show, Eq, Ord)

data EntryInfo = NotFoundInfo | TypeInfo EntryType | FullInfo Entry (Maybe [(EntryName, EntryType)])
  deriving (Show, Eq, Ord)

toInfo :: Ord t => t -> CacheEntry t -> [(EntryName, EntryType)] -> Maybe EntryInfo
toInfo tcur e childs = case e of
  NotFound t | valid t -> Just NotFoundInfo
  DirInfo t ct | valid t -> Just $ FullInfo (Dir DirectoryStats) (cInfo ct)
  FileInfo t mstats | valid t -> case mstats of
    Just (stats, t') | valid t' -> Just $ FullInfo (File stats) (Just [])
    _ -> Just $ TypeInfo FileType
  _ -> Nothing
  where
    valid t = tcur <= t

    cInfo (Just t) | valid t = Just childs
    cInfo _ = Nothing

type Cache t = PathMap (CacheEntry t)

lookup :: Ord t => t -> Path -> Cache t -> Maybe EntryInfo
lookup t p m = case PM.match p m of
  Just (_, e, suffix) | B.null suffix -> toInfo t e childs
                      | otherwise -> if childrenKnown e then Just NotFoundInfo else Nothing
  _ -> Nothing

  where
    childs = mapMaybe addType (PM.lookupChildren p m)

    addType (p', FileInfo _ _) = Just (p', FileType)
    addType (p', DirInfo _ _) = Just (p', DirType)
    addType _ = Nothing

    childrenKnown e = case toInfo t e [] of
      Just (TypeInfo DirType) -> False
      Just (FullInfo _ Nothing) -> False
      _ -> True

fromInfo :: t -> Path -> EntryInfo -> [(Path, CacheEntry t)]
fromInfo t p info = case info of
  NotFoundInfo -> [(p, NotFound t)]
  TypeInfo ty -> (p, typeInfo ty) : parentInfos
  FullInfo e cs -> (p, entryInfo e (t <$ cs)) : maybe [] cInfos cs ++ parentInfos
  where
    parentInfos = map (\p' -> (p', DirInfo t Nothing)) . init $ PM.segments p

    typeInfo FileType = FileInfo t Nothing
    typeInfo DirType = DirInfo t Nothing

    entryInfo (File stats) _ = FileInfo t (Just (stats, t))
    entryInfo (Dir _) cs = DirInfo t cs

    cInfos = map (\(p', i) -> (p </> p', typeInfo i))

insert :: t -> Path -> EntryInfo -> Cache t -> Cache t
insert t p0 info cache = foldr (uncurry $ PM.insertWith' combine) cache $ fromInfo t p0 info

combine :: CacheEntry t -> CacheEntry t -> CacheEntry t
combine (DirInfo t c1) (DirInfo _ c2) = DirInfo t (first c1 c2)
combine (FileInfo t s1) (FileInfo _ s2) = FileInfo t (first s1 s2)
combine e _ = e

first :: Maybe a -> Maybe a -> Maybe a
first Nothing a = a
first a _ = a
