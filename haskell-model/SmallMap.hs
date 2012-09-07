module SmallMap (
  SmallMap(),
  empty,
  singleton,
  lookup,
  elems,
  toList,
  insert,
  delete,
  adjust,
  map,
  unionWith,
  fold
) where

import Prelude hiding (lookup, map)
import qualified Data.Array.IArray as A
import Data.Maybe(catMaybes)

newtype SmallMap a = SmallMap { un :: (A.Array Int (Maybe a)) }
  deriving (Eq, Show)

empty :: SmallMap a
empty = SmallMap $ A.array (0, 31) [(i, Nothing) | i <- [0..31]] 

insert :: Int -> a -> SmallMap a -> SmallMap a
insert i x sm = SmallMap $ (un sm) A.// [(i, Just x)]

singleton :: Int -> a -> SmallMap a
singleton i x = insert i x empty

lookup :: Int -> SmallMap a -> Maybe a
lookup i sm = (un sm) A.! i

delete :: Int -> SmallMap a -> SmallMap a
delete i sm = SmallMap $ (un sm) A.// [(i, Nothing)]

toList :: SmallMap a -> [(Int, a)]
toList sm = [(i, x) | (i, Just x) <- A.assocs (un sm)]

unionWith :: (a -> a -> a) -> SmallMap a -> SmallMap a -> SmallMap a
unionWith f sm1 sm2 = SmallMap $ newArray
  where newArray = A.listArray (0, 31) (zipWith join (elems sm1) (elems sm2))
        elems = A.elems . un
        join Nothing Nothing = Nothing
        join j@(Just x) Nothing = j
        join Nothing j@(Just x) = j
        join (Just x) (Just y) = Just (f x y)

adjust :: (a -> a) -> Int -> SmallMap a -> SmallMap a
adjust f i sm = case lookup i sm of
                  Nothing -> sm
                  Just x  -> insert i (f x) sm

fold :: (a -> b -> b) -> b -> SmallMap a -> b
fold f z = foldr f z . elems

elems :: SmallMap a -> [a]
elems = catMaybes . A.elems . un

map :: (a -> b) -> SmallMap a -> SmallMap b
map f sm = SmallMap $ A.amap (fmap f) (un sm)
