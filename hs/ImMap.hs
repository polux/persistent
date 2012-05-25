-- Copyright 2012 Google Inc. All Rights Reserved.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- Author: Paul Brauner (polux@google.com)

module ImMap (
  Map(), empty, adjust, lookup, insert, delete, unionWith, toList,
  fromList, member, size
) where

import qualified Data.IntMap as IM
import qualified Data.Map as M -- for cheak equality
import Prelude hiding (lookup)
import qualified Prelude as P
import Data.Hashable
import Data.Bits
import Data.Maybe
import Data.List (sort)
import Control.Arrow((***))

data Map k v = EmptyMap
             | Leaf Int [(k, v)]
             | SubMap (IM.IntMap (Map k v))
  deriving (Show)

empty = EmptyMap

-- part n i selects the nth part of i
part n i = (i `shiftR` (n * 5)) .&. 0x1f
hash30 k = (hash k `shiftR` 2) .&. 0x3fffffff

lookup :: (Eq k, Hashable k) => k -> Map k v -> Maybe v
lookup k m = lookup' 0 m
  where
    code = hash30 k
    lookup' _ EmptyMap = Nothing
    lookup' _ (Leaf h assocs) | code == h = P.lookup k assocs
                              | otherwise = Nothing
    lookup' l (SubMap m) = lookup' (l+1) =<< IM.lookup (part l code) m


insert :: (Eq k, Hashable k) => k -> v -> Map k v -> Map k v
insert k v m = insertWith const 0 (hash30 k) [(k,v)] m

insertWith :: Eq k
           => (v -> v -> v) -- merge function
           -> Int           -- depth
           -> Int           -- hashcode
           -> [(k, v)]      -- assoc list to insert
           -> Map k v
           -> Map k v
insertWith f depth code kvs m = insertWith' depth m
  where
    insertWith'  _ EmptyMap = Leaf code kvs
    insertWith'  6 (Leaf h assocs) = Leaf code (insertAssocWith f kvs assocs)
    insertWith'  l lf@(Leaf code2 assocs)
      | code == code2 = Leaf code (insertAssocWith f kvs assocs)
      | otherwise = insertWith'  l (SubMap $ IM.singleton (part l code2) lf)
    insertWith'  l (SubMap im) =
      SubMap $ case IM.lookup i im of
        Nothing -> IM.insert i (Leaf code kvs) im
        Just m  -> IM.insert i (insertWith'  (l+1) m) im
      where i = part l code

insertAssocWith :: Eq k
                => (v -> v -> v) -- merge function
                -> [(k, v)]      -- assoc list to insert
                -> [(k, v)]
                -> [(k, v)]
insertAssocWith f kvs assocs = foldl iter assocs kvs
  where
    iter acc (k,v) = insertAssocWith' k v acc
    insertAssocWith' k v [] = [(k,v)]
    insertAssocWith' k v (p@(k', v') : assocs)
      | k == k' = (k, f v v') : assocs
      | otherwise = p : insertAssocWith' k v assocs

delete :: (Eq k, Hashable k) => k -> Map k v -> Map k v
delete k m = delete' 0 m
  where
    code = hash30 k
    delete' _ EmptyMap = EmptyMap
    delete' _ lf@(Leaf h assocs)
      | h == code =
        case filter (\p -> fst p /= k) assocs of
          [] -> EmptyMap
          assocs' -> (Leaf h assocs')
      | otherwise = lf
    delete' l m@(SubMap im) =
      case IM.lookup i im of
        Nothing -> m
        Just m' ->
          let m'' = delete' (l+1) m'
          in SubMap (IM.insert i m'' im) -- could be further optimized
-- like this for instance:
{-
          in case m'' of
               EmptyMap -> SubMap (IM.delete i im)
               _ -> SubMap (IM.insert i m'' im) -- could simplify the tree here in some cases
-}
      where i = part l code

unionWith :: Eq k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith f m1 m2 = unionWith' 0 m1 m2
  where unionWith' _ EmptyMap m = m
        unionWith' _ m EmptyMap = m
        unionWith' d (SubMap sm1) (SubMap sm2) = SubMap sm
          where sm = IM.unionWith (unionWith' (d+1)) sm1 sm2
        unionWith' d m@(SubMap sm) (Leaf h kvs) = insertWith (flip f) d h kvs m
        unionWith' d (Leaf h kvs) m@(SubMap sm) = insertWith f d h kvs m
        unionWith' d (Leaf h1 kvs1) m@(Leaf h2 kvs2) = insertWith f d h1 kvs1 m

adjust :: (Eq k, Hashable k) => k -> (v -> v) -> Map k v -> Map k v
adjust k f m = adjust' 0 m
  where code = hash30 k
        adjust' _ e@EmptyMap = e
        adjust' _ l@(Leaf h kvs) | h == code = Leaf h (adjust'' kvs)
                                 | otherwise = l
        adjust' d (SubMap sm) = SubMap (IM.adjust (adjust' (d+1)) (part d code) sm)

        adjust'' [] = []
        adjust'' (kv@(k',v):kvs) | k' == k = (k', f v):kvs
                                 | otherwise = kv : adjust'' kvs

size :: Map k v -> Int
size EmptyMap = 0
size (Leaf _ kvs) = length kvs
size (SubMap sm) = IM.fold (\m a -> size m + a) 0 sm

toList EmptyMap = []
toList (Leaf _ xs) = xs
toList (SubMap sm) = concatMap toList (IM.elems sm)

member k = isJust . lookup k

fromList l = foldl (\m (k,v) -> insert k v m) EmptyMap l

instance Functor (Map k) where
  fmap f m = fmap' m
    where fmap' EmptyMap = EmptyMap
          fmap' (Leaf h kvs) = Leaf h (map (id *** f) kvs)
          fmap' (SubMap sm) = SubMap (IM.map fmap' sm)

instance (Ord k, Eq k, Eq v) => Eq (Map k v) where
  m1 == m2 = M.fromList (toList m1) == M.fromList (toList m2)
