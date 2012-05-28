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

import qualified ImMap as M

import Test.QuickCheck
import Control.Applicative
import Data.Hashable
import Control.Arrow((***))
import Text.Show.Functions

-- a datatype with an imperfect hash function
data Key = Key String Bool
  deriving (Show, Eq, Ord)

instance Hashable Key where
  hash (Key s _) = hash s

instance Arbitrary Key where
  arbitrary = Key <$> arbitrary <*> arbitrary
  shrink (Key s b) = Key <$> shrink s <*> shrink b

instance (Eq k, Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (M.Map k v) where
  arbitrary = M.fromList <$> arbitrary
  shrink = map M.fromList . shrink . M.toList

lookupInsertWith :: Key -> Int -> (Int -> Int -> Int) -> M.Map Key Int -> Bool
lookupInsertWith k v f m = M.lookup k (M.insertWith f k v m) == Just (maybe v (f v) (M.lookup k m))

deleteInsertWith :: Key -> Int -> (Int -> Int -> Int) -> M.Map Key Int -> Property
deleteInsertWith k v f m = not (k `M.member` m) ==> M.delete k (M.insertWith f k v m) == m

lookupDelete :: Key -> M.Map Key Int -> Bool
lookupDelete k m = M.lookup k (M.delete k m) == Nothing

fromListToList :: M.Map Key Int -> Bool
fromListToList m = M.fromList (M.toList m) == m

fmapToList :: M.Map Key Int -> (Int -> Int) -> Bool
fmapToList m f = M.toList (fmap f m) == map (id *** f) (M.toList m)

fmapInsert :: M.Map Key Int -> (Int -> Int) -> Key -> Int -> Bool
fmapInsert m f k v = M.insert k (f v) (fmap f m) == fmap f (M.insert k v m)

fmapDelete :: M.Map Key Int -> (Int -> Int) -> Key -> Bool
fmapDelete m f k = M.delete k (fmap f m) == fmap f (M.delete k m)

fmapLookup :: M.Map Key Int -> (Int -> Int) -> Key -> Bool
fmapLookup m f k = fmap f (M.lookup k m) == M.lookup k (fmap f m)

unionEmptyLeft :: M.Map Key Int -> (Int -> Int -> Int) -> Bool
unionEmptyLeft m f = M.unionWith f M.empty m == m

unionEmptyRight :: M.Map Key Int -> (Int -> Int -> Int) -> Bool
unionEmptyRight m f = M.unionWith f m M.empty == m

unionTrans :: M.Map Key Int -> M.Map Key Int -> M.Map Key Int -> Bool
unionTrans m1 m2 m3 = M.unionWith const m1 (M.unionWith const m2 m3) == M.unionWith const (M.unionWith const m1 m2) m3

unionFmap :: M.Map Key Int -> M.Map Key Int -> (Int -> Int) -> Bool
unionFmap m1 m2 f = fmap f (M.unionWith const m1 m2) == M.unionWith const (fmap f m1) (fmap f m2)

sizeEmpty :: Bool
sizeEmpty = M.size M.empty == 0

sizeInsertWith :: M.Map Key Int -> Key -> Int -> (Int -> Int -> Int) -> Bool
sizeInsertWith m k v f = M.size (M.insertWith f k v m) == M.size m + if M.member k m then 0 else 1

sizeDelete :: M.Map Key Int -> Key -> Bool
sizeDelete m k = M.size (M.delete k m) == M.size m - if M.member k m then 1 else 0

sizeFMap :: M.Map Key Int -> (Int -> Int) -> Bool
sizeFMap m f = M.size (fmap f m) == M.size m

adjustSpec :: M.Map Key Int -> Key -> (Int -> Int) -> Bool
adjustSpec m k f = M.adjust k f m == expected
  where expected = case M.lookup k m of
                     Nothing -> m
                     Just v -> M.insert k (f v) m

check p = quickCheckWith args p
  where args = stdArgs { maxSuccess = 1000 }

main = do
  check lookupDelete 
  check lookupInsertWith
  check deleteInsertWith
  check fromListToList
  check fmapToList
  check fmapInsert
  check fmapDelete 
  check fmapLookup
  check unionEmptyLeft
  check unionEmptyRight
  check unionTrans
  check unionFmap
  check sizeInsertWith
  check sizeDelete
  check sizeFMap
  check adjustSpec
