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

import qualified Data.Map as M
import qualified ImMap as IM

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

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (M.Map k v) where
  arbitrary = M.fromList <$> arbitrary
  shrink = map M.fromList . shrink . M.toList

from = IM.fromList . M.toList
same im m = M.fromList (IM.toList im) == m

type Val = Int
type M = M.Map Key Val

equalsProp :: M -> M -> Bool
equalsProp m1 m2 = (from m1 == from m2) == (m1 == m2)

insertProp :: M -> Key -> Val -> (Val -> Val -> Val) -> Bool
insertProp m k v f = IM.insertWith f k v (from m) `same` M.insertWith f k v m

deleteProp :: M -> Key -> Bool
deleteProp m k = IM.delete k (from m) `same` M.delete k m

lookupProp :: M -> Key -> Bool
lookupProp m k = IM.lookup k (from m) == M.lookup k m

adjustProp :: M -> Key -> (Val -> Val) -> Bool
adjustProp m k f = IM.adjust k f (from m) `same` M.adjust f k m

fmapProp :: M -> (Val -> Val) -> Bool
fmapProp m f = fmap f (from m) `same` fmap f m

sizeProp :: M -> Bool
sizeProp m = IM.size (from m) == M.size m

unionProp :: M -> M -> (Val -> Val -> Val) -> Bool
unionProp m1 m2 f =
  IM.unionWith f (from m1) (from m2) `same` M.unionWith f m1 m2

check p = quickCheckWith args p
  where args = stdArgs { maxSuccess = 1000 }

main = do
  check equalsProp
  check insertProp
  check deleteProp
  check lookupProp
  check adjustProp
  check fmapProp
  check sizeProp
  check unionProp
