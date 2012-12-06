-- Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
-- is governed by a BSD-style license that can be found in the LICENSE file.

-- Author: Paul Brauner (polux@google.com)

module SmallMap (
  SmallMap(),
  empty,
  singleton,
  lookup,
  elems,
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
import Data.Bits
import GHC.Int

import Debug.Trace

type BitMap = Int
type Mask = Int

data SmallMap a = SmallMap
  { bitmap :: BitMap
  , array  :: A.Array Int a
  } deriving (Eq, Show)


popcount x = 
  let x1 = x - ((x `shiftR` 1) .&. 0x55555555);
      x2 = (x1 .&. 0x33333333) + ((x1 `shiftR` 2) .&. 0x33333333);
      x3 = (x2 + (x2 `shiftR` 4)) .&. 0x0F0F0F0F;
      x4 = x3 + (x3 `shiftR` 8);
      x5 = x4 + (x4 `shiftR` 16);
  in x5 .&. 0x0000003F;

mask :: Int -> Mask
mask i = 1 `shiftL` i

index :: BitMap -> Mask -> Int
index b m = popcount (b .&. (m - 1))

set i x a = a A.// [(i, x)]

insertAt i x a = A.listArray (0, u+1) na
  where (_, u) = A.bounds a
        na = take i ea ++ [x] ++ drop i ea
        ea = A.elems a

deleteAt i a = A.listArray (0, u-1) na
  where (_, u) = A.bounds a
        na = take i ea ++ drop (i+1) ea
        ea = A.elems a

empty :: SmallMap a
empty = SmallMap 0 (A.listArray (0, -1) [])

insert :: Int -> a -> SmallMap a -> SmallMap a
insert i x (SmallMap b a) 
    | (b .&. msk) == 0 = SmallMap (b .|. msk) (insertAt idx x a)
    | otherwise        = SmallMap b (set idx x a)
  where msk = mask i
        idx = index b msk

singleton :: Int -> a -> SmallMap a
singleton i x = insert i x empty

lookup :: Int -> SmallMap a -> Maybe a
lookup i (SmallMap b a) 
    | (b .&. msk) == 0 = Nothing
    | otherwise        = Just (a A.! idx)
  where msk = mask i
        idx = index b msk

delete :: Int -> SmallMap a -> SmallMap a
delete i sm@(SmallMap b a) 
    | (b .&. msk) == 0 = error "no value at i in delete"
    | otherwise        = SmallMap (b .&. complement msk) (deleteAt idx a)
  where msk = mask i
        idx = index b msk

unionWith :: (a -> a -> a) -> SmallMap a -> SmallMap a -> SmallMap a
unionWith f (SmallMap b1 a1) (SmallMap b2 a2) = SmallMap b a
  where b = b1 .|. b2
        b' = b1 .&. b2
        a = go (A.array (0, popcount b - 1) []) 1 0 0 0
        go a m i j c | m > b = a
                     | b' .&. m /= 0 = go (set c (f (a1 A.! i) (a2 A.! j)) a)
                                          (m `shiftL` 1) (i + 1) (j + 1) (c + 1)
                     | b1 .&. m /= 0 = go (set c (a1 A.! i) a) 
                                          (m `shiftL` 1) (i + 1) j (c + 1)
                     | b2 .&. m /= 0 = go (set c (a2 A.! j) a) 
                                          (m `shiftL` 1) i (j + 1) (c + 1)
                     | otherwise     = go a (m `shiftL` 1) i j c


adjust :: (a -> a) -> Int -> SmallMap a -> SmallMap a
adjust f i sm@(SmallMap b a) 
    | (b .&. msk) == 0 = sm
    | otherwise        = SmallMap b (set idx (f oldval) a)
  where msk = mask i
        idx = index b msk
        oldval = a A.! idx

fold :: (a -> b -> b) -> b -> SmallMap a -> b
fold f z = foldr f z . elems

elems :: SmallMap a -> [a]
elems = A.elems . array

map :: (a -> b) -> SmallMap a -> SmallMap b
map f (SmallMap b a) = SmallMap b (A.amap f a)
