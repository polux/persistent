// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

library map_test;

import 'package:propcheck/propcheck.dart';
import 'package:persistent/persistent.dart';
import 'test_src/test_util.dart';

// a deliberately non-commutative operation on nullable integers
int minus(int x, int y) => (x == null ? 0 : x) - (y == null ? 0 : y);

// a unary function on nullable integers
int times42(x) => (x == null ? 0 : x) * 42;

testIsEmpty(Map<Key, int> map) =>
    implemMapFrom(map).isEmpty == modelMapFrom(map).isEmpty;

testEquals(Map<Key, int> map1, Map<Key, int> map2) =>
    (implemMapFrom(map1) == implemMapFrom(map2)) == mapEquals(map1, map2);

testInsert(Map<Key, int> map, Key key, int value) =>
    sameMap(implemMapFrom(map).insert(key, value, minus),
            modelMapFrom(map).insert(key, value, minus));

testDelete(Map<Key, int> map, Key key) {
  PersistentMap m = implemMapFrom(map).delete(key);
  return sameMap(m, modelMapFrom(map).delete(key))
    // checks that delete's normalizes the map so that == is well defined
    && implemMapFrom(m.toMap()) == m;
}

testLookup(Map<Key, int> map, Key key) =>
    implemMapFrom(map).lookup(key) == modelMapFrom(map).lookup(key);

testAdjust(Map<Key, int> map, Key key) =>
    sameMap(implemMapFrom(map).adjust(key, times42),
            modelMapFrom(map).adjust(key, times42));

testMapValues(Map<Key, int> map) =>
    sameMap(implemMapFrom(map).mapValues(times42),
            modelMapFrom(map).mapValues(times42));

testLength(Map<Key, int> map) =>
    implemMapFrom(map).length == modelMapFrom(map).length;

testUnion(Map<Key, int> map1, Map<Key, int> map2) =>
    sameMap(implemMapFrom(map1).union(implemMapFrom(map2), minus),
            modelMapFrom(map1).union(modelMapFrom(map2), minus));

testIntersection(Map<Key, int> map1, Map<Key, int> map2) =>
    sameMap(implemMapFrom(map1).intersection(implemMapFrom(map2), minus),
            modelMapFrom(map1).intersection(modelMapFrom(map2), minus));

testIterator(Map<Key, int> map) =>
    setEquals(implemMapFrom(map).toSet(), modelMapFrom(map).toSet());

main() {
  final e = new Enumerations();
  final properties = {
    'isEmpty'     : forall(e.maps, testIsEmpty),
    'equals'      : forall2(e.maps, e.maps, testEquals),
    'insert'      : forall3(e.maps, e.keys, e.values, testInsert),
    'delete'      : forall2(e.maps, e.keys, testDelete),
    'lookup'      : forall2(e.maps, e.keys, testLookup),
    'adjust'      : forall2(e.maps, e.keys, testAdjust),
    'mapValues'   : forall(e.maps, testMapValues),
    'length'      : forall(e.maps, testLength),
    'union'       : forall2(e.maps, e.maps, testUnion),
    'intersection': forall2(e.maps, e.maps, testIntersection),
    'iterator'    : forall(e.maps, testIterator)
  };
  testMain(properties);
}
