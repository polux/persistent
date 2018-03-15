// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

library map_test;

import 'package:propcheck/propcheck.dart';
import 'package:persistent/persistent.dart';
import 'src/test_util.dart';

// Test const constructor for PersistentMap.
const constMap = const PersistentMap<Key, int>();

// a deliberately non-commutative operation on nullable integers
int minus(int x, int y) => (x == null ? 0 : x) - (y == null ? 0 : y);

// a unary function on nullable integers
int times42(x) => (x == null ? 0 : x) * 42;

bool testIsEmpty(Map<Key, int> map) =>
    implemMapFrom(map).isEmpty == modelMapFrom(map).isEmpty;

bool testEquals(Map<Key, int> map1, Map<Key, int> map2) =>
    (implemMapFrom(map1) == implemMapFrom(map2)) == mapEquals(map1, map2);

bool testHashCode(
    PersistentMap<Element, int> map1, PersistentMap<Element, int> map2) {
  return (map1 != map2) || (map1.hashCode == map2.hashCode);
}

bool testInsert(Map<Key, int> map, Key key, int value) => sameMap(
    implemMapFrom(map).insert(key, value, minus),
    modelMapFrom(map).insert(key, value, minus));

bool testDelete(Map<Key, int> map, Key key) {
  PersistentMap m = implemMapFrom(map).delete(key);
  return sameMap(m, modelMapFrom(map).delete(key))
      // checks that delete's normalizes the map so that == is well defined
      &&
      implemMapFrom(m.toMap()) == m;
}

bool testLookup(Map<Key, int> map, Key key) =>
    implemMapFrom(map).lookup(key) == modelMapFrom(map).lookup(key);

bool testAdjust(Map<Key, int> map, Key key) => sameMap(
    implemMapFrom(map).adjust(key, times42),
    modelMapFrom(map).adjust(key, times42) as ModelMap<Key, int>);

bool testMapValues(Map<Key, int> map) => sameMap(
    implemMapFrom(map).mapValues(times42),
    modelMapFrom(map).mapValues(times42));

bool testLength(Map<Key, int> map) =>
    implemMapFrom(map).length == modelMapFrom(map).length;

bool testUnion(Map<Key, int> map1, Map<Key, int> map2) => sameMap(
    implemMapFrom(map1).union(implemMapFrom(map2), minus),
    modelMapFrom(map1).union(modelMapFrom(map2), minus));

bool testIntersection(Map<Key, int> map1, Map<Key, int> map2) => sameMap(
    implemMapFrom(map1).intersection(implemMapFrom(map2), minus),
    modelMapFrom(map1).intersection(modelMapFrom(map2), minus));

bool testIterator(Map<Key, int> map) =>
    setEquals(implemMapFrom(map).toSet(), modelMapFrom(map).toSet());

bool testElementAt(Map<Key, int> map) {
  final expected = implemMapFrom(map);
  int i = 0;
  for (final entry in expected) {
    if (entry != expected.elementAt(i)) return false;
    i++;
  }
  return true;
}

bool testLast(Map<Key, int> map) {
  if (map.isEmpty) return true;
  final implem = implemMapFrom(map);
  return implem.last == naiveLast(implem);
}

main(List<String> arguments) {
  final e = new Enumerations();
  final properties = {
    'isEmpty': forall(e.maps, testIsEmpty),
    'equals': forall2(e.maps, e.maps, testEquals),
    'hashCode': forall2(e.pmaps, e.pmaps, testHashCode),
    'insert': forall3(e.maps, e.keys, e.values, testInsert),
    'delete': forall2(e.maps, e.keys, testDelete),
    'lookup': forall2(e.maps, e.keys, testLookup),
    'adjust': forall2(e.maps, e.keys, testAdjust),
    'mapValues': forall(e.maps, testMapValues),
    'length': forall(e.maps, testLength),
    'union': forall2(e.maps, e.maps, testUnion),
    'intersection': forall2(e.maps, e.maps, testIntersection),
    'iterator': forall(e.maps, testIterator),
    'elementAt': forall(e.maps, testElementAt),
    'last': forall(e.maps, testLast)
  };
  testMain(arguments, properties);
}
