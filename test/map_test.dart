// Copyright 2012 Google Inc. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Author: Paul Brauner (polux@google.com)

#library('map_test');

#import('dart:math');
#import('package:dart-check/dart_check.dart');
#import('package:dart-enumerators/combinators.dart');
#import('package:dart-enumerators/enumerators.dart');
#import('package:dart-immutable/dart_immutable.dart');
#import('package:unittest/unittest.dart');

#source('test_src/test_util.dart');
#source('test_src/map_model.dart');

// a deliberately non-commutative operation on ints
minus(int x, int y) => x - y;

// a unary function on integers
times42(x) => x * 42;

testEquals(Map<Key, int> map1, Map<Key, int> map2) =>
    (implemFrom(map1) == implemFrom(map2)) == mapEquals(map1, map2);

testInsert(Map<Key, int> map, Key key, int value) =>
    same(implemFrom(map).insert(key, value, minus),
         modelFrom(map).insert(key, value, minus));

testDelete(Map<Key, int> map, Key key) {
  ImmutableMap m = implemFrom(map).delete(key);
  return same(m, modelFrom(map).delete(key))
    // checks that delete's normalizes the map so that == is well defined
    && implemFrom(m.toMap()) == m;
}

testLookup(Map<Key, int> map, Key key) =>
    implemFrom(map).lookup(key) == modelFrom(map).lookup(key);

testAdjust(Map<Key, int> map, Key key) =>
    same(implemFrom(map).adjust(key, times42),
         modelFrom(map).adjust(key, times42));

testMapValues(Map<Key, int> map) =>
    same(implemFrom(map).mapValues(times42),
        modelFrom(map).mapValues(times42));

testSize(Map<Key, int> map) =>
    implemFrom(map).size() == modelFrom(map).size();

testUnion(Map<Key, int> map1, Map<Key, int> map2) =>
    same(implemFrom(map1).union(implemFrom(map2), minus),
         modelFrom(map1).union(modelFrom(map2), minus));

main() {
  final e = new Enumerations();
  final sc = new SmallCheck(depth: 15);
  final qc = new QuickCheck(maxSize: 300);

  final properties = {
    'equals'   : forall2(e.maps, e.maps, testEquals),
    'insert'   : forall3(e.maps, e.keys, e.values, testInsert),
    'delete'   : forall2(e.maps, e.keys, testDelete),
    'lookup'   : forall2(e.maps, e.keys, testLookup),
    'adjust'   : forall2(e.maps, e.keys, testAdjust),
    'mapValues': forall(e.maps, testMapValues),
    'size'     : forall(e.maps, testSize),
    'union'    : forall2(e.maps, e.maps, testUnion)
  };

  group('quickcheck', () {
    properties.forEach((name, prop) {
      test(name, () => qc.check(prop));
    });
  });
  group('smallcheck', () {
    properties.forEach((name, prop) {
      test(name, () => sc.check(prop));
    });
  });
}
