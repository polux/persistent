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

library set_test;

import 'package:dart_check/dart_check.dart';
import 'package:persistent/persistent.dart';
import 'test_src/test_util.dart';

// a unary function on Keys
Key times42(Key key) => new Key(key.i * 42, key.b);

// a predicate on Keys
bool pred(Key key) => key.i % 3 == 0;

testEquals(Set<Key> s1, Set<Key> s2) =>
    (implemSetFrom(s1) == implemSetFrom(s2)) == setEquals(s1, s2);

testInsert(Set<Key> s, Key elem) =>
    sameSet(implemSetFrom(s).insert(elem), modelSetFrom(s).insert(elem));

testDelete(Set<Key> s, Key elem) {
  PersistentSet ps = implemSetFrom(s).delete(elem);
  return sameSet(ps, modelSetFrom(s).delete(elem))
    // checks that delete's normalizes the set so that == is well defined
    && implemSetFrom(ps.toSet()) == ps;
}

testContains(Set<Key> s, Key elem) =>
    implemSetFrom(s).contains(elem) == modelSetFrom(s).contains(elem);

testMap(Set<Key> s) =>
    sameSet(implemSetFrom(s).map(times42), modelSetFrom(s).map(times42));

testFilter(Set<Key> s) =>
    sameSet(implemSetFrom(s).filter(pred), modelSetFrom(s).filter(pred));

testSize(Set<Key> s) =>
    implemSetFrom(s).size() == modelSetFrom(s).size();

testUnion(Set<Key> s1, Set<Key> s2) =>
    sameSet(implemSetFrom(s1).union(implemSetFrom(s2)),
            modelSetFrom(s1).union(modelSetFrom(s2)));

testDifference(Set<Key> s1, Set<Key> s2) =>
    sameSet(implemSetFrom(s1).difference(implemSetFrom(s2)),
            modelSetFrom(s1).difference(modelSetFrom(s2)));

testIntersection(Set<Key> s1, Set<Key> s2) =>
    sameSet(implemSetFrom(s1).intersection(implemSetFrom(s2)),
            modelSetFrom(s1).intersection(modelSetFrom(s2)));

testCartesianProduct(Set<Key> s1, Set<Key> s2) =>
    sameSet(implemSetFrom(s1).cartesianProduct(implemSetFrom(s2)),
            modelSetFrom(s1).cartesianProduct(modelSetFrom(s2)));

main() {
  final e = new Enumerations();
  final properties = {
    'equals'       : forall2(e.sets, e.sets, testEquals),
    'insert'       : forall2(e.sets, e.keys, testInsert),
    'delete'       : forall2(e.sets, e.keys, testDelete),
    'contains'     : forall2(e.sets, e.keys, testContains),
    'map'          : forall(e.sets, testMap),
    'filter'       : forall(e.sets, testFilter),
    'size'         : forall(e.sets, testSize),
    'union'        : forall2(e.sets, e.sets, testUnion),
    'difference'   : forall2(e.sets, e.sets, testDifference),
    'intersection' : forall2(e.sets, e.sets, testIntersection),
    'product'      : forall2(e.sets, e.sets, testCartesianProduct),
  };
  testMain(properties);
}
