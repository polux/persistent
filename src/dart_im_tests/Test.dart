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

class Test {
  Map<String, Function> _tests;

  Test() {
    _tests = {
      "testLookupDelete": testLookupDelete,
      "testLookupInsert": testLookupInsert,
      "testDeleteInsert": testDeleteInsert,
      "testMapValuesInsert": testMapValuesInsert,
      "testMapValuesDelete": testMapValuesDelete,
      "testMapValuesLookup": testMapValuesLookup,
      "testUnionEmptyLeft": testUnionEmptyLeft,
      "testUnionEmptyRight": testUnionEmptyRight,
      "testUnionMapValues": testUnionMapValues,
      "testUnionTrans": testUnionTrans,
      "testSizeEmpty": testSizeEmpty,
      "testSizeInsert": testSizeInsert,
      "testSizeDelete": testSizeDelete,
      "testSizeMapValues": testSizeMapValues,
      "testAdjustSpec": testAdjustSpec
    };
  }

  // runs all the tests in _tests
  run(updateCallback(String)) {
    _tests.forEach((name, f) {
      f((info) => updateCallback("$name: $info"));
    });
  }

  // looking up a deleted key yields none
  testLookupDelete(callback(String)) =>
    forAllMap((map) =>
        forAllInt((k) =>
            map.delete(k).lookup(k) == new Option.none()),
        updateCallback: callback);

  // looking up an inserted key/value yields the value
  testLookupInsert(callback(String)) {
    combine(x, y) => x + 42 * y;
    expected(map, k, v) {
      Option<int> present = map.lookup(k);
      return present.isDefined
          ? combine(present.value, v)
          : v;
    }
    return forAllMap((map) =>
        forAllInt((k) =>
            forAllInt((v) =>
                map.insert(k, v, combine).lookup(k)
                    == new Option.some(expected(map, k, v)))),
        updateCallback: callback);
  }

  // deleting an inserted key yields the orginal map,
  // provided it didn't contain the key
  testDeleteInsert(callback(String)) {
    combine(n, m) => n + 42 * m;
    return forAllMap((map) =>
        forAllInt((k) =>
            forAllInt((v) =>
                (map.lookup(k).isDefined)
                    ? true
                    : map.insert(k, v, combine).delete(k) == map)),
        updateCallback: callback);
  }

  // mapValues and insert commute
  testMapValuesInsert(callback(String)) {
    int f(int n) => n * 42;
    return forAllMap((map) =>
        forAllInt((k) =>
            forAllInt((v) =>
                map.mapValues(f).insert(k, f(v))
                    == map.insert(k,v).mapValues(f))),
        updateCallback: callback);
  }

  // mapValues and delete commute
  testMapValuesDelete(callback(String)) {
    int f(int n) => n * 42;
    return forAllMap((map) =>
        forAllInt((k) =>
            map.mapValues(f).delete(k) == map.delete(k).mapValues(f)),
        updateCallback: callback);
  }

  // mapValues and lookup commute
  testMapValuesLookup(callback(String)) {
    int f(int n) => n * 42;
    return forAllMap((map) =>
        forAllInt((k) =>
            map.mapValues(f).lookup(k) == map.lookup(k).map(f)),
        updateCallback: callback);
  }

  // empty is union's left neutral
  testUnionEmptyLeft(callback(String)) {
    return forAllMap((map) =>
        new ImmutableMap().unionWith(map, (v1,v2) => v1 * v2) == map,
        updateCallback: callback);
  }

  // empty is union's right neutral
  testUnionEmptyRight(callback(String)) {
    return forAllMap((map) =>
        map.unionWith(new ImmutableMap(), (v1,v2) => v1 * v2) == map,
        updateCallback: callback);
  }

  // union with f and mapValue with g commute provided f and g commute
  testUnionMapValues(callback(String)) {
    // g(f(v1,v2)) == f(g(v1), g(v2))
    f(v1, v2) => v1 + v2;
    g(x) => x * 42;
    return forAllMap((map1) =>
        forAllMap((map2) =>
            map1.unionWith(map2, f).mapValues(g)
                == map1.mapValues(g).unionWith(map2.mapValues(g), f)),
        updateCallback: callback);
  }

  testUnionTrans(callback(String)) {
    // f is transitive
    f(v1, v2) => v1 + v2;
    return forAllMap((map1) =>
        forAllMap((map2) =>
            forAllMap((map3) =>
                map1.unionWith(map2, f).unionWith(map3, f)
                    == map1.unionWith(map2.unionWith(map3, f), f))),
        updateCallback: callback);
  }

  // an empty map has size 0
  testSizeEmpty(callback(String)) {
    callback("1 / 1");
    new ImmutableMap().size() == 0;
  }

  // inserting (key,value) in a map increments its size
  // unless the key was already present
  testSizeInsert(callback(String)) =>
      forAllMap((m) =>
          forAllInt((k) =>
              forAllInt((v) =>
                  m.insert(k, v, (a, b) => a + 42 * b).size()
                      == m.size() + (m.lookup(k).isDefined ? 0 : 1))),
           updateCallback : callback);

  // deleting a key from a map decrements its size
  // provided the key was present in the map
  testSizeDelete(callback(String)) =>
      forAllMap((m) =>
          forAllInt((k) =>
              m.delete(k).size()
                  == m.size() - (m.lookup(k).isDefined ? 1 : 0)),
           updateCallback : callback);

  // mapValues doesn't change size
  testSizeMapValues(callback(String)) =>
      forAllMap((m) =>
           m.mapValues((n) => n * 42).size() == m.size(),
           updateCallback : callback);

  // adjust behaves like its spec
  testAdjustSpec(callback(String)) {
    ImmutableMap<int,int> spec(
        ImmutableMap<int,int> m, int key, int f(int n)) {
      Option<int> val = m.lookup(key);
      return val.isDefined ? m.insert(key, f(val.value)) : m;
    }
    int f(n) => n * 42;
    return forAllMap((m) =>
        forAllInt((k) => m.adjust(k, f) == spec(m, k, f)),
        updateCallback : callback);
  }
}
