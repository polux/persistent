// Copyright (c) 2014, VaccumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

library functions_list_test;

import 'package:vacuum_persistent/persistent.dart';
import 'package:unittest/unittest.dart';

main() {
  run();
}

eqPer(x) => equals(persist(x));

run() {
  group("conj", (){
    test("- PersistentVector", (){
      PersistentVector s = persist([1, 2, 3]);
      expect(conj(s, 5), equals(s.push(5)));
      expect(conj(s, 5, 6), equals(s.push(5).push(6)));
      expect(conj(s, null), equals(s.push(null)));
    });

    test("- PersistentMap", (){
      PersistentMap s = persist({'a': 5});
      expect(conj(s, new Pair('b', 6)), equals(s.assoc('b', 6)));
      expect(conj(s, ['b', 6]), equals(s.assoc('b', 6)));
      expect(() => conj(s, 6), throws);
    });

    test("- PersistentSet", (){
      PersistentSet s = persist(new Set.from(['a', 'c']));
      expect(conj(s, "b"), equals(s.insert('b')));
    });

    test("- List Map Set throws", () {
      expect(() => conj([], 5), throws);
      expect(() => conj({}, [1, 2]), throws);
      expect(() => conj(new Set.from({}), [1, 2]), throws);
    });
  });

  group("into", (){
    test("- PersistentVector", (){
      PersistentVector s = persist([1, 2, 3]);
      expect(into(s, [5, 5]), equals(conj(s, 5, 5)));
    });
    test("- PersistentMap", (){
      PersistentMap s = persist({'a': 5});
      expect(into(s, [new Pair('b', 6), new Pair('a', 8)]), equals(s.assoc('b', 6).assoc('a', 8)));
    });

    test("- PersistentSet", (){
      PersistentSet s = persist(new Set.from(['a', 'c']));
      expect(into(s, ["b"]), equals(s.insert('b')));
    });
  });

  group("assoc", () {
    test("- PersistentVector", (){
      PersistentVector s = persist([1, 2, 3]);
      expect(assoc(s, 1, 7), equals(s.set(1, 7)));
      expect(assoc(s, 1, 7, 1, 8), equals(s.set(1, 8)));
      expect(assoc(s, 1, 7, 0, 8), equals(s.set(1, 7).set(0, 8)));
    });
    test("- PersistentMap", (){
      PersistentMap s = persist({'a': 5});
      expect(assoc(s, 'b', 8, 'c', 10), equals(s.assoc('b', 8).assoc('c', 10)));
    });

    test("- PersistentSet", (){
      PersistentSet s = persist(new Set.from(['a', 'c']));
      expect(() => assoc(s, "b", 5), throws);
    });

    test('- throw when key is specified but value not', () {
      var s = persist({'a' : 5});
      //key is provided but value is not
      expect(() => assoc(s, '0', 1, '5'), throws);
    });
  });

  group("dissoc", () {
    test("- PersistentMap", (){
      PersistentMap s = persist({'a': 5, 'b': 6, 'c': 7});
      expect(dissoc(s, 'b', 'c'), eqPer({'a': 5}));
      expect(dissoc(s, 'b', 'd'), eqPer({'a': 5, 'c': 7}));
    });

    test("- PersistentSet", (){
      PersistentSet s = persist(new Set.from(['a', 'b', 'c', 'd']));
      expect(dissoc(s, 'b', 'c'), eqPer(['a', 'd']));
      expect(dissoc(s, 'b', 'd'), eqPer(['a', 'c']));
    });

    test("- PersistentVector", (){
      PersistentVector s = persist(['a', 5, 'b', 6, 'c', 7]);
      expect(dissoc(s, 0, 3), eqPer([5, 'b', 'c', 7]));
      expect(dissoc(s, 3, 0), eqPer([5, 'b', 'c', 7]));
      expect(dissoc(s, -1), equals(s));
      expect(dissoc(s, 50), equals(s));
    });
  });

  group("distinc", () {
    test("- PersistentVector", (){
      PersistentVector s = persist([1, 3, 2, 4, 3, 3]);
      expect(distinct(s), eqPer([1,3,2,4]));
      s = persist([1, null, 1, null]);
      expect(distinct(s), eqPer([1, null]));
    });
  });

  group("empty", () {
    test("- PersistentVector", (){
      PersistentVector s = persist([1, 2, 3]);
      expect(empty(s), eqPer([]));
    });
    test("- PersistentMap", (){
      PersistentMap s = persist({'a': 5});
      expect(empty(s), eqPer({}));
    });

    test("- PersistentSet", (){
      PersistentSet s = persist(new Set.from(['a', 'c']));
      expect(empty(s), eqPer(new Set()));
    });
  });

  group("hasKey", () {
    test("- PersistentVector", (){
      PersistentVector s = persist([1, 2, 3]);
      expect(hasKey(s, -1), equals(false));
      expect(hasKey(s, 0), equals(true));
      expect(hasKey(s, 2), equals(true));
      expect(hasKey(s, 3), equals(false));
    });
    test("- PersistentMap", (){
      PersistentMap s = persist({'a': 5});
      expect(hasKey(s, 'a'), equals(true));
      expect(hasKey(s, 'b'), equals(false));
      expect(hasKey(s, 2), equals(false));
    });

    test("- PersistentSet", (){
      PersistentSet s = persist(new Set.from(['a', 'c']));
      expect(hasKey(s, 'a'), equals(true));
      expect(hasKey(s, 'b'), equals(false));
    });
  });

  group("get", () {
    test("- PersistentVector", (){
      PersistentVector s = persist([1, 2, 3]);
      expect(get(s, 0), equals(1));
      expect(get(s, 2), equals(3));
      expect(get(s, -1, 10), equals(10));
      expect(() => get(s, -1), throws);
    });
    test("- PersistentMap", (){
      PersistentMap s = persist({'a': 5, 'b': {'c': 1}});
      expect(get(s, 'a'), equals(5));
      expect(get(s, 'b'), eqPer({'c':1}));
      expect(get(s, 'c', 10), equals(10));
      expect(() => get(s, 'c'), throws);
    });

    test("- PersistentSet", (){
      PersistentSet s = persist(new Set.from(['a', 'c']));
      expect(get(s, 'a'), equals('a'));
      expect(get(s, 'c'), equals('c'));
      expect(get(s, 'd', 10), equals(10));
      expect(() => get(s, 'd'), throws);
    });
  });


  group("getIn", () {
    test("- Tree structure", (){
      PersistentVector s = persist([1, {'a': {'b': 4}}, 3]);
      expect(getIn(s, []), equals(s));
      expect(getIn(s, [0]), equals(1));
      expect(getIn(s, [1]), eqPer({'a': {'b': 4}}));
      expect(getIn(s, [1, 'a']), eqPer({'b': 4}));
      expect(getIn(s, [1, 'a', 'b']), equals(4));
      expect(getIn(s, [1, 'c', 'c', 'c'], 15), equals(15));
      expect(() => getIn(s, [1, 'c', 'c', 'c']), throws);
      expect(getIn(s, [1, 'a', 'c'], 17), equals(17));
      expect(() => getIn(s, [1, 'a', 'c']), throws);
    });
  });

  group("find", () {
    test("- PersistentVector", (){
      PersistentVector s = persist([1, 2, 3]);
      expect(() => find(s, -1), throws);
      expect(find(s, -1, 17), equals(new Pair(-1, 17)));
      expect(find(s, 0, 1), equals(new Pair(0, 1)));
      expect(find(s, 1, 2), equals(new Pair(1, 2)));
    });
    test("- PersistentMap", (){
      PersistentMap s = persist({'a': 5});
      expect(() => find(s, -1), throws);
      expect(find(s, -1, 17), equals(new Pair(-1, 17)));
      expect(find(s, 'a'), equals(new Pair('a', 5)));
      expect(find(s, 'a', 17), equals(new Pair('a', 5)));
    });

    test("- PersistentSet", (){
      PersistentSet s = persist(new Set.from(['a', 'c']));
      expect(() => find(s, -1), throws);
      expect(find(s, -1, 17), equals(new Pair(-1, 17)));
      expect(find(s, 'a'), equals(new Pair('a', 'a')));
      expect(find(s, 'a', 17), equals(new Pair('a', 'a')));
    });
  });

  group("assocIn", () {
    test("- Tree structure", (){
      PersistentVector s = persist([1, {'a': {'b': 4}}, 3]);
      expect(assocIn(s, [], persist({'c':10})), eqPer({'c': 10}));
      expect(assocIn(s, [], 5), equals(5));
      expect(assocIn(s, [0], 5), equals(assoc(s, 0, 5)));
      expect(assocIn(s, [1, 'a'], 17), eqPer([1, {'a': 17}, 3]));
      expect(assocIn(s, [1, 'a', 'b'], 17), eqPer([1, {'a': {'b':17}}, 3]));
      expect(assocIn(s, [1, 'a', 'c'], {'c':5}), eqPer([1, {'a': {'b': 4, 'c':{'c':5}}}, 3]));
      expect(() => assocIn(s, [1, 'c', 'c', 'c'], 14), throws);
    });
  });

  group("updateIn", () {
    test("- Tree structure", (){
      PersistentVector s = persist([1, {'a': {'b': 4}}, 3]);
      inc(x) => ++x;
      expect(updateIn(s, [], (x) => persist({})), eqPer({}));
      expect(() => updateIn(s, [], inc), throws);
      expect(updateIn(s, [0], inc), equals(assoc(s, 0, 2)));

      expect(updateIn(s, [1, 'a'], (x) => assoc(x, 'b', 6)), eqPer([1, {'a': {'b': 6}}, 3]));
      expect(updateIn(s, [1, 'a', 'b'], inc), eqPer([1, {'a': {'b':5}}, 3]));
      expect(() => updateIn(s, [1, 'a', 'c'], inc), throws);

      maybeInc([x]) => (x == null)? 0 : ++x;
      expect(updateIn(s, [1, 'a', 'c'], maybeInc), eqPer([1, {'a': {'b':4, 'c': 0}}, 3]));
      expect(() => updateIn(s, [1, 'c', 'c', 'c'], inc), throws);
    });
  });

  group("zipmap", () {
    test("", () {
      expect(zipmap(['a', 'b', 'c'], [1, 2, 3]), eqPer({'a':1, 'b':2, 'c':3}));
      expect(zipmap(['a', 'b', 'c'], [1, 2, 3, 4, 5]), eqPer({'a':1, 'b':2, 'c':3}));
      expect(zipmap(['a', 'b', 'c', 'd', 'e'], [1, 2, 3]), eqPer({'a':1, 'b':2, 'c':3}));
    });
  });

  group("subvec", () {
    test("", () {
      var s = persist([1,2,3,4,5]);
      expect(subvec(s, 0), equals(s));
      expect(subvec(s, 1), eqPer([2,3,4,5]));
      expect(subvec(s, 1, 3), eqPer([2,3]));
      expect(subvec(s, 1, 10), eqPer([2,3,4,5]));
      expect(subvec(s, 10), eqPer([]));
      expect(subvec(s, 1, -1), eqPer([]));
    });
  });

  group("disj", () {
    test("", () {
      var s = persist(new Set.from([1,'a','c']));
      expect(disj(s, 1), eqPer(new Set.from(['a', 'c'])));
      expect(disj(s, 5), equals(s));
    });
  });
}
