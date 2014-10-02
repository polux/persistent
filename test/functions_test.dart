// Copyright (c) 2014, VaccumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

library functions_list_test;

import 'package:persistent/persistent.dart';
import 'package:unittest/unittest.dart';

main() {
  run();
}

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
      expect(conj(s, new Pair('b', 6)), equals(s.insert('b', 6)));
      expect(conj(s, ['b', 6]), equals(s.insert('b', 6)));
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
      expect(into(s, [new Pair('b', 6), new Pair('a', 8)]), equals(s.insert('b', 6).insert('a', 8)));
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
      expect(assoc(s, 'b', 8, 'c', 10), equals(s.insert('b', 8).insert('c', 10)));
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
      expect(dissoc(s, 'b', 'c'), equals(persist({'a': 5})));
      expect(dissoc(s, 'b', 'd'), equals(persist({'a': 5, 'c': 7})));
    });
  });

  group("distinc", () {
    test("- PersistentVector", (){
      PersistentVector s = persist([1, 3, 2, 4, 3, 3]);
      expect(distinct(s), equals(persist([1,3,2,4])));
      s = persist([1, null, 1, null]);
      expect(distinct(s), equals(persist([1, null])));
    });
  });

  group("empty", () {
    test("- PersistentVector", (){
      PersistentVector s = persist([1, 2, 3]);
      expect(empty(s), equals(persist([])));
    });
    test("- PersistentMap", (){
      PersistentMap s = persist({'a': 5});
      expect(empty(s), equals(persist({})));
    });

    test("- PersistentSet", (){
      PersistentSet s = persist(new Set.from(['a', 'c']));
      expect(empty(s), equals(persist(new Set())));
    });
  });
}