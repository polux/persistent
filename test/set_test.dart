// Copyright (c) 2014, VaccumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

library set_test;

import 'package:vacuum_persistent/persistent.dart';
import 'package:unittest/unittest.dart';

main() {
  run();
}

run() {

  group('Persistent set', () {

    test('length', () {
      var set = new PSet.from(["a","b","c"]);
      expect(set.length, equals(3));
    });

    test('contains', () {
      var set = new PSet.from(["a","b","c"]);
      expect(set.contains("a"), isTrue);
      expect(set.contains("b"), isTrue);
      expect(set.contains("c"), isTrue);
      expect(!set.contains("d"), isTrue);
    });

    test('==', () {
      var set1 = new PSet.from(["a","b"]);
      var set2 = new PSet.from(["b","a"]);
      var set3 = new PSet.from(["b"]);
      expect(set1==set2, isTrue);
      expect(set2!=set3, isTrue);
      expect(set1!=set3, isTrue);
    });

    test('union', () {
      var set1 = new PSet.from(["a","b","c"]);
      var set2 = new PSet.from(["d","c"]);
      var set3 = new PSet();
      expect(set1.union(set3) == set1, isTrue);
      expect(set3.union(set2) == set2, isTrue);
      expect(set1.union(set2) == new PSet.from(["a","b","c","d"]), isTrue);
    });

    test('difference', () {
      var set1 = new PSet.from(["a","b","c"]);
      var set2 = new PSet.from(["d","c"]);
      var set3 = new PSet();
      expect(set1.difference(set3) == set1, isTrue);
      expect(set3.difference(set2) == set3, isTrue);
      expect(
          set1.difference(set2) == new PSet.from(["a","b"]),
          isTrue
      );
    });

    test('intersection', () {
      var set1 = new PSet.from(["a","b","c"]);
      var set2 = new PSet.from(["d","c"]);
      var set3 = new PSet();
      expect(set1.intersection(set3) == set3, isTrue);
      expect(set3.intersection(set2) == set3, isTrue);
      expect(set1.intersection(set2) == new PSet.from(["c"]), isTrue);
    });

    test('cartesianProduct', () {
      var set1 = new PSet.from(["a","b","c"]);
      var set2 = new PSet.from(["d","c"]);
      var set3 = new PSet();
      expect(new PSet.from(set1 * set3) == set3, isTrue);
      expect(new PSet.from(set3 * set2) == set3, isTrue);
      expect(new PSet.from(set1 * set2) == new PSet.from([
        new Pair("a","d"), new Pair("b","d"), new Pair("c","d"),
        new Pair("a","c"), new Pair("b","c"), new Pair("c","c")
      ]), isTrue);
    });

    test('insert', () {
      var set = new PSet.from(["a","b","c"]);
      set = set.insert("c");
      expect(set, equals(new PSet.from(["a","b","c"])));
      expect(set.contains("c"), isTrue);
      set = set.insert("d");
      expect(set, equals(new PSet.from(["a","b","c","d"])));
      expect(set.contains("d"), isTrue);
    });

    test('delete', () {
      var set = new PSet.from(["a","b","c"]);
      set = set.delete("c");
      expect(set, equals(new PSet.from(["a","b"])));
      expect(set.contains("c"), isFalse);
      set = set.delete("d", missingOk:true);
      expect(set, equals(new PSet.from(["a","b"])));
      expect(set.contains("d"), isFalse);
    });
  });

  group('Transient set', () {

    test('length', () {
      var set = new PSet.from(["a","b","c"]).asTransient();
      expect(set.length, equals(3));
    });

    test('contains', () {
      var set = new PSet.from(["a","b","c"]).asTransient();
      expect(set.contains("a"), isTrue);
      expect(set.contains("b"), isTrue);
      expect(set.contains("c"), isTrue);
      expect(!set.contains("d"), isTrue);
    });

    test('doInsert', () {
      var set = new PSet.from(["a","b","c"]).asTransient();
      set.doInsert("c");
      expect(set.asPersistent(),
          equals(new PSet.from(["a","b","c"])));
      expect(set.contains("c"), isTrue);
      set = set.asPersistent().asTransient();
      set.doInsert("d");
      expect(set.asPersistent(),
          equals(new PSet.from(["a","b","c","d"])));
      expect(set.contains("d"), isTrue);
    });

    test('doDelete', () {
      var set = new PSet.from(["a","b","c"]).asTransient();
      set.doDelete("c");
      expect(set.asPersistent(), equals(new PSet.from(["a","b"])));
      expect(set.contains("c"), isFalse);
      set = set.asPersistent().asTransient();
      set.doDelete("d", missingOk:true);
      expect(set.asPersistent(), equals(new PSet.from(["a","b"])));
      expect(set.contains("d"), isFalse);
    });
  });
}