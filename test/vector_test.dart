// Copyright (c) 2014, VaccumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

library vector_test;

import 'package:vacuum_persistent/persistent.dart';
import 'package:unittest/unittest.dart';

main() {
  run();
}

run() {

  group('PersistentVector', () {

    PV(list) => new PersistentVector.from(list);

    test('get', () {
      PersistentVector v = PV([0, 1, 2]);
      expect(v.get(0), equals(0));
      expect(v[1], equals(1));
      expect(v.get(2, 47), equals(2));
      expect(() => v.get(3), throws);
      expect(() => v[4], throws);
      expect(v.get(5, 47), equals(47));
    });

    test('first', () {
      expect(PV([0, 1, 2]).first, equals(0));
      expect(PV([0]).first, equals(0));
      expect(() => PV([]).first, throws);
    });

    test('last', () {
      expect(PV([0, 1, 2]).last, equals(2));
      expect(PV([0]).last, equals(0));
      expect(() => PV([]).last, throws);
    });

    test('iterator', () {
      // Indirectly, using `toList`
      expect(PV([0, 1, 2]).toList(), equals([0, 1, 2]));
      expect(PV([0, 1]).toList(), equals([0, 1]));
      expect(PV([0]).toList(), equals([0]));
      expect(PV([]).toList(), equals([]));
    });

    test('length', () {
      expect(PV([0, 1, 2]).length, equals(3));
      expect(PV([0, 1]).length, equals(2));
      expect(PV([0]).length, equals(1));
      expect(PV([]).length, equals(0));
    });

    test('push', () {
      expect(PV([0]).push(1).toList(), equals([0, 1]));
      expect(PV([]).push(0).push(1).toList(), equals([0, 1]));
    });

    test('pop', () {
      expect(PV([0, 1]).pop().toList(), equals([0]));
      expect(PV([0, 1]).pop().pop().toList(), equals([]));
      expect(() => PV([0, 1]).pop().pop().pop(), throws);
    });

    test('set', () {
      expect(PV([0, 1]).set(0, 1).toList(), equals([1, 1]));
      expect(() => PV([0, 1]).set(2, 1), throws);
    });

    test('asTransient', () {
      expect(PV([0, 1]).asTransient().toList(), equals([0, 1]));
      expect(PV([0, 1]).asTransient() is TransientVector, isTrue);
    });

    test('withTransient', () {
      expect(PV([0, 1]).withTransient((v){
         v[0]=2;
      }).toList(), equals([2, 1]));
    });


    test('pushing nulls', () {
      PersistentVector v = PV([]);
      v = v.push(null);
      v = v.push(47);
      expect(v, orderedEquals([null, 47]));
    });

    test('created from array of nulls', () {
      PersistentVector v = PV([null, null]);
      v = v.push(null);
      v = v.push(47);
      expect(v, orderedEquals([null, null, null, 47]));
    });
  });


  group('TransientVector', () {

    TV(list) => new PersistentVector.from(list).asTransient();

    test('get', () {
      TransientVector v = TV([0, 1, 2]);
      expect(v.get(0), equals(0));
      expect(v[1], equals(1));
      expect(v.get(2, 47), equals(2));
      expect(() => v.get(3), throws);
      expect(() => v[4], throws);
      expect(v.get(5, 47), equals(47));
    });

    test('first', () {
      expect(TV([0, 1, 2]).first, equals(0));
      expect(TV([0]).first, equals(0));
      expect(() => TV([]).first, throws);
    });

    test('last', () {
      expect(TV([0, 1, 2]).last, equals(2));
      expect(TV([0]).last, equals(0));
      expect(() => TV([]).last, throws);
    });

    test('iterator', () {
      // Indirectly, using `toList`
      expect(TV([0, 1, 2]).toList(), equals([0, 1, 2]));
      expect(TV([0, 1]).toList(), equals([0, 1]));
      expect(TV([0]).toList(), equals([0]));
      expect(TV([]).toList(), equals([]));
    });

    test('length', () {
      expect(TV([0, 1, 2]).length, equals(3));
      expect(TV([0, 1]).length, equals(2));
      expect(TV([0]).length, equals(1));
      expect(TV([]).length, equals(0));
    });

    test('push', () {
      expect(TV([0])..doPush(1)..toList(), equals([0, 1]));
      expect(TV([])..doPush(0)..doPush(1)..toList(), equals([0, 1]));
    });

    test('pop', () {
      expect(TV([0, 1])..doPop()..toList(), equals([0]));
      expect(TV([0, 1])..doPop()..doPop()..toList(), equals([]));
      expect(() => TV([0, 1])..doPop()..doPop()..doPop(), throws);
    });

    test('set', () {
      TransientVector v = TV([0, 1]);
      v[0] = 1;
      expect(v, equals([1, 1]));
      expect(TV([0, 1])..doSet(0, 1)..toList(), equals([1, 1]));
      expect(() => TV([0, 1])..doSet(2, 1), throws);
      expect(() => TV([0, 1])[2] = 1, throws);
    });

    test('asPersistent', () {
      expect(TV([0, 1]).asPersistent().toList(), equals([0, 1]));
      expect(TV([0, 1]).asPersistent() is PersistentVector, isTrue);
    });


  });
}