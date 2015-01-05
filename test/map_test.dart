// Copyright (c) 2014, VaccumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

library map_test;

import 'package:vacuum_persistent/persistent.dart';
import 'package:unittest/unittest.dart';
import 'dart:math';


class Element {
  var value, hash;
  Element(this.value, this.hash);
  get hashCode => hash;
  operator ==(other){
    if (other is! Element) {
      return false;
    }
    return value == other.value;
  }
}

main() {
  run();
}

run() {
  group('Persistent map', () {
    test('assoc', () {
      PersistentMap pm = new PersistentMap();
      pm = pm.assoc('a', 'b');
      expect(pm.toMap(), equals({'a': 'b'}));
      pm = pm.assoc('a', 'c');
      expect(pm.toMap(), equals({'a': 'c'}));

    });

    test('get', () {
      PersistentMap pm = new PersistentMap();
      pm = pm.assoc('a', 'b');
      pm = pm.assoc('b', 'c');

      expect(pm.get('a'), equals('b'));
      expect(() => pm.get('c'), throws);
      expect(pm.get('c','none'), equals('none'));
    });

    test('delete', () {
      PersistentMap pm = new PersistentMap();
      pm = pm.assoc('a', 'b');

      expect(pm.delete('a').toMap(), equals({}));
      expect(() => pm.delete('b'), throws);
      expect(pm.delete('b', missingOk: true).toMap(), equals({'a': 'b'}));
    });

    test('update', () {
      PersistentMap pm = persist({'a':'b', 'c': 'd'});
      expect(pm.update('c', (v) => 'updated $v'), equals(persist({'a': 'b', 'c': 'updated d'})));
    });

    test('equality speed', (){
      PersistentMap pm;
      Map m = {};
      for(int i=0; i<100000; i++) {
        m['hello${i}'] = 'world${i}';
      }
      pm = persist(m);
      // more important than the 'expect' is that this test takes reasonable time to finish
      for (int i=0; i<100000; i++) {
        expect(pm == pm.assoc('hello${i}', 'different'), isFalse);
      }
    });

    test('transient basics', (){
      TransientMap m = new TransientMap();
      m.doAssoc('a', 'b');
      m.doAssoc('c', 'd');
      expect(m.toMap(), equals({'a':'b', 'c':'d'}));
      m.doAssoc('a', 'bb');
      expect(m.toMap(), equals({'a':'bb', 'c':'d'}));
      m.doUpdate('a', (v) => "b${v}");
      expect(m.toMap(), equals({'a':'bbb', 'c':'d'}));
      m.doDelete('a');
      expect(m.toMap(), equals({'c':'d'}));
    });

    test('equality on hash colision', (){
      var e1 = new Element(0, 0);
      var e2 = new Element(1, 0);
      var m1 = persist({e1: 0, e2: 0});
      var m2 = persist({e2: 0, e1: 0});
      expect(m1, equals(m2));
    });

    solo_test('pokus', (){
      Map m = {};
      for (int i=0; i<2000; i++){
        m[i]=i;
      }
      PersistentMap pm = persist(m);
      print(pm);
      print(pm.runtimeType);
      for (int i=0; i<2000-50; i++){
        pm = pm.delete(i);
      }
      print(pm.length);
      print(pm);
      print(pm.runtimeType);

    });

  });

}
