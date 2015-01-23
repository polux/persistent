// Copyright (c) 2014, VaccumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

library map_test;

import 'package:vacuum_persistent/persistent.dart';
import 'package:unittest/unittest.dart';


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
  toString() => "Element(${value},${hash})";
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

    test('union', (){
      var dm1 = {};
      var dm2 = {};
      for(int i = 1; i <= 1000; i++){
        if(i%4 != 3) dm1[i] = i;
        if(i%4 != 1) dm2[i] = -i;
      }
      PersistentMap m1 = persist(dm1);
      PersistentMap m2 = persist(dm2);
      PersistentMap m3 = m1.union(m2);
      PersistentMap m4 = m1.union(m2, (a,b)=>a+b);

      expect(m3.length, equals(1000));
      expect(m4.length, equals(1000));
      for(int i = 1; i <= 1000; i++){
        if(i%4 == 1){
          expect(m3[i], equals(i));
          expect(m4[i], equals(i));
        } else if(i%4 == 3){
          expect(m3[i], equals(-i));
          expect(m4[i], equals(-i));
        } else {
          expect(m3[i], equals(-i));
          expect(m4[i], equals(0));
        }
      }
    });

    test('intersection', (){
      var dm1 = {};
      var dm2 = {};
      for(int i = 1; i <= 1000; i++){
        if(i%4 != 3) dm1[i] = i;
        if(i%4 != 1) dm2[i] = -i;
      }
      PersistentMap m1 = persist(dm1);
      PersistentMap m2 = persist(dm2);
      PersistentMap m3 = m1.intersection(m2);
      PersistentMap m4 = m1.intersection(m2, (a,b)=>a+b);

      expect(m3.length, equals(500));
      expect(m4.length, equals(500));
      for(int i = 2; i <= 1000; i+=2){
        expect(m3[i], equals(-i));
        expect(m4[i], equals(0));
      }
    });

  });

}
