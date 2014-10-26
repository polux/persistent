// Copyright (c) 2014, VaccumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

library map_test;

import 'package:vacuum_persistent/persistent.dart';
import 'package:unittest/unittest.dart';

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

    test('update', () {
      PersistentMap pm = new PersistentMap();
      pm = pm.assoc('a', 'b');

      expect(pm.update('a', (a) => '$a b').toMap(), equals({'a': 'b b'}));
      expect(() => pm.update('c', (a) => '$a b'), throws);
      expect(pm.update('c', ([a = 'new value']) => '$a b').toMap(), equals({'a': 'b', 'c': 'new value b'}));
    });

    test('delete', () {
      PersistentMap pm = new PersistentMap();
      pm = pm.assoc('a', 'b');

      expect(pm.delete('a').toMap(), equals({}));
      expect(() => pm.delete('b'), throws);
      expect(pm.delete('b', missingOk: true).toMap(), equals({'a': 'b'}));
    });

    test('forEachKeyValue', () {
      PersistentMap pm = new PersistentMap();
      pm = pm.assoc('a', 'b');
      pm = pm.assoc('c', 'b');

      String res = '';
      pm.forEachKeyValue((k,v) => res = '${res}${k}${v},');

      expect(res, equals('ab,cb,'));
    });

    test('mapValues', () {
      PersistentMap pm = new PersistentMap();
      pm = pm.assoc('a', 'b');
      pm = pm.assoc('c', 'b');

      String res = '';
      pm = pm.mapValues((v) => '$v a');

      expect(pm.toMap(), equals({'a': 'b a', 'c': 'b a'}));
    });

    test('intersection', () {
      var m1 = new PersistentMap.fromMap({'a':1, 'b':2});
      var m2 = new PersistentMap.fromMap({'c':4, 'b':3});
      var i1 = m1.intersection(m2).toMap();
      var i2 = m1.intersection(m2, (x,y)=>x+y).toMap();
      expect(i1, equals({'b':3}));
      expect(i2, equals({'b':5}));
    });

    test('union', () {
      var m1 = new PersistentMap.fromMap({'a':1, 'b':2});
      var m2 = new PersistentMap.fromMap({'c':4, 'b':3});
      var u1 = m1.union(m2).toMap();
      var u2 = m1.union(m2, (x,y)=>x+y).toMap();
      expect(u1, equals({'b':3, 'a':1, 'c':4}));
      expect(u2, equals({'b':5, 'a':1, 'c':4}));
    });
  });

  group('Transient map', () {
    test('insert', () {
      TransientMap tm = new TransientMap();
      tm.doAssoc('a', 'b');
      expect(tm.toMap(), equals({'a': 'b'}));

      tm.doAssoc('a', 'c');
      expect(tm.toMap(), equals({'a': 'c'}));
    });

    test('get', () {
      TransientMap tm = new TransientMap();
      tm.doAssoc('a', 'b');
      tm.doAssoc('b', 'c');

      expect(tm.get('a'), equals('b'));
      expect(() => tm.get('c'), throws);
      expect(tm.get('c', 'none'), equals('none'));
    });

    test('update', () {
      TransientMap tm = new TransientMap();
      tm.doAssoc('a', 'b');

      expect(tm.doUpdate('a', (a) => '$a b').toMap(), equals({'a': 'b b'}));
      expect(() => tm.doUpdate('c', (a) => '$a b'), throws);
    });

    test('delete', () {
      TransientMap tm = new TransientMap();
      tm.doAssoc('a', 'b');
      tm.doAssoc('b', 'b');

      expect(tm.doDelete('a').toMap(), equals({'b': 'b'}));
      expect(() => tm.doDelete('c'), throws);
      expect(tm.doDelete('c', missingOk: true).toMap(), equals({'b': 'b'}));
    });

    test('forEachKeyValue', () {
      TransientMap tm = new TransientMap();
      tm.doAssoc('a', 'b');
      tm.doAssoc('c', 'b');

      String res = '';
      tm.forEachKeyValue((k,v) => res = '${res}${k}${v},');

      expect(res, equals('ab,cb,'));
    });

    test('mapValues', () {
      TransientMap tm = new TransientMap();
      tm.doAssoc('a', 'b');
      tm.doAssoc('c', 'b');

      String res = '';
      tm.doMapValues((v) => '$v a');

      expect(tm.toMap(), equals({'a': 'b a', 'c': 'b a'}));
    });
  });

  group('Persistent+Transient map', () {

      test('lookup', (){
        PersistentMap map = new PersistentMap();
        map = map.assoc('key1', 'val1');
        for (var _map in [map, map.asTransient()]) {
          expect(_map.get('key1'), equals('val1'));
          expect(() => _map.get('key2'), throws);
        }
      });

      test('containsKey', () {
        PersistentMap map = new PersistentMap();
        map = map.assoc('key1', 'val1');
        map = map.assoc('key2', 'val2');
        map = map.assoc('key3', 'val3');
        expect(map.containsKey('key1'), isTrue);
        expect(map.containsKey('key2'), isTrue);
        expect(map.containsKey('key22'), isFalse);
        TransientMap trans = map.asTransient();
        trans['key4'] = 'val4';
        expect(trans.containsKey('key1'), isTrue);
        expect(trans.containsKey('key2'), isTrue);
        expect(trans.containsKey('key4'), isTrue);
        expect(trans.containsKey('key22'), isFalse);

      });

    });

  group('deep persistent data', () {
    test('insertIn', () {
      PersistentMap map = new PersistentMap();
      map = map.assoc('a', new PersistentMap());
      PersistentMap map2 = insertIn(map, ['a', 'b'], 'c');

      expect(map2 == per({'a': {'b': 'c'}}), isTrue);
      expect(map == map2, isFalse);
      expect(map, equals(per({'a': {}})));
    });

    test('deleteIn', () {
      PersistentMap map = new PersistentMap();
      map = map.assoc('a', new PersistentMap());
      map = insertIn(map, ['a', 'b'], 'c');
      PersistentMap map2 = deleteIn(map, ['a', 'b']);

      expect(map2['a'], equals(new PersistentMap.fromMap({})));
      expect(map == map2, isFalse);
      expect(map, equals(new PersistentMap.fromMap({'a': new PersistentMap.fromMap({'b': 'c'})})));
    });

    test('lookupIn', () {
      PersistentMap map = new PersistentMap();
      map = map.assoc('a', new PersistentMap());
      map = insertIn(map, ['a', 'b'], 'c');

      expect(lookupIn(map, ['a', 'b']), equals('c'));
    });
  });
}
