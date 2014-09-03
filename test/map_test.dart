// Copyright (c) 2013, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

library option_test;

import 'package:persistent/persistent.dart';
import 'package:unittest/unittest.dart';

main() {
  group('Persistent map', () {
    test('insert', () {
      PersistentMap pm = new PersistentMap();
      pm = pm.insert('a', 'b');
      expect(pm.toMap(), equals({'a': 'b'}));

      pm = pm.insert('a', 'c');
      expect(pm.toMap(), equals({'a': 'c'}));

      pm = pm.insert('a', 'b', (a, b) => '$a$b');
      expect(pm.toMap(), equals({'a': 'cb'}));
    });

    test('lookup', () {
      PersistentMap pm = new PersistentMap();
      pm = pm.insert('a', 'b');
      pm = pm.insert('b', 'c');

      expect(pm.lookup('a'), equals('b'));
      expect(() => pm.lookup('c'), throws);
      expect(pm.lookup('c', orElse: () => 'none'), equals('none'));
    });

    test('adjust', () {
      PersistentMap pm = new PersistentMap();
      pm = pm.insert('a', 'b');

      expect(pm.adjust('a', (a) => '$a b').toMap(), equals({'a': 'b b'}));
      expect(() => pm.adjust('c', (a) => '$a b'), throws);
    });

    test('delete', () {
      PersistentMap pm = new PersistentMap();
      pm = pm.insert('a', 'b');

      expect(pm.delete('a').toMap(), equals({}));
      expect(() => pm.delete('b'), throws);
      expect(pm.delete('b', safe: true).toMap(), equals({'a': 'b'}));
    });

    test('forEachKeyValue', () {
      PersistentMap pm = new PersistentMap();
      pm = pm.insert('a', 'b');
      pm = pm.insert('c', 'b');

      String res = '';
      pm.forEachKeyValue((k,v) => res = '${res}${k}${v},');

      expect(res, equals('ab,cb,'));
    });

    test('mapValues', () {
      PersistentMap pm = new PersistentMap();
      pm = pm.insert('a', 'b');
      pm = pm.insert('c', 'b');

      String res = '';
      pm = pm.mapValues((v) => '$v a');

      expect(pm.toMap(), equals({'a': 'b a', 'c': 'b a'}));
    });
  });

  group('Transient map', () {
    test('insert', () {
      TransientMap tm = new TransientMap();
      tm.doInsert('a', 'b');
      expect(tm.toMap(), equals({'a': 'b'}));

      tm.doInsert('a', 'c');
      expect(tm.toMap(), equals({'a': 'c'}));

      tm.doInsert('a', 'b', (a, b) => '$a$b');
      expect(tm.toMap(), equals({'a': 'cb'}));
    });

    test('lookup', () {
      TransientMap tm = new TransientMap();
      tm.doInsert('a', 'b');
      tm.doInsert('b', 'c');

      expect(tm.lookup('a'), equals('b'));
      expect(() => tm.lookup('c'), throws);
      expect(tm.lookup('c', orElse: () => 'none'), equals('none'));
    });

    test('adjust', () {
      TransientMap tm = new TransientMap();
      tm.doInsert('a', 'b');

      expect(tm.doAdjust('a', (a) => '$a b').toMap(), equals({'a': 'b b'}));
      expect(() => tm.doAdjust('c', (a) => '$a b'), throws);
    });

    test('delete', () {
      TransientMap tm = new TransientMap();
      tm.doInsert('a', 'b');
      tm.doInsert('b', 'b');

      expect(tm.doDelete('a').toMap(), equals({'b': 'b'}));
      expect(() => tm.doDelete('c'), throws);
      expect(tm.doDelete('c', safe: true).toMap(), equals({'b': 'b'}));
    });

    test('forEachKeyValue', () {
      TransientMap tm = new TransientMap();
      tm.doInsert('a', 'b');
      tm.doInsert('c', 'b');

      String res = '';
      tm.forEachKeyValue((k,v) => res = '${res}${k}${v},');

      expect(res, equals('ab,cb,'));
    });

    test('mapValues', () {
      TransientMap tm = new TransientMap();
      tm.doInsert('a', 'b');
      tm.doInsert('c', 'b');

      String res = '';
      tm.doMapValues((v) => '$v a');

      expect(tm.toMap(), equals({'a': 'b a', 'c': 'b a'}));
    });
  });
  group('deep persistent data', () {
    test('insertIn', () {
      PersistentMap map = new PersistentMap();
      map = map.insert('a', new PersistentMap());
      PersistentMap map2 = map.insertIn(['a', 'b'], 'c');

      expect(map2 == deepPersistent({'a': {'b': 'c'}}), isTrue);
      expect(map == map2, isFalse);
      expect(map, equals(deepPersistent({'a': {}})));
    });

    test('adjustIn', () {
      PersistentMap map = new PersistentMap();
      map = map.insert('a', new PersistentMap());
      map = map.insertIn(['a', 'b'], 'c');
      PersistentMap map2 = map.adjustIn(['a', 'b'], (a) => a+'c');
      expect(map2['a'], equals(new PersistentMap.fromMap({'b': 'cc'})));
      expect(map == map2, isFalse);
      expect(map['a'], equals(new PersistentMap.fromMap({'b': 'c'})));
    });

    test('deleteIn', () {
      PersistentMap map = new PersistentMap();
      map = map.insert('a', new PersistentMap());
      map = map.insertIn(['a', 'b'], 'c');
      PersistentMap map2 = map.deleteIn(['a', 'b']);

      expect(map2['a'], equals(new PersistentMap.fromMap({})));
      expect(map == map2, isFalse);
      expect(map, equals(new PersistentMap.fromMap({'a': new PersistentMap.fromMap({'b': 'c'})})));
    });

    test('lookupIn', () {
      PersistentMap map = new PersistentMap();
      map = map.insert('a', new PersistentMap());
      map = map.insertIn(['a', 'b'], 'c');

      expect(map.lookupIn(['a', 'b']), equals('c'));
    });
  });
}
