// Copyright (c) 2013, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

library option_test;

import 'package:persistent/persistent.dart';
import 'package:unittest/unittest.dart';

main() {
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

      expect(map.lookupIn(['a', 'b']).asNullable, equals('c'));
    });
  });
}
