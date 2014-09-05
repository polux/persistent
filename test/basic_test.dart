// Copyright (c) 2014, VaccumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

library basic_test;

import 'package:persistent/persistent.dart';
import 'package:unittest/unittest.dart';

main() {
  run();
}

run() {
  group('Basics', () {

    test('PersistentMap#lookup', (){
      PersistentMap map = new PersistentMap();
      map = map.insert('key1', 'val1');
      for (var _map in [map, map.asTransient()]) {
        expect(_map.lookup('key1'), equals('val1'));
        expect(() => _map.lookup('key2'), throws);
      }
    });

    test('Maps#containsKey', () {
      PersistentMap map = new PersistentMap();
      map = map.insert('key1', 'val1');
      map = map.insert('key2', 'val2');
      map = map.insert('key3', 'val3');
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
}