// Copyright (c) 2014, VaccumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

library map_test;

import 'package:vacuum_persistent/persistent.dart';
import 'package:unittest/unittest.dart';
import 'dart:math';


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

//    test('forEachKeyValue', () {
//      PersistentMap pm = new PersistentMap();
//      pm = pm.assoc('a', 'b');
//      pm = pm.assoc('c', 'b');
//
//      String res = '';
//      pm.forEachKeyValue((k,v) => res = '${res}${k}${v},');
//
//      expect(res, equals('ab,cb,'));
//    });
  });

  group('random', (){
    Random r = new Random(47);
    PersistentMap pm = new PersistentMap();
    Map m = {};

    for (int i=0; i<1000000; i++){
      var key = r.nextInt(10000);
      if (r.nextBool()){
        pm = pm.delete(key, missingOk: true);
        m.remove(key);
      } else {
        var val = 'hello${r.nextInt(100000)}';
        pm = pm.assoc(key, val);
        m[key] = val;
      }
      print("round: $i, size: ${pm.length}");
      if (pm != new PersistentMap.fromMap(m)){
        print(m.length);
        print((new PersistentMap.fromMap(m)).length);
        print(pm.length);
        print(pm);
        print(m);
        expect(false, isTrue);
      }
    }
  });

}
