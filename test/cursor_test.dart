// Copyright (c) 2014, VaccumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

library cursor_test;

import 'package:vacuum_persistent/persistent.dart';
import 'package:unittest/unittest.dart';

main() {
  run();
}

eqPer(x) => equals(persist(x));

run() {
  Reference r;
  Cursor c;
  setUp(() {
    r = new Reference(per({'a': {'b' : 10}}));
    c = r.cursor;
  });

  test('Create top cursor from reference', () {
    expect(c.deref(), eqPer(r.value));
  });

  test('Create cursor with path from reference', () {
    var c = new Cursor(r, ['a']);
    expect(c.deref(), eqPer({'b' : 10}));
  });

  test('Create cursor from existing cursor one level', () {
    var c1 = c['a'];
    expect(c1.deref(), eqPer({'b': 10}));
  });

  test('Create cursor from existing cursor more level', () {
    var c1 = c['a']['b'];
    expect(c1.deref(), eqPer(10));
  });

  test('Derref cursor works when ref value changed', () {
    var c1 = c['a'];
    expect(c.deref(), eqPer({'a': {'b' : 10}}));
    expect(c1.deref(), eqPer({'b' : 10}));

    r.value = per({'c': 10, 'a': 15});
    expect(c.deref(), eqPer({'c': 10, 'a': 15}));
    expect(c1.deref(), eqPer(15));
  });

  test('Cursor with invalid path work', () {
    var c1 = c['c']['d'];
    expect(() => c1.deref(), throws);
    expect(c1.deref(null), null);

    r.value = per({'c': {'d': {'e': 15}}});
    expect(c1.deref(), per({'e': 15}));
  });

  test('Update on cursor work', () {
    var c1 = c['a'];
    c1.update((x) => assoc(x, 'g', 17));
    expect(r.value, eqPer({'a': {'b' : 10, 'g': 17}}));
  });

  test('Update on invalid cursor work', () {
    var c2 = c['a']['h'];
    c2.update(([x]) => 15);
    expect(r.value, eqPer({'a': {'b' : 10, 'h': 15}}));

    var c3 = c['a']['j']['k'];
    expect(() => c3.update(([x]) => 15), throws);
  });
}