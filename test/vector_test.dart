// Copyright (c) 2014, VaccumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

library vector_test;

import 'package:persistent/persistent.dart';
import 'package:unittest/unittest.dart';

main() {

  group('Persistent vector', () {
    test('pushing nulls', () {
      PersistentVector v = new PersistentVector();
      v = v.push(null);
      v = v.push(47);
      expect(v, orderedEquals([null, 47]));
    });

    test('created from array of nulls', () {
      PersistentVector v = new PersistentVector.from([null, null]);
      v = v.push(null);
      v = v.push(47);
      expect(v, orderedEquals([null, null, null, 47]));
    });
  });
}