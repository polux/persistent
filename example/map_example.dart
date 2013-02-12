// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

library map_example;

import 'package:persistent/persistent.dart';

main() {
  final emptyMap = new PersistentMap<String,int>();
  final m1 = emptyMap.insert('a', 1).insert('b', 2);
  final m2 = new PersistentMap<String,int>.fromMap({'a': 3, 'c': 4});

  print(m1);  // {a: 1, b: 2}
  print(m2);  // {c: 4, a: 3}
  print(m1.lookup('a'));  // Option.some(1)
  print(m1.lookup('c'));  // Option.none()

  final m3 = m1.delete('a');
  print(m1);  // {a: 1, b: 2}
  print(m3);  // {b: 2}

  final m4 = m1.union(m2, (n,m) => n + m);
  print(m4);  // {c: 4, a: 4, b: 2}

  final m5 = m1.mapValues((n) => n + 1);
  print(m5);  // {a: 2, b: 3}

  final m6 = m1.adjust('a', (n) => n + 1);
  print(m6);  // {a: 2, b: 2}

  for (final pair in m4) {
    print(pair);
  }
}
