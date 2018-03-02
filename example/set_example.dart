// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

library map_example;

import 'package:persistent/persistent.dart';

main() {
  final emptySet = new PersistentSet<String>();
  final s1 = emptySet.insert('a').insert('b');
  final s2 = new PersistentSet<String>.from(['a', 'c']);

  print(s1); // {a, b}
  print(s2); // {a, c}
  print(s1.contains('a')); // true
  print(s1.contains('c')); // false

  final s3 = s1.delete('a');
  print(s1); // {a, b}
  print(s3); // {b}

  final s4 = s1 + s2;
  print(s4); // {a, b, c}

  final s5 = s1.map((s) => 'prefix_$s');
  print(s5); // {prefix_a, prefix_b}

  final s6 = s1 - s2;
  print(s6); // {b}

  final s7 = s1 * s2;
  print(s7); // {Pair(a, c), Pair(a, a), Pair(b, c), Pair(b, a)}

  final s8 = s1.intersection(s2);
  print(s8); // {a}

  for (final e in s4) {
    print(e); // a, b, c
  }
}
