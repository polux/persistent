// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

library map_example;
import 'packages/http/http.dart' as http;
import 'packages/persistent/persistent.dart';

run(x) => Process.divide(_entry, x);
_entry(fn) => fn();

main() async {
  final m1 = new PersistentMap.fromMap({'a': 3, 'c': 4});
  final m2 = new PersistentMap.fromMap({'a': 4, 'b': 3});
  final m3 = m1.union(m2);
  print(m3);
  print(isImmutable(m3));
  print(run([() => m1, () => m2]));
}
