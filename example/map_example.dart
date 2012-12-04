// Copyright 2012 Google Inc. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the 'License');
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an 'AS IS' BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

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
}
