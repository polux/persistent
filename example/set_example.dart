// Copyright 2012 Google Inc. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Author: Paul Brauner (polux@google.com)

library map_example;

import 'package:persistent/persistent.dart';

main() {
  final emptySet = new PersistentSet<String>();
  final s1 = emptySet.insert("a").insert("b");
  final s2 = new PersistentSet<String>.fromSet(new Set.from(["a", "c"]));

  print(s1);  // {a, b}
  print(s2);  // {a, c}
  print(s1.contains("a"));  // true
  print(s1.contains("c"));  // false

  final s3 = s1.delete("a");
  print(s1);  // {a, b}
  print(s3);  // {b}

  final s4 = s1 + s2;
  print(s4);  // {a, b, c}

  final s5 = s1.map((s) => "prefix_$s");
  print(s5);  // {prefix_a, prefix_b}

  final s6 = s1 - s2;
  print(s6);  // {b}

  final s7 = s1 * s2;
  print(s7);  // {Pair(a, c), Pair(a, a), Pair(b, c), Pair(b, a)}

  final s8 = s1.intersection(s2);
  print(s8);  // {a}
}
