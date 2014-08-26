// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

library map_bench;

import 'package:persistent/persistent.dart';
import 'package:benchmark_harness/benchmark_harness.dart';

part 'src/benchmark.dart';
part 'src/interface.dart';
part 'src/interface_impl.dart';

var interfaces = {
  "LinkedList": () => new LinkedListInterface(),
  "PersistentMap": () => new PersistentMapInterface(),
  "TransientMap": () => new TransientMapInterface(),
  "StandartMap": () => new StandardMapInterface(),
  "CopyMap": () => new CopyMapInterface(),
};

void main() {
  
  for (int n = 0; n < 20000; n += 100) {
    for (String name in interfaces.keys){
      new OverallBenchmark(n, interfaces[name](), name).report();
    }
  }    

}
