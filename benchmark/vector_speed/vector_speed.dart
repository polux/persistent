// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

library vector_speed;

import 'package:persistent/persistent.dart';
import 'package:benchmark_harness/benchmark_harness.dart';

part 'benchmarks.dart';
part 'interface.dart';
part 'interface_impl.dart';

var interfaces = {
  "PersistentVector": () => new PersistentVectorInterface(),
  "TransientVector": () => new TransientVectorInterface(),
  "List": () => new ListInterface(),
};

void main() {

  for (int n in [1,10,100,1000,10000]) {
    for (String name in interfaces.keys){
      new ReadBenchmark(n, interfaces[name](), name).report();
    }
  }

  for (int n in [1,10,100,1000,10000]) {
    for (String name in interfaces.keys){
      new WriteBenchmark(n, interfaces[name](), name).report();
    }
  }

}
