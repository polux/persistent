// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

library map_bench;

import 'package:persistent/persistent.dart';
import 'package:benchmark_harness/benchmark_harness.dart';

part 'benchmark_overall.dart';
part 'interface.dart';
part 'interface_impl.dart';

var interfaces = {
  //"LinkedList": () => new LinkedListInterface(),
  "PersistentMap": () => new PersistentMapInterface(),
  "TransientMap": () => new TransientMapInterface(),
  "StandartMap": () => new StandardMapInterface(),
  //"CopyMap": () => new CopyMapInterface(),
};

void main() {

  for (int n in [1,10,100,1000,10000]) {
    for (String name in interfaces.keys){
      new OverallBenchmark(n, interfaces[name](), name).report();
    }
  }

}
