// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

library map_bench;

import 'package:vacuum_persistent/persistent.dart';
import 'package:benchmark_harness/benchmark_harness.dart';

part 'benchmarks.dart';
part 'interface.dart';
part 'interface_impl.dart';

var interfaces = {
  "PersistentMap": () => new PersistentMapInterface(),
  "TransientMap": () => new TransientMapInterface(),
  "Map": () => new StandardMapInterface(),
};

//var sizes = [2,10,10000];
var sizes = [10000];

void main() {
//  for (int n in sizes) {
//    double unit;
//    unit = new ReadBenchmark(n, interfaces["Map"](), '').measure();
//    unit = new ReadBenchmark(n, interfaces["Map"](), '').measure();
//    for (String name in interfaces.keys){
//      double res = new ReadBenchmark(n, interfaces[name](), name).measure();
//      print('Reading ${name} size $n: ${res/unit} ${res} us');
//    }
//    print('');
//  }
//  print('');
  for (int n in sizes) {
    double unit;
    unit = new WriteBenchmark(n, interfaces["Map"](), '').measure();
    unit = new WriteBenchmark(n, interfaces["Map"](), '').measure();
    for (String name in interfaces.keys){
      double res = new WriteBenchmark(n, interfaces[name](), name).measure();
      print('Writing ${name} size $n: ${res/unit} ${res} us');
    }
    print('');
  }

}
