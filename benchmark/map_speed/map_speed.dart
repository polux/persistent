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

//var wSizes = [[1,2,3,4,5,6], [1000, 2000, 3000, 5000, 8000, 13000]];
//var rSizes = [2, 10000];
//var wSizes = [[1000, 2000, 3000, 5000, 8000, 13000]];
var wSizes = [[10000]];
var rSizes = [10000];

void main() {
  for (int n in rSizes) {
    double unit;
    unit = new ReadBenchmark(n, interfaces["Map"](), '').measure();
    for (String name in interfaces.keys){
      double res = new ReadBenchmark(n, interfaces[name](), name).measure();
      print('Reading ${name} size $n: ${res/unit} ${res} us');
    }
    print('');
  }
  print('');
  for (List n in wSizes) {
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
