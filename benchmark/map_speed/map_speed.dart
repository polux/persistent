// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

library map_bench;

import 'package:vacuum_persistent/persistent.dart';
import 'package:benchmark_harness/benchmark_harness.dart';
import 'dart:math';

part 'benchmarks.dart';
part 'interface.dart';
part 'interface_impl.dart';

var interfaces = {
  "PersistentMap": () => new PersistentMapInterface(),
  "TransientMap": () => new TransientMapInterface(),
  "Map": () => new StandardMapInterface(),
};

preciseBenchmark(result, times){
  var res = 0;
  var ressqr = 0;
  for (int i=0; i<times; i++){
    var _res = result();
    res += _res;
    ressqr += _res * _res;
  }
  res /= times;
  ressqr /= times;
  return [res, sqrt(ressqr-res*res)];
}

var sizes = [{1000:6, 2000: 3, 3000: 2, 6000: 1}];
int times = 10;

void main() {
  var config =
  {'Write': ((sample, factory) => (new WriteBenchmark(sample, factory))),
   'Read': ((sample, factory) => (new ReadBenchmark(sample, factory))),
  };
  config.forEach((mode, creator){
    for (Map sample in sizes) {
      List<double> unit =
        preciseBenchmark(() => creator(sample, interfaces['Map']).measure(),
        times);
      for (String name in interfaces.keys) {
        List<double> res = preciseBenchmark(
          () => creator(sample, interfaces[name]).measure(),
          times);
        print('${mode} ${name} sample ${sample}: ${res[0]/unit[0]} ${res[0]} us +- ${res[1]/res[0]*100} %');
      }
      print('');
    }
  });


}
