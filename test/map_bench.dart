// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

library map_bench;

import 'package:persistent/persistent.dart';

import 'dart:math';

part 'bench_src/benchmark.dart';
part 'bench_src/simple_map_1.dart';
part 'bench_src/simple_map_2.dart';

void main() {
  Benchmark.warmup();
  for (int n = 0; n < 20000; n += 100) {
    Benchmark benchmark = new Benchmark(n);
    print("$n: ${benchmark.bench()}");
  }
}
