// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

part of vector_speed;

class WriteBenchmark extends BenchmarkBase{

  final int size;
  final BenchmarkInterface object;


  WriteBenchmark(size, object, name):
    size = size,
    object = object,
    super("Writing $name($size)");


  void run(){

    object.create();

    for (int i = 0; i < size; i++) {
      object.push("${i}".padLeft(8,"0"));
    }

    for (int i = size-1; i >= 0; i--) {
      object.set(i, "${i}".padLeft(8,"_"));
    }

    for (int i = 0; i < size; i++) {
      object.set(i, "${i}".padLeft(8,"x"));
    }

    object.save();
    for (int i = size-1 ; i >= 0; i--) {
      object.pop();
    }

    object.restore();
    for (int i = 0; i < size; i++) {
      object.pop();
    }
  }
}

class ReadBenchmark extends BenchmarkBase{

  final int size;
  final BenchmarkInterface object;


  ReadBenchmark(size, object, name):
    size = size,
    object = object,
    super("Reading $name($size)");

  void setup(){

    object.create();

    for (int i = 0; i < size; i++) {
      object.push("${i}".padLeft(8,"0"));
    }
  }

  void run(){

    for (int i = size-1; i >= 0; i--) {
      object.get(i);
    }

    for (int i = 0; i < size; i++) {
      object.get(i);
    }
  }
}
