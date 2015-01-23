// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

part of map_bench;

class WriteBenchmark extends BenchmarkBase{

  final Map<num, num> sample;
  BenchmarkInterface object;
  final dynamic factory;

  void setup(){
    object = factory();
  }

  WriteBenchmark(this.sample, this.factory):super('Writing');

  void run(){
    for (var size in this.sample.keys) {
      for (var j=0; j<this.sample[size]; j++){
        object = factory();
        for (var val in ["foo", "bar", "baz", "woo", "hoo", "goo", "wat"]){
          for (int i = 0; i < size; i++) {
            object.assoc(i*i, val);
          }
        }
        for (int i = 0; i < size; i++) {
          object.delete(i*i);
        }
      }
    }
  }
}


class ReadBenchmark extends BenchmarkBase{

  final Map<num, num> sample;
  Map<num, List<BenchmarkInterface>> objects = new Map();
  final dynamic factory;

  ReadBenchmark(this.sample, this.factory):super('Reading');

  void setup(){
    this.sample.forEach((size, count){
      objects[size] = [];
      for (int j=0; j<count; j++){
        BenchmarkInterface object = factory();
        objects[size].add(object);
        for (int i = 0; i < size; i++) {
          object.assoc(i*i, "foo");
        }
      }
    });
  }

  void run(){
    for (var size in this.sample.keys) {
      for (var j=0; j<this.sample[size]; j++){
        BenchmarkInterface object = objects[size][j];
        for (int i = size; i >= 0; i--) {
          object.get(i*i);
        }
      }
    }
  }
}


