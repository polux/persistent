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
        for (int i = 0; i < size; i++) {
          var key = "key$i";
          object.assoc("key$i", "foo");
          object.assoc("key$i", "bar");
          object.assoc("key$i", "baz");
          object.assoc("key$i", "woo");
        }
        for (int i = 0; i < size; i++) {
          var key = "key$i";
          object.delete(key);
        }
      }
    }
  }
}


class ReadBenchmark extends BenchmarkBase{

  final Map<num, num> sample;
  Map<num, BenchmarkInterface> objects = new Map();
  final dynamic factory;

  ReadBenchmark(this.sample, this.factory):super('Reading');

  void setup(){
    this.sample.forEach((size, count){
      BenchmarkInterface object = factory();
      objects[size] = object;
      for (int i = 0; i < size; i++) {
        object.assoc("key$i", "foo");
      }
    });
  }

  void run(){
    for (var size in this.sample.keys) {
      BenchmarkInterface object = objects[size];
      for (var j=0; j<this.sample[size]; j++){
        for (int i = size; i >= 0; i--) {
          object.get("key$i");
        }
        for (int i = 0; i <= size; i++) {
          object.get("key$i");
        }
      }
    }
  }
}


