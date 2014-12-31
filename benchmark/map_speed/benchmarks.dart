// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

part of map_bench;

class WriteBenchmark extends BenchmarkBase{

  final List<int> sizes;
  final BenchmarkInterface object;


  WriteBenchmark(this.sizes, this.object, name):super('Writing');

  void run(){
    for (int size in sizes) {
      object.create();

      for (int i = 0; i < size; i++) {
        var key = "key$i";
//        var key = persist({"key$i": "val$i"});
//        object.assoc(key, "foo");
//        object.assoc(key, "bar");
//        object.assoc(key, "baz");
//        object.assoc(key, "woo");
        object.assoc("key$i", "foo");
        object.assoc("key$i", "bar");
        object.assoc("key$i", "baz");
        object.assoc("key$i", "woo");

      }

      for (int i = 0; i < size; i++) {
        var key = "key$i";
//        var key = persist({"key$i": "val$i"});
        object.delete(key);
      }
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
      object.assoc("key$i", "foo");
    }
  }

  void run(){

    for (int i = size * 2; i >= 0; i--) {
      object.get("key$i");
    }

    for (int i = 0; i <= size * 2; i++) {
      object.get("key$i");
    }
  }
}


