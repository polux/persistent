// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

part of map_bench;

class OverallBenchmark extends BenchmarkBase{
  
  final int size;
  final BenchmarkInterface object;
  
  
  OverallBenchmark(size, object, name):
    size = size,
    object = object,
    super("$name($size)");
  
  
  void run(){
    
    object.create();
    
    for (int i = 0; i < size; i++) {
      object.insert("key$i", "foo", (String x, String y) => x + y);
      //object.insert("key$i", "bar", (String x, String y) => x + y);
    }

    for (int i = size * 2; i >= 0; i--) {
      object.lookup("key$i");
    }
    
    for (int i = 0; i <= size * 2; i++) {
      object.lookup("key$i");
    }

    object.save();
    for (int i = size * 2; i >= 0; i--) {
      object.delete("key$i");
    }
    
    object.restore();
    for (int i = 0; i < size * 2; i++) {
      object.delete("key$i");
    }
  }
}
