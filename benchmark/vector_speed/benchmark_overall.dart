// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

part of vector_speed;

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
      object.push("${i}".padLeft(8,"0"));
    }

    for (int i = size-1; i >= 0; i--) {
      object.get(i);
    }
    
    for (int i = size-1; i >= 0; i--) {
      object.set(i, "${i}".padLeft(8,"_"));
    }
    
    for (int i = 0; i < size; i++) {
      object.get(i);
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
