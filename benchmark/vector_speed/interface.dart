// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

part of vector_speed;

abstract class BenchmarkInterface<E>{

  void create();
  void push(E value);
  void set(int index, E value);
  void get(int index);
  void pop();

  void save();
  void restore();
}


abstract class EncapsulatingInterface<E, T>
  extends BenchmarkInterface<E>{

  T object = null;
  T object_copy = null;

  T _copy();

  save(){
    object_copy = _copy();
  }

  restore(){
    object = object_copy;
  }
}
