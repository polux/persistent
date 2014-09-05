// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

part of map_bench;

abstract class BenchmarkInterface<K, V>{

  void create();
  void insert(K key, V value, V combine(V left, V right));
  void lookup(K key);
  void delete(K key);

  void save();
  void restore();
}


abstract class EncapsulatingInterface<K, V, T>
  extends BenchmarkInterface<K, V>{

  T object = null;
  T object_copy = null;

  T _copy();

  save(){
    object_copy = _copy();
  }

  restore(){
    return object_copy;
  }
}
