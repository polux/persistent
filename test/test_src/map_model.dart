// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

part of test_util;

/**
 * Naive implementation of PersistentMap using dart:core [Map]s.
 */
class ModelMap<K, V> extends PersistentMapBase<K, V> {
  final Map<K, V> map;

  ModelMap(this.map);

  bool get isEmpty => map.isEmpty;

  PersistentMap<K, V> insert(K key, V value, [V combine(V x, V y)]) {
    combine = (combine != null) ? combine : (V x, V y) => y;
    Map<K, V> newmap = new Map<K, V>.from(map);
    newmap[key] = map.containsKey(key) ? combine(map[key], value) : value;
    return new ModelMap(newmap);
  }

  PersistentMap<K, V> delete(K key) {
    Map<K, V> newmap = new Map<K, V>.from(map);
    newmap.remove(key);
    return new ModelMap(newmap);
  }

  Option<V> lookup(K key) {
    if (map.containsKey(key)) {
      return new Option<V>.some(map[key]);
    } else {
      return new Option<V>.none();
    }
  }

  PersistentMap mapValues(f(V)) {
    Map newmap = new Map.from(map);
    map.forEach((K key, V value) {
      newmap[key] = f(value);
    });
    return new ModelMap(newmap);
  }

  PersistentMap<K, V> adjust(K key, V update(V)) {
    if (map.containsKey(key)) {
      Map newmap = new Map.from(map);
      newmap[key] = update(map[key]);
      return new ModelMap(newmap);
    }
    return this;
  }

  void forEach(f(K, V)) {
    map.forEach(f);
  }

  String toString() => map.toString();

  int get length => map.length;

  PersistentMap<K, V> union(ModelMap<K, V> other, [V combine(V x, V y)]) {
    if (combine == null) { combine = (x, y) => y; }
    Map newmap = new Map.from(map);
    other.map.forEach((K key, V value) {
      newmap[key] = newmap.containsKey(key)
          ? combine(newmap[key], value)
          : value;
    });
    return new ModelMap(newmap);
  }

  PersistentMap<K, V> intersection(ModelMap<K, V> other,
                                   [V combine(V x, V y)]) {
    if (combine == null) { combine = (x, y) => y; }
    Map newmap = new Map();
    map.forEach((K key, V value) {
      if (other.map.containsKey(key)) {
        newmap[key] = combine(value, other.map[key]);
      }
    });
    return new ModelMap(newmap);
  }
}
