// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

part of test_util;

/**
 * Naive implementation of PersistentMap using dart:core [Map]s.
 */
class ModelMap<K, V> extends PersistentMapBase<K, V> {
  final Map<K, V> _map;

  ModelMap(this._map);

  bool get isEmpty => _map.isEmpty;

  ModelMap<K, V> insert(K key, V value, [V combine(V x, V y)]) {
    combine = (combine != null) ? combine : (V x, V y) => y;
    Map<K, V> newmap = new Map<K, V>.from(_map);
    newmap[key] = _map.containsKey(key) ? combine(_map[key], value) : value;
    return new ModelMap(newmap);
  }

  ModelMap<K, V> delete(K key) {
    Map<K, V> newmap = new Map<K, V>.from(_map);
    newmap.remove(key);
    return new ModelMap(newmap);
  }

  Option<V> lookup(K key) {
    if (_map.containsKey(key)) {
      return new Option<V>.some(_map[key]);
    } else {
      return new Option<V>.none();
    }
  }

  ModelMap<K, U> mapValues<U>(U f(V value)) {
    Map newmap = new Map.from(_map);
    _map.forEach((K key, V value) {
      newmap[key] = f(value);
    });
    return new ModelMap(newmap);
  }

  PersistentMap<K, V> adjust(K key, V update(V value)) {
    if (_map.containsKey(key)) {
      Map newmap = new Map.from(_map);
      newmap[key] = update(_map[key]);
      return new ModelMap(newmap);
    }
    return this;
  }

  void forEachKeyValue(f(K key, V value)) {
    _map.forEach(f);
  }

  String toString() => _map.toString();

  int get length => _map.length;

  ModelMap<K, V> union(PersistentMap<K, V> other, [V combine(V x, V y)]) {
    if (combine == null) {
      combine = (x, y) => y;
    }
    Map newmap = new Map.from(_map);
    (other as ModelMap<K, V>)._map.forEach((K key, V value) {
      newmap[key] =
          newmap.containsKey(key) ? combine(newmap[key], value) : value;
    });
    return new ModelMap(newmap);
  }

  ModelMap<K, V> intersection(PersistentMap<K, V> other,
      [V combine(V x, V y)]) {
    if (combine == null) {
      combine = (x, y) => y;
    }
    Map<K, V> newmap = new Map();
    ModelMap<K, V> otherMap = other as ModelMap<K, V>;
    _map.forEach((K key, V value) {
      if (otherMap._map.containsKey(key)) {
        newmap[key] = combine(value, otherMap._map[key]);
      }
    });
    return new ModelMap(newmap);
  }

  Iterator<Pair<K, V>> get iterator {
    final res = <Pair<K, V>>[];
    this.forEachKeyValue((k, v) {
      res.add(new Pair<K, V>(k, v));
    });
    return res.iterator;
  }

  Pair<K, V> pickRandomEntry([Random random]) {
    throw new UnsupportedError("");
  }
}
