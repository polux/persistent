// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

part of map_bench;

/**
 * Naive implementation of PersistentMap dart:core [Map]s.
 */
class SimplePersistentMap2<K, V> extends PersistentMapBase<K, V> {
  final Map<K, V> _map;

  bool get isEmpty => _map.isEmpty;

  SimplePersistentMap2._internal(this._map);
  factory SimplePersistentMap2() =>
      new SimplePersistentMap2._internal(new Map<K, V>());

  PersistentMap<K, V> insert(K key, V value, [V combine(V x, V y)]) {
    combine = (combine != null) ? combine : (V x, V y) => y;
    Map<K, V> newmap = new Map<K, V>.from(_map);
    newmap[key] = _map.containsKey(key) ? combine(_map[key], value) : value;
    return new SimplePersistentMap2._internal(newmap);
  }

  PersistentMap<K, V> delete(K key) {
    Map<K, V> newmap = new Map<K, V>.from(_map);
    newmap.remove(key);
    return new SimplePersistentMap2._internal(newmap);
  }

  Option<V> lookup(K key) {
    if (_map.containsKey(key)) {
      return new Option<V>.some(_map[key]);
    } else {
      return new Option<V>.none();
    }
  }

  PersistentMap<K, U> mapValues<U>(U f(V value)) {
    Map newmap = new Map.from(_map);
    _map.forEach((K key, V value) {
      newmap[key] = f(value);
    });
    return new SimplePersistentMap2._internal(newmap);
  }

  PersistentMap<K, V> adjust(K key, V update(V value)) {
    if (_map.containsKey(key)) {
      Map newmap = new Map.from(_map);
      newmap[key] = update(_map[key]);
      return new SimplePersistentMap2._internal(newmap);
    }
    return this;
  }

  void forEachKeyValue(f(K key, V value)) {
    _map.forEach(f);
  }

  String toString() => _map.toString();

  int get length => _map.length;

  PersistentMap<K, V> union(PersistentMap<K, V> other, [V combine(V x, V y)]) {
    throw new UnsupportedError("union is not supported");
  }

  PersistentMap<K, V> intersection(PersistentMap<K, V> other,
      [V combine(V x, V y)]) {
    throw new UnsupportedError("intersection is not supported");
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
