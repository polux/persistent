// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

part of persistent;

class _PersistentSetImpl<E> extends PersistentSetBase<E> {
  final PersistentMap<E, Object> _map;

  _PersistentSetImpl._internal(this._map);

  factory _PersistentSetImpl() =>
      new _PersistentSetImpl._internal(new PersistentMap<E, Object>());

  bool get isEmpty => _map.isEmpty;

  _PersistentSetImpl<E> insert(E element) =>
      new _PersistentSetImpl._internal(_map.insert(element, null));

  _PersistentSetImpl<E> delete(E element) =>
      new _PersistentSetImpl._internal(_map.delete(element));

  bool contains(E element) => _map.lookup(element).isDefined;

  void forEach(f(E element)) => _map.forEach((E k, v) => f(k));

  _PersistentSetImpl map(f(E element)) {
    _PersistentSetImpl result = new _PersistentSetImpl();
    _map.forEach((E k, v) { result = result.insert(f(k)); });
    return result;
  }

  _PersistentSetImpl<E> filter(bool f(E element)) {
    _PersistentSetImpl result = new _PersistentSetImpl();
    _map.forEach((E k, v) {
      if (f(k)) {
        result = result.insert(k);
      }
    });
    return result;
  }

  int get length => _map.length;

  _PersistentSetImpl<E> union(_PersistentSetImpl<E> persistentSet) =>
      new _PersistentSetImpl._internal(_map.union(persistentSet._map));

  _PersistentSetImpl<E> difference(_PersistentSetImpl<E> persistentSet) {
    _PersistentSetImpl result = new _PersistentSetImpl();
    _map.forEach((E k, v) {
      if (!persistentSet.contains(k)) {
        result = result.insert(k);
      }
    });
    return result;
  }

  _PersistentSetImpl<E> intersection(_PersistentSetImpl<E> persistentSet) =>
      new _PersistentSetImpl._internal(_map.intersection(persistentSet._map));

  PersistentSet<Pair> cartesianProduct(_PersistentSetImpl<E> persistentSet) {
    _PersistentSetImpl<Pair> result = new _PersistentSetImpl();
    _map.forEach((E e1, _) {
      persistentSet._map.forEach((e2, _) {
        result = result.insert(new Pair<E,Object>(e1, e2));
      });
    });
    return result;
  }

  bool operator ==(_PersistentSetImpl<E> other) => _map == other._map;
}
