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

  bool contains(Object element) => _map.lookup(element).isDefined;

  void forEach(f(E element)) => _map.forEachKeyValue((E k, v) => f(k));

  _PersistentSetImpl<F> map<F>(F f(E element)) {
    _PersistentSetImpl result = new _PersistentSetImpl();
    _map.forEachKeyValue((E k, v) {
      result = result.insert(f(k));
    });
    return result;
  }

  _PersistentSetImpl<E> filter(bool f(E element)) {
    _PersistentSetImpl result = new _PersistentSetImpl();
    _map.forEachKeyValue((E k, v) {
      if (f(k)) {
        result = result.insert(k);
      }
    });
    return result;
  }

  int get length => _map.length;

  _PersistentSetImpl<E> union(PersistentSet<E> persistentSet) =>
      new _PersistentSetImpl._internal(
          _map.union((persistentSet as _PersistentSetImpl<E>)._map));

  _PersistentSetImpl<E> difference(PersistentSet<E> persistentSet) {
    _PersistentSetImpl result = new _PersistentSetImpl();
    _map.forEachKeyValue((E k, v) {
      if (!persistentSet.contains(k)) {
        result = result.insert(k);
      }
    });
    return result;
  }

  _PersistentSetImpl<E> intersection(PersistentSet<E> persistentSet) =>
      new _PersistentSetImpl._internal(
          _map.intersection((persistentSet as _PersistentSetImpl<E>)._map));

  _PersistentSetImpl<Pair<E, F>> cartesianProduct<F>(
      PersistentSet<F> persistentSet) {
    _PersistentSetImpl<Pair> result = new _PersistentSetImpl();
    _map.forEachKeyValue((E e1, _) {
      (persistentSet as _PersistentSetImpl<F>)._map.forEachKeyValue((e2, _) {
        result = result.insert(new Pair<E, Object>(e1, e2));
      });
    });
    return result;
  }

  bool operator ==(Object other) =>
      (other is _PersistentSetImpl<E>) && _map == other._map;

  int get hashCode => _map.hashCode;

  Iterator<E> get iterator =>
      _map.map((Pair<E, Object> pair) => pair.fst).iterator;

  E pickRandomElement([Random random]) => _map.pickRandomEntry(random).fst;

  // PersistentMap's "last" is optimized
  E get last => _map.last.fst;

  // PersistentMap's "elementAt" is optimized
  E elementAt(int index) => _map.elementAt(index).fst;
}
