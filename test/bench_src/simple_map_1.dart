// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

part of map_bench;

/**
 * Naive implementation of PersistentMap using a [LList] of [Pair]s.
 */
class SimplePersistentMap<K, V> extends PersistentMapBase<K, V> {
  final LList<Pair<K, V>> _list;

  bool get isEmpty => _list.isNil();

  SimplePersistentMap._internal(this._list);
  factory SimplePersistentMap() =>
      new SimplePersistentMap._internal(new LList.nil());

  PersistentMap<K, V> insert(K key, V value, [V combine(V x, V y)]) {
    combine = (combine != null) ? combine : (V x, V y) => y;
    LList<Pair<K, V>> newList() {
      LListBuilder<Pair<K, V>> builder = new LListBuilder<Pair<K, V>>();
      LList<Pair<K, V>> it = _list;
      while (!it.isNil()) {
        Cons<Pair<K, V>> cons = it.asCons();
        Pair<K, V> elem = cons.elem;
        if (elem.fst == key) {
          builder.add(new Pair<K, V>(key, combine(elem.snd, value)));
          return builder.build(cons.tail);
        }
        builder.add(elem);
        it = cons.tail;
      }
      builder.add(new Pair<K, V>(key, value));
      return builder.build();
    }
    return new SimplePersistentMap._internal(newList());
  }

  PersistentMap<K, V> delete(K key) =>
      new SimplePersistentMap._internal(_list.filter((p) => p.fst != key));

  Option<V> lookup(K key) {
    LList<Pair<K, V>> it = _list;
    while (!it.isNil()) {
      Cons<Pair<K, V>> cons = it.asCons();
      Pair<K, V> elem = cons.elem;
      if (elem.fst == key) return new Option<V>.some(elem.snd);
      it = cons.tail;
    }
    return new Option<V>.none();
  }

  PersistentMap mapValues(f(V)) =>
    new SimplePersistentMap._internal(
      _list.map((p) => new Pair(p.fst, f(p.snd))));

  forEach(f(K, V)) {
    Map<K, V> tmp = new Map<K, V>();
    _list.foreach((pair) {
      if (!tmp.containsKey(pair.fst))
        tmp[pair.fst] = pair.snd;
    });
    tmp.forEach(f);
  }

  String toString() => _list.toString();

  int get length => toMap().length;

  PersistentMap<K, V> union(PersistentMap<K, V> other, [V combine(V x, V y)]) {
    throw new UnsupportedError("union is not supported");
  }

  PersistentMap<K, V> intersection(PersistentMap<K, V> other,
                                   [V combine(V x, V y)]) {
    throw new UnsupportedError("intersection is not supported");
  }

  PersistentMap<K, V> adjust(K key, V update(V)) {
    throw new UnsupportedError("adjust is not supported");
  }
}
