// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

part of map_bench;

/**
 * Naive implementation of PersistentMap using a [LinkedList] of [Pair]s.
 */
class SimplePersistentMap<K, V> extends IterableBase implements PersistentMap<K, V> {
  final LinkedList<Pair<K, V>> _list;

  bool get isEmpty => _list.isNil;

  SimplePersistentMap._internal(this._list);
  factory SimplePersistentMap() =>
      new SimplePersistentMap._internal(new Nil());

  PersistentMap<K, V> insert(K key, V value, [V combine(V x, V y)]) {
    combine = (combine != null) ? combine : (V x, V y) => y;
    LinkedList<Pair<K, V>> newList() {
      LinkedListBuilder<Pair<K, V>> builder =
          new LinkedListBuilder<Pair<K, V>>();
      LinkedList<Pair<K, V>> it = _list;
      while (it.isCons) {
        Cons<Pair<K, V>> cons = it.asCons;
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
      new SimplePersistentMap._internal(_list.strictWhere((p) => p.fst != key));

  V lookup(K key, [orElse()]) {
    LinkedList<Pair<K, V>> it = _list;
    while (it.isCons) {
      Cons<Pair<K, V>> cons = it.asCons;
      Pair<K, V> elem = cons.elem;
      if (elem.fst == key) return elem.snd;
      it = cons.tail;
    }
    return orElse == null ? null : orElse();
  }

  PersistentMap mapValues(f(V)) =>
    new SimplePersistentMap._internal(
      _list.strictMap((p) => new Pair(p.fst, f(p.snd))));

  forEachKeyValue(f(K, V)) {
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
