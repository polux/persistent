// Copyright 2012 Google Inc. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Author: Paul Brauner (polux@google.com)

part of map_bench;

/**
 * Naive implementation of PersistentMap using a [LList] of [Pair]s.
 */
class SimplePersistentMap<K, V> extends PersistentMapBase<K, V> {
  final LList<Pair<K, V>> _list;

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
