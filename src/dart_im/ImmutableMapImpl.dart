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

class _ImmutableMapFactory<K extends Hashable,V> {
  factory ImmutableMap() => new _EmptyMap();
}

/**
 * Superclass for _EmptyMap, _Leaf and _SubMap.
 */
abstract class _AImmutableMap<K extends Hashable,V> extends AImmutableMap<K,V> {
  abstract Option<V> _lookup(K key, int hash, int depth);
  abstract ImmutableMap<K,V> _insertWith(
      LList<Pair<K,V>> keyValues, V combine(V,V), int hash, int depth);
  abstract ImmutableMap<K,V> _delete(K key, int hash, int depth);
  abstract ImmutableMap<K,V> _adjust(K key, V update(V), int hash, int depth);

  abstract _AImmutableMap<K,V>
      _unionWith(_AImmutableMap<K,V> m, V combine(V,V), int depth);
  abstract _AImmutableMap<K,V>
      _unionWithEmptyMap(_EmptyMap<K,V> m, V combine(V,V), int depth);
  abstract _AImmutableMap<K,V>
      _unionWithLeaf(_Leaf<K,V> m, V combine(V,V), int depth);
  abstract _AImmutableMap<K,V>
      _unionWithSubMap(_SubMap<K,V> m, V combine(V,V), int depth);

  LList<Pair<K,V>> _onePair(K key, V value) =>
      new LList<Pair<K,V>>.cons(new Pair<K,V>(key, value),
          new LList<Pair<K,V>>.nil());

  Option<V> lookup(K key) =>
      _lookup(key, (key.hashCode() >> 2) & 0x3fffffff, 0);

  ImmutableMap<K,V> insert(K key, V value) =>
      _insertWith(_onePair(key, value), (V v1, V v2) => v2,
          (key.hashCode() >> 2) & 0x3fffffff, 0);

  ImmutableMap<K,V> delete(K key) =>
      _delete(key, (key.hashCode() >> 2) & 0x3fffffff, 0);

  ImmutableMap<K,V> adjust(K key, V update(V)) =>
      _adjust(key, update, (key.hashCode() >> 2) & 0x3fffffff, 0);

  ImmutableMap<K,V> unionWith(ImmutableMap<K,V> other, V combine(V,V)) =>
    this._unionWith(other, combine, 0);
}

class _EmptyMap<K extends Hashable, V> extends _AImmutableMap<K,V> {
  Option<V> _lookup(K key, int hash, int depth) => new Option<V>.none();

  ImmutableMap<K,V> _insertWith(
      LList<Pair<K,V>> keyValues, V combine(V,V), int hash, int depth) =>
          new _Leaf(hash, keyValues);

  ImmutableMap<K,V> _delete(K key, int hash, int depth) => this;

  ImmutableMap<K,V> _adjust(K key, V update(V), int hash, int depth) => this;

  ImmutableMap<K,V>
      _unionWith(ImmutableMap<K,V> m, V combine(V,V), int depth) => m;

  ImmutableMap<K,V>
      _unionWithEmptyMap(_EmptyMap<K,V> m, V combine(V,V), int depth) {
    throw "should never be called";
  }

  ImmutableMap<K,V>
      _unionWithLeaf(_Leaf<K,V> m, V combine(V,V), int depth) => m;

  ImmutableMap<K,V>
      _unionWithSubMap(_SubMap<K,V> m, V combine(V,V), int depth) => m;

  ImmutableMap mapValues(f(V)) => this;

  void forEach(f(K,V)) {}

  int size() => 0;
}

class _Leaf<K extends Hashable, V> extends _AImmutableMap<K,V> {
  int _hash;
  LList<Pair<K, V>> _pairs;

  _Leaf(this._hash, this._pairs);

  ImmutableMap<K,V> _insertWith(
      LList<Pair<K,V>> keyValues, V combine(V,V), int hash, int depth) {

    LList<Pair<K,V>> insertPair(Pair<K,V> toInsert, LList<Pair<K,V>> pairs) {
      LListBuilder<Pair<K,V>> builder = new LListBuilder<Pair<K,V>>();
      LList<Pair<K,V>> it = pairs;
      while (!it.isNil()) {
        Cons<Pair<K,V>> cons = it.asCons();
        Pair<K,V> elem = cons.elem;
        if (elem.fst == toInsert.fst) {
          builder.add(new Pair<K,V>(
              toInsert.fst,
              combine(elem.snd, toInsert.snd)));
          return builder.build(cons.tail);
        }
        builder.add(elem);
        it = cons.tail;
      }
      builder.add(toInsert);
      return builder.build();
    }

    LList<Pair<K,V>> insertPairs(
        LList<Pair<K,V>> toInsert, LList<Pair<K,V>> pairs) {
      LList<Pair<K,V>> res = pairs;
      LList<Pair<K,V>> it = toInsert;
      while (!it.isNil()) {
        Cons<Pair<K,V>> cons = it.asCons();
        Pair<K,V> elem = cons.elem;
        res = insertPair(elem, res);
        it = cons.tail;
      }
      return res;
    }

    if (depth > 5) {
      assert(_hash == hash);
      return new _Leaf(hash, insertPairs(keyValues, _pairs));
    } else {
      if (hash == _hash) {
        return new _Leaf(hash, insertPairs(keyValues, _pairs));
      } else {
        Map<int, _AImmutableMap<K,V>> submap =
            new Map<int, _AImmutableMap<K,V>>();
        int branch = (_hash >> (depth * 5)) & 0x1f;
        submap[branch] = this;
        return new _SubMap(submap)
            ._insertWith(keyValues, combine, hash, depth);
      }
    }
  }

  ImmutableMap<K,V> _delete(K key, int hash, int depth) {
    if (hash != _hash)
      return this;
    LList<Pair<K, V>> newPairs = _pairs.filter((p) => p.fst != key);
    return newPairs.isNil()
        ? new _EmptyMap()
        : new _Leaf(_hash, newPairs);
  }

  ImmutableMap<K,V> _adjust(K key, V update(V), int hash, int depth) {
    LList<Pair<K,V>> adjustPairs() {
      LListBuilder<Pair<K,V>> builder = new LListBuilder<Pair<K,V>>();
      LList<Pair<K,V>> it = _pairs;
      while (!it.isNil()) {
        Cons<Pair<K,V>> cons = it.asCons();
        Pair<K,V> elem = cons.elem;
        if (elem.fst == key) {
          builder.add(new Pair<K,V>(
              key,
              update(elem.snd)));
          return builder.build(cons.tail);
        }
        builder.add(elem);
        it = cons.tail;
      }
      return builder.build();
    }

    return (hash != _hash)
        ? this
        : new _Leaf(_hash, adjustPairs());
  }

  ImmutableMap<K,V>
      _unionWith(_AImmutableMap<K,V> m, V combine(V,V), int depth) =>
          m._unionWithLeaf(this, combine, depth);

  ImmutableMap<K,V>
      _unionWithEmptyMap(_EmptyMap<K,V> m, V combine(V,V), int depth) =>
          this;

  ImmutableMap<K,V>
      _unionWithLeaf(_Leaf<K,V> m, V combine(V,V), int depth) =>
          m._insertWith(_pairs, combine, _hash, depth);

  ImmutableMap<K,V>
      _unionWithSubMap(_SubMap<K,V> m, V combine(V,V), int depth) =>
          m._insertWith(_pairs, combine, _hash, depth);

  Option<V> _lookup(K key, int hash, int depth) {
    if (hash != _hash)
      return new Option<V>.none();
    LList<Pair<K,V>> it = _pairs;
    while (!it.isNil()) {
      Cons<Pair<K,V>> cons = it.asCons();
      Pair<K,V> elem = cons.elem;
      if (elem.fst == key) return new Option<V>.some(elem.snd);
      it = cons.tail;
    }
    return new Option<V>.none();
  }

  ImmutableMap mapValues(f(V)) =>
      new _Leaf(_hash, _pairs.map((p) => new Pair(p.fst, f(p.snd))));

  void forEach(f(K,V)) {
    _pairs.foreach((Pair<K,V> pair) => f(pair.fst, pair.snd));
  }

  // no need to cache the size since it is already cached in _pairs
  int size() => _pairs.length();
}

class _SubMap<K extends Hashable, V> extends _AImmutableMap<K,V> {
  Map<int, _AImmutableMap<K,V>> _submap;
  int _size = null;

  _SubMap(this._submap);

  Option<V> _lookup(K key, int hash, int depth) {
    int branch = (hash >> (depth * 5)) & 0x1f;
    if (_submap.containsKey(branch)) {
      _AImmutableMap<K,V> map = _submap[branch];
      return map._lookup(key, hash, depth + 1);
    } else {
      return new Option<V>.none();
    }
  }

  ImmutableMap<K,V> _insertWith(
      LList<Pair<K,V>> keyValues, V combine(V,V), int hash, int depth) {
    Map<int, _AImmutableMap<K,V>> newsubmap =
        new Map<int, _AImmutableMap<K,V>>.from(_submap);
    int branch = (hash >> (depth * 5)) & 0x1f;
    if (_submap.containsKey(branch)) {
      _AImmutableMap<K,V> m = _submap[branch];
      newsubmap[branch] = m._insertWith(keyValues, combine, hash, depth + 1);
    } else {
      newsubmap[branch] = new _Leaf(hash, keyValues);
    }
    return new _SubMap(newsubmap);
  }

  ImmutableMap<K,V> _delete(K key, int hash, int depth) {
    int branch = (hash >> (depth * 5)) & 0x1f;
    if (_submap.containsKey(branch)) {
      _AImmutableMap<K,V> m = _submap[branch];
      _AImmutableMap<K,V> newm = m._delete(key, hash, depth + 1);
      Map<int, _AImmutableMap<K,V>> newsubmap =
          new Map<int, _AImmutableMap<K,V>>.from(_submap);
      newsubmap[branch] = newm;
      return new _SubMap<K,V>(newsubmap);
    } else {
      return this;
    }
  }

  ImmutableMap<K,V> _adjust(K key, V update(V), int hash, int depth) {
    int branch = (hash >> (depth * 5)) & 0x1f;
    if (_submap.containsKey(branch)) {
      _AImmutableMap<K,V> m = _submap[branch];
      _AImmutableMap<K,V> newm = m._adjust(key, update, hash, depth + 1);
      Map<int, _AImmutableMap<K,V>> newsubmap =
          new Map<int, _AImmutableMap<K,V>>.from(_submap);
      newsubmap[branch] = newm;
      return new _SubMap<K,V>(newsubmap);
    } else {
      return this;
    }
  }

  ImmutableMap<K,V>
      _unionWith(_AImmutableMap<K,V> m, V combine(V,V), int depth) =>
          m._unionWithSubMap(this, combine, depth);

  ImmutableMap<K,V>
      _unionWithEmptyMap(_EmptyMap<K,V> m, V combine(V,V), int depth) =>
          this;

  ImmutableMap<K,V>
      _unionWithLeaf(_Leaf<K,V> m, V combine(V,V), int depth) =>
          this._insertWith(m._pairs, (V v1, V v2) => combine(v2, v1),
              m._hash, depth);

  ImmutableMap<K,V>
      _unionWithSubMap(_SubMap<K,V> m, V combine(V,V), int depth) {
    Map<int, _AImmutableMap<K,V>> newsubmap =
        new Map<int, _AImmutableMap<K,V>>();
    Set<int> allKeys = new Set<int>.from(_submap.getKeys());
    allKeys.addAll(m._submap.getKeys());
    allKeys.forEach((int key) {
      if (_submap.containsKey(key)) {
        if (m._submap.containsKey(key)) {
          newsubmap[key] =
              m._submap[key]._unionWith(_submap[key], combine, depth + 1);
        } else {
          newsubmap[key] = _submap[key];
        }
      } else {
        newsubmap[key] = m._submap[key];
      }
    });
    return new _SubMap<K,V>(newsubmap);
  }

  ImmutableMap mapValues(f(V)) {
    Map newsubmap = new Map();
    _submap.forEach((int i, _AImmutableMap<K,V> m) {
      newsubmap[i] = m.mapValues(f);
    });
    return new _SubMap(newsubmap);
  }

  forEach(f(K,V)) {
    _submap.forEach((int _, _AImmutableMap<K,V> m) => m.forEach(f));
  }

  int size() {
    if (_size == null) {
      _size = 0;
      _submap.forEach((int _, _AImmutableMap<K,V> m) { _size += m.size(); });
    }
    return _size;
  }
}
