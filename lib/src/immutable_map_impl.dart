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

/**
 * Exception used for aborting forEach loops.
 */
class _Stop implements Exception {}

/**
 * Superclass for _EmptyMap, _Leaf and _SubMap.
 */
abstract class _AImmutableMap<K extends Hashable,V>
    extends ImmutableMapBase<K,V> {
  abstract bool _isEmpty();
  abstract bool _isLeaf();

  abstract Option<V> _lookup(K key, int hash, int depth);
  abstract ImmutableMap<K,V> _insertWith(
      LList<Pair<K,V>> keyValues, V combine(V x, V y), int hash, int depth);
  abstract ImmutableMap<K,V> _delete(K key, int hash, int depth);
  abstract ImmutableMap<K,V> _adjust(K key, V update(V), int hash, int depth);

  abstract _AImmutableMap<K,V>
      _unionWith(_AImmutableMap<K,V> m, V combine(V x, V y), int depth);
  abstract _AImmutableMap<K,V>
      _unionWithEmptyMap(_EmptyMap<K,V> m, V combine(V x, V y), int depth);
  abstract _AImmutableMap<K,V>
      _unionWithLeaf(_Leaf<K,V> m, V combine(V x, V y), int depth);
  abstract _AImmutableMap<K,V>
      _unionWithSubMap(_SubMap<K,V> m, V combine(V x, V y), int depth);

  LList<Pair<K,V>> _onePair(K key, V value) =>
      new LList<Pair<K,V>>.cons(new Pair<K,V>(key, value),
          new LList<Pair<K,V>>.nil());

  Option<V> lookup(K key) =>
      _lookup(key, (key.hashCode() >> 2) & 0x3fffffff, 0);

  ImmutableMap<K,V> insert(K key, V value, [V combine(V x, V y)]) =>
      _insertWith(_onePair(key, value),
          (combine != null) ? combine : (V x, V y) => y,
          (key.hashCode() >> 2) & 0x3fffffff, 0);

  ImmutableMap<K,V> delete(K key) =>
      _delete(key, (key.hashCode() >> 2) & 0x3fffffff, 0);

  ImmutableMap<K,V> adjust(K key, V update(V)) =>
      _adjust(key, update, (key.hashCode() >> 2) & 0x3fffffff, 0);

  ImmutableMap<K,V> union(ImmutableMap<K,V> other, [V combine(V x, V y)]) =>
    this._unionWith(other, (combine != null) ? combine : (V x, V y) => y, 0);
}

class _EmptyMap<K extends Hashable, V> extends _AImmutableMap<K,V> {
  bool _isEmpty() => true;
  bool _isLeaf() => false;

  Option<V> _lookup(K key, int hash, int depth) => new Option<V>.none();

  ImmutableMap<K,V> _insertWith(
      LList<Pair<K,V>> keyValues, V combine(V x, V y), int hash, int depth) =>
          new _Leaf<K,V>(hash, keyValues);

  ImmutableMap<K,V> _delete(K key, int hash, int depth) => this;

  ImmutableMap<K,V> _adjust(K key, V update(V), int hash, int depth) => this;

  ImmutableMap<K,V>
      _unionWith(ImmutableMap<K,V> m, V combine(V x, V y), int depth) => m;

  ImmutableMap<K,V>
      _unionWithEmptyMap(_EmptyMap<K,V> m, V combine(V x, V y), int depth) {
    throw "should never be called";
  }

  ImmutableMap<K,V>
      _unionWithLeaf(_Leaf<K,V> m, V combine(V x, V y), int depth) => m;

  ImmutableMap<K,V>
      _unionWithSubMap(_SubMap<K,V> m, V combine(V x, V y), int depth) => m;

  ImmutableMap mapValues(f(V)) => this;

  void forEach(f(K,V)) {}

  int size() => 0;

  bool operator ==(ImmutableMap<K,V> other) => other is _EmptyMap;

  toDebugString() => "_EmptyMap()";
}

class _Leaf<K extends Hashable, V> extends _AImmutableMap<K,V> {
  int _hash;
  LList<Pair<K, V>> _pairs;

  _Leaf(this._hash, this._pairs);

  bool _isEmpty() => false;
  bool _isLeaf() => true;

  ImmutableMap<K,V> _insertWith(
      LList<Pair<K,V>> keyValues, V combine(V x, V y), int hash, int depth) {

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
      return new _Leaf<K,V>(hash, insertPairs(keyValues, _pairs));
    } else {
      if (hash == _hash) {
        return new _Leaf<K,V>(hash, insertPairs(keyValues, _pairs));
      } else {
        int branch = (_hash >> (depth * 5)) & 0x1f;
        List<_AImmutableMap<K,V>> array = <_AImmutableMap<K,V>>[this];
        return new _SubMap<K,V>(1 << branch, array)
            ._insertWith(keyValues, combine, hash, depth);
      }
    }
  }

  ImmutableMap<K,V> _delete(K key, int hash, int depth) {
    if (hash != _hash)
      return this;
    LList<Pair<K, V>> newPairs = _pairs.filter((p) => p.fst != key);
    return newPairs.isNil()
        ? new _EmptyMap<K,V>()
        : new _Leaf<K,V>(_hash, newPairs);
  }

  ImmutableMap<K,V> _adjust(K key, V update(V), int hash, int depth) {
    LList<Pair<K,V>> adjustPairs() {
      LListBuilder<Pair<K,V>> builder = new LListBuilder<Pair<K,V>>();
      LList<Pair<K,V>> it = _pairs;
      while (!it.isNil()) {
        Cons<Pair<K,V>> cons = it.asCons();
        Pair<K,V> elem = cons.elem;
        if (elem.fst == key) {
          builder.add(new Pair<K,V>(key, update(elem.snd)));
          return builder.build(cons.tail);
        }
        builder.add(elem);
        it = cons.tail;
      }
      return builder.build();
    }

    return (hash != _hash)
        ? this
        : new _Leaf<K,V>(_hash, adjustPairs());
  }

  ImmutableMap<K,V>
      _unionWith(_AImmutableMap<K,V> m, V combine(V x, V y), int depth) =>
          m._unionWithLeaf(this, combine, depth);

  ImmutableMap<K,V>
      _unionWithEmptyMap(_EmptyMap<K,V> m, V combine(V x, V y), int depth) =>
          this;

  ImmutableMap<K,V>
      _unionWithLeaf(_Leaf<K,V> m, V combine(V x, V y), int depth) =>
          m._insertWith(_pairs, combine, _hash, depth);

  ImmutableMap<K,V>
      _unionWithSubMap(_SubMap<K,V> m, V combine(V x, V y), int depth) =>
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

  bool operator ==(ImmutableMap<K,V> other) {
    if (this === other) return true;
    if (other is! _Leaf) return false;
    if (_hash != other._hash) return false;
    Map<K,V> thisAsMap = toMap();
    int counter = 0;
    LList<Pair<K,V>> it = other._pairs;
    while (!it.isNil()) {
      Cons<Pair<K,V>> cons = it.asCons();
      Pair<K,V> elem = cons.elem;
      if (thisAsMap[elem.fst] != elem.snd)
        return false;
      counter++;
      it = cons.tail;
    }
    return thisAsMap.length == counter;
  }

  toDebugString() => "_Leaf($_hash, $_pairs)";
}

class _SubMap<K extends Hashable, V> extends _AImmutableMap<K,V> {
  int _bitmap;
  List<_AImmutableMap<K,V>> _array;
  int _size = null;

  _SubMap(this._bitmap, this._array);

  static _popcount(int n) {
    n = n - ((n >> 1) & 0x55555555);
    n = (n & 0x33333333) + ((n >> 2) & 0x33333333);
    n = (n + (n >> 4)) & 0x0F0F0F0F;
    n = n + (n >> 8);
    n = n + (n >> 16);
    return n & 0x0000003F;
  }

  bool _isEmpty() => false;
  bool _isLeaf() => false;

  Option<V> _lookup(K key, int hash, int depth) {
    int branch = (hash >> (depth * 5)) & 0x1f;
    int mask = 1 << branch;
    if ((_bitmap & mask) != 0) {
      int index = _popcount(_bitmap & (mask - 1));
      _AImmutableMap<K,V> map = _array[index];
      return map._lookup(key, hash, depth + 1);
    } else {
      return new Option<V>.none();
    }
  }

  ImmutableMap<K,V> _insertWith(
      LList<Pair<K,V>> keyValues, V combine(V x, V y), int hash, int depth) {
    int branch = (hash >> (depth * 5)) & 0x1f;
    int mask = 1 << branch;
    int index = _popcount(_bitmap & (mask - 1));

    if ((_bitmap & mask) != 0) {
      List<_AImmutableMap<K,V>> newarray =
          new List<_AImmutableMap<K,V>>.from(_array);
      _AImmutableMap<K,V> m = _array[index];
      newarray[index] = m._insertWith(keyValues, combine, hash, depth + 1);
      return new _SubMap<K,V>(_bitmap, newarray);
    } else {
      int newlength = _array.length + 1;
      List<_AImmutableMap<K,V>> newarray =
          new List<_AImmutableMap<K,V>>(newlength);
      // TODO: find out if there's a "copy array" native function somewhere
      for (int i = 0; i < index; i++) { newarray[i] = _array[i]; }
      for (int i = index; i < newlength - 1; i++) { newarray[i+1] = _array[i]; }
      newarray[index] = new _Leaf<K,V>(hash, keyValues);
      return new _SubMap<K,V>(_bitmap | mask, newarray);
    }
  }

  ImmutableMap<K,V> _delete(K key, int hash, int depth) {
    int branch = (hash >> (depth * 5)) & 0x1f;
    int mask = 1 << branch;

    if ((_bitmap & mask) != 0) {
      int index = _popcount(_bitmap & (mask - 1));
      _AImmutableMap<K,V> m = _array[index];
      _AImmutableMap<K,V> newm = m._delete(key, hash, depth + 1);
      if (m === newm) {
        return this;
      }
      if (newm._isEmpty()) {
        if (_array.length > 2) {
          int newsize = _array.length - 1;
          List<_AImmutableMap<K,V>> newarray =
              new List<_AImmutableMap<K,V>>(newsize);
          for (int i = 0; i < index; i++) { newarray[i] = _array[i]; }
          for (int i = index; i < newsize; i++) { newarray[i] = _array[i + 1]; }
          assert(newarray.length >= 2);
          return new _SubMap(_bitmap ^ mask, newarray);
        } else {
          assert(_array.length == 2);
          assert(index == 0 || index == 1);
          _AImmutableMap<K,V> onlyValueLeft = _array[1 - index];
          return onlyValueLeft._isLeaf()
              ? onlyValueLeft
              : new _SubMap(_bitmap ^ mask,
                            <_AImmutableMap<K,V>>[onlyValueLeft]);
        }
      } else if (newm._isLeaf()){
        if (_array.length == 1) {
          return newm;
        } else {
          List<_AImmutableMap<K,V>> newarray =
              new List<_AImmutableMap<K,V>>.from(_array);
          newarray[index] = newm;
          return new _SubMap(_bitmap, newarray);
        }
      } else {
        List<_AImmutableMap<K,V>> newarray =
            new List<_AImmutableMap<K,V>>.from(_array);
        newarray[index] = newm;
        return new _SubMap(_bitmap, newarray);
      }
    } else {
      return this;
    }
  }

  ImmutableMap<K,V> _adjust(K key, V update(V), int hash, int depth) {
    int branch = (hash >> (depth * 5)) & 0x1f;
    int mask = 1 << branch;
    if ((_bitmap & mask) != 0) {
      int index = _popcount(_bitmap & (mask - 1));
      _AImmutableMap<K,V> m = _array[index];
      _AImmutableMap<K,V> newm = m._adjust(key, update, hash, depth + 1);
      if (newm === m) {
        return this;
      }
      List<_AImmutableMap<K,V>> newarray =
          new List<_AImmutableMap<K,V>>.from(_array);
      newarray[index] = newm;
      return new _SubMap(_bitmap, newarray);
    } else {
      return this;
    }
  }

  ImmutableMap<K,V>
      _unionWith(_AImmutableMap<K,V> m, V combine(V x, V y), int depth) =>
          m._unionWithSubMap(this, combine, depth);

  ImmutableMap<K,V>
      _unionWithEmptyMap(_EmptyMap<K,V> m, V combine(V x, V y), int depth) =>
          this;

  ImmutableMap<K,V>
      _unionWithLeaf(_Leaf<K,V> m, V combine(V x, V y), int depth) =>
          this._insertWith(m._pairs, (V v1, V v2) => combine(v2, v1),
              m._hash, depth);

  ImmutableMap<K,V>
      _unionWithSubMap(_SubMap<K,V> m, V combine(V x, V y), int depth) {
    int ormap = _bitmap | m._bitmap;
    int andmap = _bitmap & m._bitmap;
    List<_AImmutableMap<K,V>> newarray =
        new List<_AImmutableMap<K,V>>(_popcount(ormap));
    int mask = 1, i = 0, i1 = 0, i2 = 0;
    while (mask <= _bitmap) {
      if ((andmap & mask) != 0) {
        _array[i1];
        m._array[i2];
        newarray[i] = m._array[i2]._unionWith(_array[i1], combine, depth + 1);
        i1++;
        i2++;
        i++;
      } else if ((_bitmap & mask) != 0) {
        newarray[i] = _array[i1];
        i1++;
        i++;
      } else if ((m._bitmap & mask) != 0) {
        newarray[i] = m._array[i2];
        i2++;
        i++;
      }
      mask <<= 1;
    }
    return new _SubMap<K,V>(ormap, newarray);
  }

  ImmutableMap mapValues(f(V)) {
    List<_AImmutableMap<K,V>> newarray =
        new List<_AImmutableMap<K,V>>.from(_array);
    for (int i = 0; i < _array.length; i++) {
      _AImmutableMap<K,V> mi = _array[i];
        newarray[i] = mi.mapValues(f);
    }
    return new _SubMap(_bitmap, newarray);
  }

  forEach(f(K,V)) {
    _array.forEach((mi) => mi.forEach(f));
  }

  int size() {
    if (_size == null) {
      _size = 0;
      for (_AImmutableMap<K,V> mi in _array) {
        _size += mi.size();
      }
    }
    return _size;
  }

  bool operator ==(ImmutableMap<K,V> other) {
    if (this === other) return true;
    if (other is! _SubMap) return false;
    if (_bitmap != other._bitmap) return false;
    assert(_array.length == other._array.length);
    for (int i = 0; i < _array.length; i++) {
      _AImmutableMap<K,V> mi = _array[i];
      _AImmutableMap<K,V> omi = other._array[i];
      if (mi != omi) {
        return false;
      }
    }
    return true;
  }

  toDebugString() => "_SubMap($_array)";
}
