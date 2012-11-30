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

part of persistent;

/**
 * Exception used for aborting forEach loops.
 */
class _Stop implements Exception {}

/**
 * Superclass for _EmptyMap, _Leaf and _SubMap.
 */
abstract class _APersistentMap<K, V> extends PersistentMapBase<K, V> {
  final int length;

  _APersistentMap(this.length);

  bool _isEmpty();
  bool _isLeaf();

  Option<V> _lookup(K key, int hash, int depth);
  PersistentMap<K, V> _insertWith(LList<Pair<K, V>> keyValues, int size,
      V combine(V x, V y), int hash, int depth);
  PersistentMap<K, V> _intersectWith(LList<Pair<K, V>> keyValues, int size,
      V combine(V x, V y), int hash, int depth);
  PersistentMap<K, V> _delete(K key, int hash, int depth);
  PersistentMap<K, V> _adjust(K key, V update(V), int hash, int depth);

  _APersistentMap<K, V>
      _unionWith(_APersistentMap<K, V> m, V combine(V x, V y), int depth);
  _APersistentMap<K, V>
      _unionWithEmptyMap(_EmptyMap<K, V> m, V combine(V x, V y), int depth);
  _APersistentMap<K, V>
      _unionWithLeaf(_Leaf<K, V> m, V combine(V x, V y), int depth);
  _APersistentMap<K, V>
      _unionWithSubMap(_SubMap<K, V> m, V combine(V x, V y), int depth);

  _APersistentMap<K, V>
      _intersectionWith(_APersistentMap<K, V> m, V combine(V x, V y),
                        int depth);
  _APersistentMap<K, V>
      _intersectionWithEmptyMap(_EmptyMap<K, V> m, V combine(V x, V y),
                                int depth);
  _APersistentMap<K, V>
      _intersectionWithLeaf(_Leaf<K, V> m, V combine(V x, V y), int depth);
  _APersistentMap<K, V>
      _intersectionWithSubMap(_SubMap<K, V> m, V combine(V x, V y), int depth);

  LList<Pair<K, V>> _onePair(K key, V value) =>
      new LList<Pair<K, V>>.cons(new Pair<K, V>(key, value),
          new LList<Pair<K, V>>.nil());

  Option<V> lookup(K key) =>
      _lookup(key, key.hashCode & 0x3fffffff, 0);

  PersistentMap<K, V> insert(K key, V value, [V combine(V x, V y)]) =>
      _insertWith(_onePair(key, value),
          1,
          (combine != null) ? combine : (V x, V y) => y,
          key.hashCode & 0x3fffffff, 0);

  PersistentMap<K, V> delete(K key) =>
      _delete(key, key.hashCode & 0x3fffffff, 0);

  PersistentMap<K, V> adjust(K key, V update(V)) =>
      _adjust(key, update, key.hashCode & 0x3fffffff, 0);

  PersistentMap<K, V> union(PersistentMap<K, V> other, [V combine(V x, V y)]) =>
    this._unionWith(other, (combine != null) ? combine : (V x, V y) => y, 0);

  PersistentMap<K, V>
      intersection(PersistentMap<K, V> other, [V combine(V left, V right)]) =>
    this._intersectionWith(other,
        (combine != null) ? combine : (V x, V y) => y, 0);

  // toString() => toDebugString();
}

class _EmptyMap<K, V> extends _APersistentMap<K, V> {
  _EmptyMap() : super(0);

  bool _isEmpty() => true;
  bool _isLeaf() => false;

  Option<V> _lookup(K key, int hash, int depth) => new Option<V>.none();

  PersistentMap<K, V> _insertWith(
      LList<Pair<K, V>> keyValues, int size, V combine(V x, V y), int hash,
      int depth) {
    assert(size == keyValues.length());
    return new _Leaf<K, V>(hash, keyValues, size);
  }

  PersistentMap<K, V> _intersectWith(LList<Pair<K, V>> keyValues, int size,
      V combine(V x, V y), int hash, int depth) {
    assert(size == keyValues.length());
    return this;
  }

  PersistentMap<K, V> _delete(K key, int hash, int depth) => this;

  PersistentMap<K, V> _adjust(K key, V update(V), int hash, int depth) => this;

  PersistentMap<K, V>
      _unionWith(PersistentMap<K, V> m, V combine(V x, V y), int depth) => m;

  PersistentMap<K, V>
      _unionWithEmptyMap(_EmptyMap<K, V> m, V combine(V x, V y), int depth) {
    throw "should never be called";
  }

  PersistentMap<K, V>
      _unionWithLeaf(_Leaf<K, V> m, V combine(V x, V y), int depth) => m;

  PersistentMap<K, V>
      _unionWithSubMap(_SubMap<K, V> m, V combine(V x, V y), int depth) => m;

  PersistentMap<K, V>
      _intersectionWith(_APersistentMap<K, V> m, V combine(V x, V y),
                        int depth) => this;

  PersistentMap<K, V>
      _intersectionWithEmptyMap(_EmptyMap<K, V> m, V combine(V x, V y),
                                int depth) {
        throw "should never be called";
  }

  PersistentMap<K, V> _intersectionWithLeaf(
      _Leaf<K, V> m, V combine(V x, V y), int depth) => this;

  PersistentMap<K, V> _intersectionWithSubMap(
      _SubMap<K, V> m, V combine(V x, V y), int depth) => this;

  PersistentMap mapValues(f(V)) => this;

  void forEach(f(K, V)) {}

  bool operator ==(PersistentMap<K, V> other) => other is _EmptyMap;

  toDebugString() => "_EmptyMap()";
}

class _Leaf<K, V> extends _APersistentMap<K, V> {
  int _hash;
  LList<Pair<K, V>> _pairs;

  _Leaf(this._hash, pairs, int size) : super(size) {
    this._pairs = pairs;
    assert(size == pairs.length());
  }

  bool _isEmpty() => false;
  bool _isLeaf() => true;

  PersistentMap<K, V> _insertWith(LList<Pair<K, V>> keyValues, int size,
      V combine(V x, V y), int hash, int depth) {
    assert(size == keyValues.length());
    // newsize is incremented as a side effect of insertPair
    int newsize = length;

    LList<Pair<K, V>> insertPair(Pair<K, V> toInsert, LList<Pair<K, V>> pairs) {
      LListBuilder<Pair<K, V>> builder = new LListBuilder<Pair<K, V>>();
      LList<Pair<K, V>> it = pairs;
      while (!it.isNil()) {
        Cons<Pair<K, V>> cons = it.asCons();
        Pair<K, V> elem = cons.elem;
        if (elem.fst == toInsert.fst) {
          builder.add(new Pair<K, V>(
              toInsert.fst,
              combine(elem.snd, toInsert.snd)));
          return builder.build(cons.tail);
        }
        builder.add(elem);
        it = cons.tail;
      }
      builder.add(toInsert);
      newsize++;
      return builder.build();
    }

    LList<Pair<K, V>> insertPairs(
        LList<Pair<K, V>> toInsert, LList<Pair<K, V>> pairs) {
      LList<Pair<K, V>> res = pairs;
      LList<Pair<K, V>> it = toInsert;
      while (!it.isNil()) {
        Cons<Pair<K, V>> cons = it.asCons();
        Pair<K, V> elem = cons.elem;
        res = insertPair(elem, res);
        it = cons.tail;
      }
      assert(newsize == res.length());
      return res;
    }

    if (depth > 5) {
      assert(_hash == hash);
      final LList<Pair<K, V>> newPairs = insertPairs(keyValues, _pairs);
      return new _Leaf<K, V>(hash, newPairs, newsize);
    } else {
      if (hash == _hash) {
        final LList<Pair<K, V>> newPairs = insertPairs(keyValues, _pairs);
        return new _Leaf<K, V>(hash, newPairs, newsize);
      } else {
        int branch = (_hash >> (depth * 5)) & 0x1f;
        List<_APersistentMap<K, V>> array = <_APersistentMap<K, V>>[this];
        return new _SubMap<K, V>(1 << branch, array, length)
            ._insertWith(keyValues, size, combine, hash, depth);
      }
    }
  }

  PersistentMap<K, V> _intersectWith(LList<Pair<K, V>> keyValues, int size,
      V combine(V x, V y), int hash, int depth) {
    assert(size == keyValues.length());
    // TODO(polux): possibly faster implementation
    Map<K, V> map = toMap();
    LListBuilder<Pair<K, V>> builder = new LListBuilder<Pair<K, V>>();
    int newsize = 0;
    keyValues.foreach((Pair<K, V> pair) {
      if (map.containsKey(pair.fst)) {
        builder.add(new Pair<K, V>(pair.fst, combine(map[pair.fst], pair.snd)));
        newsize++;
      }
    });
    return new _Leaf(_hash, builder.build(), newsize);
  }

  PersistentMap<K, V> _delete(K key, int hash, int depth) {
    if (hash != _hash)
      return this;
    bool found = false;
    LList<Pair<K, V>> newPairs = _pairs.filter((p) {
      if (p.fst == key) {
        found = true;
        return false;
      }
      return true;
    });
    return newPairs.isNil()
        ? new _EmptyMap<K, V>()
        : new _Leaf<K, V>(_hash, newPairs, found ? length - 1 : length);
  }

  PersistentMap<K, V> _adjust(K key, V update(V), int hash, int depth) {
    LList<Pair<K, V>> adjustPairs() {
      LListBuilder<Pair<K, V>> builder = new LListBuilder<Pair<K, V>>();
      LList<Pair<K, V>> it = _pairs;
      while (!it.isNil()) {
        Cons<Pair<K, V>> cons = it.asCons();
        Pair<K, V> elem = cons.elem;
        if (elem.fst == key) {
          builder.add(new Pair<K, V>(key, update(elem.snd)));
          return builder.build(cons.tail);
        }
        builder.add(elem);
        it = cons.tail;
      }
      return builder.build();
    }

    return (hash != _hash)
        ? this
        : new _Leaf<K, V>(_hash, adjustPairs(), length);
  }

  PersistentMap<K, V>
      _unionWith(_APersistentMap<K, V> m, V combine(V x, V y), int depth) =>
          m._unionWithLeaf(this, combine, depth);

  PersistentMap<K, V>
      _unionWithEmptyMap(_EmptyMap<K, V> m, V combine(V x, V y), int depth) =>
          this;

  PersistentMap<K, V>
      _unionWithLeaf(_Leaf<K, V> m, V combine(V x, V y), int depth) =>
          m._insertWith(_pairs, length, combine, _hash, depth);

  PersistentMap<K, V>
      _unionWithSubMap(_SubMap<K, V> m, V combine(V x, V y), int depth) =>
          m._insertWith(_pairs, length, combine, _hash, depth);

  PersistentMap<K, V> _intersectionWith(_APersistentMap<K, V> m,
                                        V combine(V x, V y), int depth) =>
      m._intersectionWithLeaf(this, combine, depth);

  PersistentMap<K, V> _intersectionWithEmptyMap(_EmptyMap<K, V> m,
                                                V combine(V x, V y),
                                                int depth) =>
      m;

  PersistentMap<K, V> _intersectionWithLeaf(_Leaf<K, V> m, V combine(V x, V y),
                                            int depth) =>
      m._intersectWith(_pairs, length, combine, _hash, depth);

  PersistentMap<K, V> _intersectionWithSubMap(_SubMap<K, V> m,
                                              V combine(V x, V y), int depth) =>
      m._intersectWith(_pairs, length, combine, _hash, depth);

  Option<V> _lookup(K key, int hash, int depth) {
    if (hash != _hash)
      return new Option<V>.none();
    LList<Pair<K, V>> it = _pairs;
    while (!it.isNil()) {
      Cons<Pair<K, V>> cons = it.asCons();
      Pair<K, V> elem = cons.elem;
      if (elem.fst == key) return new Option<V>.some(elem.snd);
      it = cons.tail;
    }
    return new Option<V>.none();
  }

  PersistentMap mapValues(f(V)) =>
      new _Leaf(_hash, _pairs.map((p) => new Pair(p.fst, f(p.snd))), length);

  void forEach(f(K, V)) {
    _pairs.foreach((Pair<K, V> pair) => f(pair.fst, pair.snd));
  }

  bool operator ==(PersistentMap<K, V> other) {
    if (identical(this, other)) return true;
    if (other is! _Leaf) return false;
    if (_hash != other._hash) return false;
    if (length != other.length) return false;
    Map<K, V> thisAsMap = toMap();
    int counter = 0;
    LList<Pair<K, V>> it = other._pairs;
    while (!it.isNil()) {
      Cons<Pair<K, V>> cons = it.asCons();
      Pair<K, V> elem = cons.elem;
      if (elem.snd == null && !thisAsMap.containsKey(elem.fst))
        return false;
      if (thisAsMap[elem.fst] != elem.snd)
        return false;
      counter++;
      it = cons.tail;
    }
    return thisAsMap.length == counter;
  }

  toDebugString() => "_Leaf($_hash, $_pairs)";
}

class _SubMap<K, V> extends _APersistentMap<K, V> {
  int _bitmap;
  List<_APersistentMap<K, V>> _array;

  _SubMap(this._bitmap, this._array, int size) : super(size);

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
      _APersistentMap<K, V> map = _array[index];
      return map._lookup(key, hash, depth + 1);
    } else {
      return new Option<V>.none();
    }
  }

  PersistentMap<K, V> _insertWith(LList<Pair<K, V>> keyValues, int size,
      V combine(V x, V y), int hash, int depth) {
    assert(size == keyValues.length());

    int branch = (hash >> (depth * 5)) & 0x1f;
    int mask = 1 << branch;
    int index = _popcount(_bitmap & (mask - 1));

    if ((_bitmap & mask) != 0) {
      List<_APersistentMap<K, V>> newarray =
          new List<_APersistentMap<K, V>>.from(_array);
      _APersistentMap<K, V> m = _array[index];
      _APersistentMap<K, V> newM =
          m._insertWith(keyValues, size, combine, hash, depth + 1);
      newarray[index] = newM;
      int delta = newM.length - m.length;
      return new _SubMap<K, V>(_bitmap, newarray, length + delta);
    } else {
      int newlength = _array.length + 1;
      List<_APersistentMap<K, V>> newarray =
          new List<_APersistentMap<K, V>>(newlength);
      // TODO: find out if there's a "copy array" native function somewhere
      for (int i = 0; i < index; i++) { newarray[i] = _array[i]; }
      for (int i = index; i < newlength - 1; i++) { newarray[i+1] = _array[i]; }
      newarray[index] = new _Leaf<K, V>(hash, keyValues, size);
      return new _SubMap<K, V>(_bitmap | mask, newarray, length + size);
    }
  }

  PersistentMap<K, V> _intersectWith(LList<Pair<K, V>> keyValues, int size,
      V combine(V x, V y), int hash, int depth) {
    assert(size == keyValues.length());

    int branch = (hash >> (depth * 5)) & 0x1f;
    int mask = 1 << branch;

    if ((_bitmap & mask) != 0) {
      int index = _popcount(_bitmap & (mask - 1));
      _APersistentMap<K, V> m = _array[index];
      return m._intersectWith(keyValues, size, combine, hash, depth + 1);
    } else {
      return new _EmptyMap();
    }
  }

  PersistentMap<K, V> _delete(K key, int hash, int depth) {
    int branch = (hash >> (depth * 5)) & 0x1f;
    int mask = 1 << branch;

    if ((_bitmap & mask) != 0) {
      int index = _popcount(_bitmap & (mask - 1));
      _APersistentMap<K, V> m = _array[index];
      _APersistentMap<K, V> newm = m._delete(key, hash, depth + 1);
      int delta = newm.length - m.length;
      if (identical(m, newm)) {
        return this;
      }
      if (newm._isEmpty()) {
        if (_array.length > 2) {
          int newsize = _array.length - 1;
          List<_APersistentMap<K, V>> newarray =
              new List<_APersistentMap<K, V>>(newsize);
          for (int i = 0; i < index; i++) { newarray[i] = _array[i]; }
          for (int i = index; i < newsize; i++) { newarray[i] = _array[i + 1]; }
          assert(newarray.length >= 2);
          return new _SubMap(_bitmap ^ mask, newarray, length + delta);
        } else {
          assert(_array.length == 2);
          assert(index == 0 || index == 1);
          _APersistentMap<K, V> onlyValueLeft = _array[1 - index];
          return onlyValueLeft._isLeaf()
              ? onlyValueLeft
              : new _SubMap(_bitmap ^ mask,
                            <_APersistentMap<K, V>>[onlyValueLeft],
                            length + delta);
        }
      } else if (newm._isLeaf()){
        if (_array.length == 1) {
          return newm;
        } else {
          List<_APersistentMap<K, V>> newarray =
              new List<_APersistentMap<K, V>>.from(_array);
          newarray[index] = newm;
          return new _SubMap(_bitmap, newarray, length + delta);
        }
      } else {
        List<_APersistentMap<K, V>> newarray =
            new List<_APersistentMap<K, V>>.from(_array);
        newarray[index] = newm;
        return new _SubMap(_bitmap, newarray, length + delta);
      }
    } else {
      return this;
    }
  }

  PersistentMap<K, V> _adjust(K key, V update(V), int hash, int depth) {
    int branch = (hash >> (depth * 5)) & 0x1f;
    int mask = 1 << branch;
    if ((_bitmap & mask) != 0) {
      int index = _popcount(_bitmap & (mask - 1));
      _APersistentMap<K, V> m = _array[index];
      _APersistentMap<K, V> newm = m._adjust(key, update, hash, depth + 1);
      if (identical(newm, m)) {
        return this;
      }
      List<_APersistentMap<K, V>> newarray =
          new List<_APersistentMap<K, V>>.from(_array);
      newarray[index] = newm;
      return new _SubMap(_bitmap, newarray, length);
    } else {
      return this;
    }
  }

  PersistentMap<K, V>
      _unionWith(_APersistentMap<K, V> m, V combine(V x, V y), int depth) =>
          m._unionWithSubMap(this, combine, depth);

  PersistentMap<K, V>
      _unionWithEmptyMap(_EmptyMap<K, V> m, V combine(V x, V y), int depth) =>
          this;

  PersistentMap<K, V>
      _unionWithLeaf(_Leaf<K, V> m, V combine(V x, V y), int depth) =>
          this._insertWith(m._pairs, m.length, (V v1, V v2) => combine(v2, v1),
              m._hash, depth);

  PersistentMap<K, V>
      _unionWithSubMap(_SubMap<K, V> m, V combine(V x, V y), int depth) {
    int ormap = _bitmap | m._bitmap;
    int andmap = _bitmap & m._bitmap;
    List<_APersistentMap<K, V>> newarray =
        new List<_APersistentMap<K, V>>(_popcount(ormap));
    int mask = 1, i = 0, i1 = 0, i2 = 0;
    int newSize = 0;
    while (mask <= ormap) {
      if ((andmap & mask) != 0) {
        _array[i1];
        m._array[i2];
        _APersistentMap<K, V> newMap =
            m._array[i2]._unionWith(_array[i1], combine, depth + 1);
        newarray[i] = newMap;
        newSize += newMap.length;
        i1++;
        i2++;
        i++;
      } else if ((_bitmap & mask) != 0) {
        _APersistentMap<K, V> newMap = _array[i1];
        newarray[i] = newMap;
        newSize += newMap.length;
        i1++;
        i++;
      } else if ((m._bitmap & mask) != 0) {
        _APersistentMap<K, V> newMap = m._array[i2];
        newarray[i] = newMap;
        newSize += newMap.length;
        i2++;
        i++;
      }
      mask <<= 1;
    }
    return new _SubMap<K, V>(ormap, newarray, newSize);
  }

  PersistentMap<K, V> _intersectionWith(_APersistentMap<K, V> m,
                                        V combine(V x, V y), int depth) =>
      m._intersectionWithSubMap(this, combine, depth);

  PersistentMap<K, V> _intersectionWithEmptyMap(_EmptyMap<K, V> m,
                                                V combine(V x, V y),
                                                int depth) =>
      m;

  PersistentMap<K, V> _intersectionWithLeaf(_Leaf<K, V> m, V combine(V x, V y),
                                            int depth) =>
      _intersectWith(m._pairs,  m.length, (V v1, V v2) => combine(v2, v1),
                     m._hash, depth);

  PersistentMap<K, V> _intersectionWithSubMap(
      _SubMap<K, V> m, V combine(V x, V y), int depth) {
    int andmap = _bitmap & m._bitmap;
    List<_APersistentMap<K, V>> newarray = new List<_APersistentMap<K, V>>();
    int mask = 1, i1 = 0, i2 = 0;
    int newSize = 0;
    int newMask = 0;
    while (mask <= _bitmap) {
      if ((andmap & mask) != 0) {
        _array[i1];
        m._array[i2];
        _APersistentMap<K, V> newMap =
            m._array[i2]._intersectionWith(_array[i1], combine, depth + 1);
        newarray.add(newMap);
        newSize += newMap.length;
        newMask |= mask;
        i1++;
        i2++;
      } else if ((_bitmap & mask) != 0) {
        i1++;
      } else if ((m._bitmap & mask) != 0) {
        i2++;
      }
      mask <<= 1;
    }
    if (newarray.length > 1) {
      return new _SubMap<K, V>(newMask, newarray, newSize);
    } else {
      assert(newarray.length == 1);
      _APersistentMap<K, V> onlyValueLeft = newarray[0];
      return onlyValueLeft._isLeaf()
          ? onlyValueLeft
          : new _SubMap<K, V>(newMask, newarray, newSize);
    }
  }

  PersistentMap mapValues(f(V)) {
    List<_APersistentMap<K, V>> newarray =
        new List<_APersistentMap<K, V>>.from(_array);
    for (int i = 0; i < _array.length; i++) {
      _APersistentMap<K, V> mi = _array[i];
        newarray[i] = mi.mapValues(f);
    }
    return new _SubMap(_bitmap, newarray, length);
  }

  forEach(f(K, V)) {
    _array.forEach((mi) => mi.forEach(f));
  }

  bool operator ==(PersistentMap<K, V> other) {
    if (identical(this, other)) return true;
    if (other is! _SubMap) return false;
    if (_bitmap != other._bitmap) return false;
    if (length != other.length) return false;
    assert(_array.length == other._array.length);
    for (int i = 0; i < _array.length; i++) {
      _APersistentMap<K, V> mi = _array[i];
      _APersistentMap<K, V> omi = other._array[i];
      if (mi != omi) {
        return false;
      }
    }
    return true;
  }

  toDebugString() => "_SubMap($_array)";
}
