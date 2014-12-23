// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

part of persistent;

final _random = new Random();

_ThrowKeyError(key) => throw new Exception('Key Error: ${key} is not defined');

_ThrowUpdateKeyError(key, exception) => throw new Exception('Key $key was not found, calling update with no arguments threw: $exception');

_getUpdateValue(key, updateF) {
  try {
    return updateF();
  } catch(e) {
    _ThrowUpdateKeyError(key, e);
  }
}

abstract class _ReadMapImpl<K, V> extends IterableBase<Pair<K, V>> {
  _Node _root;

  V get(K key, [V notFound = _none]) {
    var val = _root.get(key);
    if(_isNone(val)){
      if (_isNone(notFound)) {
        _ThrowKeyError(key);
      } else {
        return notFound;
      }
    } else {
      return val;
    }
  }

  V operator [](K key) =>
      get(key);

  void forEachKeyValue(f(K key, V value)) => _root.forEachKeyValue(f);

  Map<K, V> toMap() {
    return _root.toMap();
  }

  Iterable<K> get keys => _root.keys;

  Iterable<V> get values => _root.values;

  Iterator get iterator => _root.iterator;

  int get length => _root.length;

  bool containsKey(key) {
    final _none = new Object();
    final value = this.get(key, _none);
    return value != _none;
  }

  bool hasKey(key) => containsKey(key);
}

class _PersistentMapImpl<K, V>
        extends _ReadMapImpl<K, V>
        implements PersistentMap<K, V>, PersistentCollection {

  int _hash;

  int get hashCode {
    if(_hash != null) return _hash;
    _hash = 0;
    this.forEachKeyValue((key, value) {
      _hash += key.hashCode ^ value.hashCode;
      _hash = _hash & 0x1fffffff;
    });
    return _hash;
  }

  bool operator==(other) {
    if (other is! _PersistentMapImpl) return false;
    if(other.hashCode != this.hashCode || this.length != other.length)
      return false;
    bool equals = true;
    this.forEachKeyValue((key, dynamic value) {
      equals = equals && other.containsKey(key) && other[key] == value;
    });
    return equals;
  }

  _PersistentMapImpl() {
    this._root = new _EmptyMap<K, V>(null);
  }

  _PersistentMapImpl.fromMap(Map<K, V> map) {
    _root = new _EmptyMap<K, V>(null);
    _Owner owner = new _Owner();
    map.forEach((K key, V value) {
      _root = _root.assoc(owner, key, value);
    });
  }

  _PersistentMapImpl.fromPairs(Iterable<Pair<K, V>> pairs) {
    _root = new _EmptyMap<K, V>(null);
    _Owner owner = new _Owner();
    pairs.forEach((pair) {
      _root = _root.assoc(owner, pair.first, pair.second);
    });
  }

  _PersistentMapImpl.fromTransient(_TransientMapImpl map) {
    this._root = map._root;
  }

  _PersistentMapImpl._new(_Node __root) { _root = __root; }

  _PersistentMapImpl<K, V>
      assoc(K key, V value) {
        return new _PersistentMapImpl._new(_root.assoc(null, key, value));
      }

  _PersistentMapImpl<K, V> delete(K key, {bool missingOk: false}) {
    return new _PersistentMapImpl._new(_root.delete(null, key, missingOk));
  }


  _PersistentMapImpl strictMap(Pair f(Pair<K, V> pair)) =>
      new _PersistentMapImpl.fromPairs(this.map(f));

  _PersistentMapImpl<K, V> strictWhere(bool f(Pair<K, V> pair)) =>
      new _PersistentMapImpl<K, V>.fromPairs(this.where(f));

  TransientMap asTransient() {
    return new _TransientMapImpl.fromPersistent(this);
  }

  _PersistentMapImpl withTransient(dynamic f(TransientMap map)) {
    TransientMap transient = this.asTransient();
    f(transient);
    return transient.asPersistent();
  }
  toString() => 'PersistentMap$_root';
}

class _TransientMapImpl<K, V>
        extends _ReadMapImpl<K, V>
        implements TransientMap<K, V> {
  _Node _root;
  _Owner _owner;
  get owner => _owner != null ? _owner :
      throw new Exception('Cannot modify TransientMap after calling asPersistent.');

  factory _TransientMapImpl() => new _TransientMapImpl.fromPersistent(new PersistentMap());

  /**
   * Creates an immutable copy of [map] using the default implementation of
   * [TransientMap].
   */
  _TransientMapImpl.fromPersistent(_PersistentMapImpl<K, V> map) {
    _owner = new _Owner();
    _root = map._root;
  }

  TransientMap _adjustRootAndReturn(newRoot) {
    _root = newRoot;
    return this;
  }

  TransientMap<K, V>
      doAssoc(K key, V value) {
        return _adjustRootAndReturn(_root.assoc(owner, key, value));
      }

  operator []=(key, value){
    this.doAssoc(key, value);
  }

  TransientMap<K, V> doDelete(K key, {bool missingOk: false}) {
    return _adjustRootAndReturn(_root.delete(owner, key, missingOk));
  }

  PersistentMap asPersistent() {
    _owner = null;
    return new _PersistentMapImpl.fromTransient(this);
  }

  toString() => 'TransientMap(${owner.hashCode}, $_root)';
}

/**
 * Exception used for aborting forEach loops.
 */
class _Stop implements Exception {}

/**
 * Superclass for _EmptyMap, _Leaf and _SubMap.
 */
abstract class _Node<K, V> extends IterableBase<Pair<K, V>> {
  _Owner _owner;

  int _length;
  get length => _length;

  _Node(this._owner, this._length);

  V _get(K key, int hash, int depth);
  _Node<K, V> _insertWith(_Owner owner, keyValues, int kvLength,
      V combine(V x, V y), int hash, int depth);
  _Node<K, V> _delete(_Owner owner, K key, int hash, int depth, bool missingOk);

  bool operator ==(other) {
    if (other is! _Node) return false;
    if (identical(this, other)) return true;
    if (this.length != other.length) {
      return false;
    }
    bool res = true;
    this.forEachKeyValue((k,v){
      if (other.get(k) != v){
        res = false;
      }
    });
    return res;
  }


  V get(K key) =>
      _get(key, key.hashCode & 0x3fffffff, 0);

  _Node<K, V> assoc(_Owner owner, K key, V value) =>
      _insertWith(owner, [key, value],
          1, (V x, V y) => y,
          key.hashCode & 0x3fffffff, 0);

  _Node<K, V> delete(_Owner owner, K key, bool missingOk) =>
      _delete(owner ,key, key.hashCode & 0x3fffffff, 0, missingOk);

  Map<K, V> toMap() {
    Map<K, V> result = new Map<K, V>();
    this.forEachKeyValue((K k, V v) { result[k] = v; });
    return result;
  }

  String toString() {
    StringBuffer buffer = new StringBuffer('{');
    bool comma = false;
    this.forEachKeyValue((K k, V v) {
      if (comma) buffer.write(', ');
      buffer.write('$k: $v');
      comma = true;
    });
    buffer.write('}');
    return buffer.toString();
  }

  Iterable<K> get keys => this.map((Pair<K, V> pair) => pair.first);

  Iterable<V> get values => this.map((Pair<K, V> pair) => pair.second);

  Pair<K, V> pickRandomEntry([Random random]) =>
      elementAt((random != null ? random : _random).nextInt(this.length));

  void forEachKeyValue(f(K key, V value));

}

class _EmptyMapIterator<K, V> implements Iterator<Pair<K, V>> {
  const _EmptyMapIterator();
  Pair<K, V> get current => null;
  bool moveNext() => false;
}

class _EmptyMap<K, V> extends _Node<K, V> {
  _EmptyMap(_Owner owner) : super(owner, 0);

  V _get(K key, int hash, int depth) => _none;

  _Node<K, V> _insertWith(_Owner owner,
      keyValues, int kvLength, V combine(V x, V y), int hash,
      int depth) {
    return new _Leaf<K, V>.abc(owner, hash, keyValues, kvLength);
  }

  _Node<K, V> _delete(_Owner owner, K key, int hash, int depth, bool missingOk) =>
      missingOk ? this : _ThrowKeyError(key);

  _Node<K, V> _update(_Owner owner, K key, dynamic updateF, int hash, int depth) =>
    this.assoc(owner, key, _getUpdateValue(key, updateF));

  void forEachKeyValue(f(K, V)) {}

  bool operator ==(other) => other is _Node ? other is _EmptyMap : false;

  Iterator<Pair<K, V>> get iterator => const _EmptyMapIterator();

  Pair<K, V> _elementAt(int index) {
    throw new RangeError.value(index);
  }

  Pair<K, V> get last {
    throw new StateError("Empty map has no entries");
  }

  toDebugString() => "_EmptyMap()";
}

class _Leaf<K, V> extends _Node<K, V> {
  int _hash;
  List _kv;

  get iterator => throw new Exception('not implemented');

  _Leaf.abc(_Owner owner, this._hash, this._kv, int kvLength) : super(owner, kvLength) {
  }

  factory _Leaf.ensureOwner(_Leaf old, _Owner owner, hash, kv, int length) {
    if(_ownerEquals(owner, old._owner)) {
      old._kv = kv;
      old._hash = hash;
      old._length = length;
      return old;
    }
    return new _Leaf.abc(owner, hash, kv, length);
  }

  _Node<K, V> polish(_Owner owner, int depth, List _kv) {
    if (_kv.length < 32 || depth > 5) {
      return new _Leaf.abc(owner, _hash, _kv, _kv.length);
    } else {
      List<List> kvs = new List.generate(32, (_) => []);
      for (int i=0; i<_kv.length; i++){
        if (i%2 == 0) {
          var key = _kv[i];
          var val = _kv[i+1];
          int branch = (key.hashCode >> (depth * 5)) & 0x1f;
          kvs[branch].add(key);
          kvs[branch].add(val);
        }
      }
      List <_Node<K, V>> array = [];
      int bitmap=0;
      for (int i=0; i<32; i++){
        if (!kvs[i].isEmpty) {
          bitmap |= 1<<i;
          array.add(new _Leaf.abc(owner, _hash, kvs[i], kvs[i].length));
        }
      }
      return new _SubMap.abc(owner, bitmap, array, length);
    }
  }

  _Node<K, V> _insertWith(_Owner owner, List kv, int kvLength,
      V combine(V x, V y), int hash, int depth) {
    List nkv = new List.from(_kv);
    for (int i=0; i<kv.length; i++){
      if (i%2 == 0) {
        for (int j=0; j<nkv.length; j++){
          if (j%2 == 0) {
            if (kv[i] == nkv[j]) {
              nkv.removeAt(j);
              nkv.removeAt(j);
              break;
            }
          }
        }
        nkv.add(kv[i]);
        nkv.add(kv[i+1]);
      }
    }
    return polish(owner, depth, nkv);
  }

  _Node<K, V> _delete(_Owner owner, K key, int hash, int depth, bool missingOk) {
    if (hash != _hash) {
      if(!missingOk) _ThrowKeyError(key);
      return this;
    }
    bool found = false;
    List nkv = new List.from(_kv);
    for (int i=0; i<_kv.length; i++){
      if (i%2 == 0 && this._kv[i] == key){
        nkv.removeAt(i);
        nkv.removeAt(i);
        found = true;
      }
    }

    if(!found && !missingOk) _ThrowKeyError(key);
    if(!found && missingOk) return this;
    return nkv.isEmpty?
          new _EmptyMap<K, V>(owner)
        : new _Leaf<K, V>.ensureOwner(this, owner, _hash, nkv, length - 1);
  }

  V _get(K key, int hash, int depth) {
    for(int i=0; i<_kv.length; i++){
      if (i%2 == 0 && _kv[i] == key) {
        return _kv[i+1];
      }
    }
    return _none;
  }

  void forEachKeyValue(f(K, V)) {
    for (int i=0; i<_kv.length; i++) {
      if (i%2 == 0) {
        f(_kv[i], _kv[i+1]);
      }
    }
  }

  toDebugString() => "_Leaf($_hash, $_kv)";
}

class _SubMap<K, V> extends _Node<K, V> {
  int _bitmap;
  List<_Node<K, V>> _array;

  get iterator => throw new Exception('not implemented');

  _SubMap.abc(_Owner owner, this._bitmap, this._array, int length) : super(owner, length);

  factory _SubMap.ensureOwner(_SubMap old, _Owner owner, bitmap, array, int length) {
    if(_ownerEquals(owner, old._owner)) {
      old._bitmap = bitmap;
      old._array = array;
      old._length = length;
    }
    return new _SubMap.abc(owner , bitmap, array, length);
  }

  static _popcount(int n) {
    n = n - ((n >> 1) & 0x55555555);
    n = (n & 0x33333333) + ((n >> 2) & 0x33333333);
    n = (n + (n >> 4)) & 0x0F0F0F0F;
    n = n + (n >> 8);
    n = n + (n >> 16);
    return n & 0x0000003F;
  }

  V _get(K key, int hash, int depth) {
    int branch = (hash >> (depth * 5)) & 0x1f;
    int mask = 1 << branch;
    if ((_bitmap & mask) != 0) {
      int index = _popcount(_bitmap & (mask - 1));
      _Node<K, V> map = _array[index];
      return map._get(key, hash, depth + 1);
    } else {
      return _none;
    }
  }

  _Node<K, V> _insertWith(_Owner owner, List keyValues, int kvLength,
      V combine(V x, V y), int hash, int depth) {

    int branch = (hash >> (depth * 5)) & 0x1f;
    int mask = 1 << branch;
    int index = _popcount(_bitmap & (mask - 1));

    if ((_bitmap & mask) != 0) {
      _Node<K, V> m = _array[index];
      int oldSize = m.length;
      _Node<K, V> newM =
                m._insertWith(owner, keyValues, kvLength, combine, hash, depth + 1);
      if(identical(m, newM)) {
        if(oldSize != m.length) this._length += m.length - oldSize;
        return this;
      }
      List<_Node<K, V>> newarray = _makeCopyIfNeeded(owner, this._owner, _array);
      newarray[index] = newM;
      int delta = newM.length - oldSize;
      return new _SubMap<K, V>.ensureOwner(this, owner, _bitmap, newarray, length + delta);
    } else {
      int newlength = _array.length + 1;
      List<_Node<K, V>> newarray =
          new List<_Node<K, V>>(newlength);
      // TODO: find out if there's a "copy array" native function somewhere
      for (int i = 0; i < index; i++) { newarray[i] = _array[i]; }
      for (int i = index; i < newlength - 1; i++) { newarray[i+1] = _array[i]; }
      newarray[index] = new _Leaf<K, V>.abc(owner, hash, keyValues, kvLength);
      return new _SubMap<K, V>.ensureOwner(this, owner, _bitmap | mask, newarray, length + kvLength);
    }
  }


  _Node<K, V> _delete(owner, K key, int hash, int depth, bool missingOk) {
    int branch = (hash >> (depth * 5)) & 0x1f;
    int mask = 1 << branch;

    if ((_bitmap & mask) != 0) {
      int index = _popcount(_bitmap & (mask - 1));
      _Node<K, V> m = _array[index];
      int oldSize = m.length;
      _Node<K, V> newm = m._delete(owner, key, hash, depth + 1, missingOk);
      int delta = newm.length - oldSize;
      if (identical(m, newm)) {
        this._length += delta;
        return this;
      }
      if (newm is _EmptyMap) {
        if (_array.length > 2) {
          int newsize = _array.length - 1;
          List<_Node<K, V>> newarray =
              new List<_Node<K, V>>(newsize);
          for (int i = 0; i < index; i++) { newarray[i] = _array[i]; }
          for (int i = index; i < newsize; i++) { newarray[i] = _array[i + 1]; }
          assert(newarray.length >= 2);
          return new _SubMap.ensureOwner(this, owner, _bitmap ^ mask, newarray, length + delta);
        } else {
          assert(_array.length == 2);
          assert(index == 0 || index == 1);
          _Node<K, V> onlyValueLeft = _array[1 - index];
          return (onlyValueLeft is _Leaf)
              ? onlyValueLeft
              : new _SubMap.ensureOwner(this, owner, _bitmap ^ mask,
                            <_Node<K, V>>[onlyValueLeft],
                            length + delta);
        }
      } else if (newm is _Leaf){
        if (_array.length == 1) {
          return newm;
        } else {
          List<_Node<K, V>> newarray = _makeCopyIfNeeded(owner, this._owner, _array);
          newarray[index] = newm;
          return new _SubMap.ensureOwner(this, owner, _bitmap, newarray, length + delta);
        }
      } else {
        List<_Node<K, V>> newarray = _makeCopyIfNeeded(owner, this._owner, _array);
        newarray[index] = newm;

        return new _SubMap.ensureOwner(this, owner, _bitmap, newarray, length + delta);
      }
    } else {
      if(!missingOk) _ThrowKeyError(key);
      return this;
    }
  }

  forEachKeyValue(f(K, V)) {
    _array.forEach((mi) => mi.forEachKeyValue(f));
  }

  toDebugString() => "_SubMap($_array)";
}

_ownerEquals(_Owner a, _Owner b) {
  return a != null && a == b;
}

_makeCopyIfNeeded(_Owner a, _Owner b, List c) {
  if(_ownerEquals(a, b))
    return c;
  else return c.sublist(0);
}
