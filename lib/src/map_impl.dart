// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

part of persistent;

final _random = new Random();

_ThrowKeyError(key) => throw new Exception('Key Error: ${key} is not defined');

abstract class _ReadMapImpl<K, V> extends IterableBase<Pair<K, V>> {
  _NodeBase _root;

  V lookup(K key, {orElse()}) {
    var val = _root.lookup(key);
    if(_isNone(val)){
      if (orElse == null) {
        _ThrowKeyError(key);
      } else {
        return orElse();
      }
    } else {
      return val;
    }
  }

  V operator [](K key) =>
      lookup(key);

  void forEachKeyValue(f(K key, V value)) => _root.forEachKeyValue(f);

  Map<K, V> toMap() {
    return _root.toMap();
  }

  Iterable<K> get keys => _root.keys;

  Iterable<V> get values => _root.values;

  Pair<K, V> pickRandomEntry([Random random]) => _root.pickRandomEntry(random);

  Iterator get iterator => _root.iterator;

  int get length => _root.length;

  bool containsKey(key) {
    final value = this.lookup(key, orElse: _getNone);
    return !_isNone(value);
  }
}

class _PersistentMapImpl<K, V>
        extends _ReadMapImpl<K, V>
        implements PersistentMap<K, V>, Persistent {

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
      _root = _root.insert(owner, key, value);
    });
  }

  _PersistentMapImpl.fromPairs(Iterable<Pair<K, V>> pairs) {
    _root = new _EmptyMap<K, V>(null);
    _Owner owner = new _Owner();
    pairs.forEach((pair) {
      _root = _root.insert(owner, pair.fst, pair.snd);
    });
  }

  _PersistentMapImpl.fromTransient(_TransientMapImpl map) {
    this._root = map._root;
  }

  _PersistentMapImpl._new(_NodeBase __root) { _root = __root; }

  _PersistentMapImpl<K, V>
      insert(K key, V value, [V combine(V oldvalue, V newvalue)]) {
        return new _PersistentMapImpl._new(_root.insert(null, key, value, combine));
      }

  _PersistentMapImpl<K, V> delete(K key, {bool safe: false}) {
    return new _PersistentMapImpl._new(_root.delete(null, key, safe));
  }

  _PersistentMapImpl<K, V> adjust(K key, V update(V value), {safe: false}) {
    return  new _PersistentMapImpl._new(_root.adjust(null, key, update, safe));
  }

  _PersistentMapImpl mapValues(f(V value)) {
    _Owner owner = new _Owner();
    return new _PersistentMapImpl._new(_root.mapValues(owner, f));
  }

  _PersistentMapImpl<K, V>
      union(PersistentMap<K, V> other, [V combine(V left, V right)]) {

    if(other is _PersistentMapImpl<K, V>){
      _Owner owner = new _Owner();
      return new _PersistentMapImpl._new(
          _root.union(owner, other._root, combine));

    } else {
      if(combine == null) combine = (_, x)=>x;
      return this.withTransient((map){
        other.forEach((pair){
          if(this.containsKey(pair.fst)){
            map[pair.fst] = combine(map[pair.fst], pair.snd);
          } else {
            map[pair.fst] = pair.snd;
          }
        });
      });
    }
  }

  _PersistentMapImpl<K, V>
    intersection(PersistentMap<K, V> other, [V combine(V left, V right)]){

      if(other is _PersistentMapImpl<K, V>){
        return new _PersistentMapImpl._new(
            _root.intersection(null, other._root, combine));

      } else {
        if(combine == null) combine = (_, x)=>x;
        return new PersistentMap.fromPairs(this.expand((pair){
          if(other.containsKey(pair.fst)){
            return [new Pair(pair.fst,combine(pair.snd,other[pair.fst]))];
          } else {
            return [];
          }
        }));
      }
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
  _NodeBase _root;
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
      doInsert(K key, V value, [V combine(V oldvalue, V newvalue)]) {
        return _adjustRootAndReturn(_root.insert(owner, key, value, combine));
      }

  operator []=(key, value){
    this.doInsert(key, value);
  }

  TransientMap<K, V> doDelete(K key, {bool safe: false}) {
    return _adjustRootAndReturn(_root.delete(owner, key, safe));
  }

  TransientMap<K, V> doAdjust(K key, V update(V value), {safe: false}) {
    return _adjustRootAndReturn(_root.adjust(owner, key, update, safe));
  }

  TransientMap doMapValues(f(V value)) {
    return _adjustRootAndReturn(_root.mapValues(owner, f));
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

abstract class _NodeBase<K, V>
    extends IterableBase<Pair<K, V>> {

  int _length;
  get length => _length;

  _NodeBase(this._length);

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

  Iterable<K> get keys => this.map((Pair<K, V> pair) => pair.fst);

  Iterable<V> get values => this.map((Pair<K, V> pair) => pair.snd);

  Pair<K, V> pickRandomEntry([Random random]) =>
      elementAt((random != null ? random : _random).nextInt(this.length));

  _NodeBase<K, V>
    insert(_Owner owner, K key, V value, [V combine(V oldvalue, V newvalue)]);

  _NodeBase<K, V> delete(_Owner owner, K key, bool safe);

  V lookup(K key);

  void forEachKeyValue(f(K key, V value));

  _NodeBase<K, V> adjust(_Owner owner, K key, V update(V value), bool safe);

  _NodeBase mapValues(_Owner owner, f(V value));

  _NodeBase<K, V>
      union(_Owner owner, _NodeBase<K, V> other, [V combine(V left, V right)]);

  _NodeBase<K, V>
      intersection(_Owner owner, _NodeBase<K, V> other, [V combine(V left, V right)]);

}

/**
 * Superclass for _EmptyMap, _Leaf and _SubMap.
 */
abstract class _ANodeBase<K, V> extends _NodeBase<K, V> {
  _Owner _owner;

  _ANodeBase(this._owner, length, this._isLeaf) : super(length);

  final bool _isLeaf;

  V _lookup(K key, int hash, int depth);
  _NodeBase<K, V> _insertWith(_Owner owner, LinkedList<Pair<K, V>> keyValues, int size,
      V combine(V x, V y), int hash, int depth);
  _NodeBase<K, V> _intersectWith(_Owner owner, LinkedList<Pair<K, V>> keyValues, int size,
      V combine(V x, V y), int hash, int depth);
  _NodeBase<K, V> _delete(_Owner owner, K key, int hash, int depth, bool safe);
  _NodeBase<K, V> _adjust(_Owner owner, K key, V update(V), int hash, int depth, bool safe);

  _ANodeBase<K, V>
      _unionWith(_Owner owner, _ANodeBase<K, V> m, V combine(V x, V y), int depth);
  _ANodeBase<K, V>
      _unionWithEmptyMap(_Owner owner, _EmptyMap<K, V> m, V combine(V x, V y), int depth);
  _ANodeBase<K, V>
      _unionWithLeaf(_Owner owner, _Leaf<K, V> m, V combine(V x, V y), int depth);
  _ANodeBase<K, V>
      _unionWithSubMap(_Owner owner, _SubMap<K, V> m, V combine(V x, V y), int depth);

  _ANodeBase<K, V>
      _intersectionWith(_Owner owner, _ANodeBase<K, V> m, V combine(V x, V y),
                        int depth);
  _ANodeBase<K, V>
      _intersectionWithEmptyMap(_Owner owner, _EmptyMap<K, V> m, V combine(V x, V y),
                                int depth);
  _ANodeBase<K, V>
      _intersectionWithLeaf(_Owner owner, _Leaf<K, V> m, V combine(V x, V y), int depth);
  _ANodeBase<K, V>
      _intersectionWithSubMap(_Owner owner, _SubMap<K, V> m, V combine(V x, V y), int depth);

  Pair<K, V> _elementAt(int index);

  LinkedList<Pair<K, V>> _onePair(K key, V value) =>
      new Cons<Pair<K, V>>(new Pair<K, V>(key, value), new Nil<Pair<K, V>>());

  V lookup(K key) =>
      _lookup(key, key.hashCode & 0x3fffffff, 0);

  _NodeBase<K, V> insert(_Owner owner, K key, V value, [V combine(V x, V y)]) =>
      _insertWith(owner, _onePair(key, value),
          1,
          (combine != null) ? combine : (V x, V y) => y,
          key.hashCode & 0x3fffffff, 0);

  _NodeBase<K, V> delete(_Owner owner, K key, bool safe) =>
      _delete(owner ,key, key.hashCode & 0x3fffffff, 0, safe);

  _NodeBase<K, V> adjust(_Owner owner, K key, V update(V), bool safe) =>
      _adjust(owner, key, update, key.hashCode & 0x3fffffff, 0, safe);

  _NodeBase<K, V> union(_Owner owner, _NodeBase<K, V> other, [V combine(V x, V y)]) =>
    this._unionWith(owner, other, (combine != null) ? combine : (V x, V y) => y, 0);

  _NodeBase<K, V>
      intersection(_Owner owner, _NodeBase<K, V> other, [V combine(V left, V right)]) =>
    this._intersectionWith(owner, other,
        (combine != null) ? combine : (V x, V y) => y, 0);

  Pair<K, V> elementAt(int index) {
    if (index < 0 || index >= length) throw new RangeError.value(index);
    return _elementAt(index);
  }

  // toString() => toDebugString();
}

class _EmptyMapIterator<K, V> implements Iterator<Pair<K, V>> {
  const _EmptyMapIterator();
  Pair<K, V> get current => null;
  bool moveNext() => false;
}

class _EmptyMap<K, V> extends _ANodeBase<K, V> {
  _EmptyMap(_Owner owner) : super(owner, 0, false);

  V _lookup(K key, int hash, int depth) => _none;

  _NodeBase<K, V> _insertWith(_Owner owner,
      LinkedList<Pair<K, V>> keyValues, int size, V combine(V x, V y), int hash,
      int depth) {
    assert(size == keyValues.length);
    return new _Leaf<K, V>.abc(owner, hash, keyValues, size);
  }

  _NodeBase<K, V> _intersectWith(_Owner owner, LinkedList<Pair<K, V>> keyValues, int size,
      V combine(V x, V y), int hash, int depth) {
    assert(size == keyValues.length);
    return this;
  }

  _NodeBase<K, V> _delete(_Owner owner, K key, int hash, int depth, bool safe) =>
    safe ? this : _ThrowKeyError(key);

  _NodeBase<K, V> _adjust(_Owner owner, K key, V update(V), int hash, int depth, bool safe) =>
    safe ? this : _ThrowKeyError(key);

  _NodeBase<K, V>
      _unionWith(_Owner owner, _NodeBase<K, V> m, V combine(V x, V y), int depth) => m;

  _NodeBase<K, V>
      _unionWithEmptyMap(_Owner owner, _EmptyMap<K, V> m, V combine(V x, V y), int depth) {
    throw "should never be called";
  }

  _NodeBase<K, V>
      _unionWithLeaf(_Owner owner, _Leaf<K, V> m, V combine(V x, V y), int depth) => m;

  _NodeBase<K, V>
      _unionWithSubMap(_Owner owner, _SubMap<K, V> m, V combine(V x, V y), int depth) => m;

  _NodeBase<K, V>
      _intersectionWith(_Owner owner, _ANodeBase<K, V> m, V combine(V x, V y),
                        int depth) => this;

  _NodeBase<K, V>
      _intersectionWithEmptyMap(_Owner owner, _EmptyMap<K, V> m, V combine(V x, V y),
                                int depth) {
        throw "should never be called";
  }

  _NodeBase<K, V> _intersectionWithLeaf(_Owner owner,
      _Leaf<K, V> m, V combine(V x, V y), int depth) => this;

  _NodeBase<K, V> _intersectionWithSubMap(_Owner owner,
      _SubMap<K, V> m, V combine(V x, V y), int depth) => this;

  _NodeBase mapValues(_Owner owner, f(V)) => this;

  void forEachKeyValue(f(K, V)) {}

  bool operator ==(_NodeBase<K, V> other) => other is _EmptyMap;

  Iterator<Pair<K, V>> get iterator => const _EmptyMapIterator();

  Pair<K, V> _elementAt(int index) {
    throw new RangeError.value(index);
  }

  Pair<K, V> get last {
    throw new StateError("Empty map has no entries");
  }

  toDebugString() => "_EmptyMap()";
}

class _Leaf<K, V> extends _ANodeBase<K, V> {
  int _hash;
  LinkedList<Pair<K, V>> _pairs;

  _Leaf.abc(_Owner owner, this._hash, pairs, int size) : super(owner, size, true) {
    this._pairs = pairs;
    assert(size == pairs.length);
  }

  factory _Leaf.ensureOwner(_Leaf old, _Owner owner, hash, pairs, int size) {
    if(_ownerEquals(owner, old._owner)) {
      old._pairs = pairs;
      old._hash = hash;
      old._length = size;
      return old;
    }
    return new _Leaf.abc(owner, hash, pairs, size);
  }

  _NodeBase<K, V> _insertWith(_Owner owner, LinkedList<Pair<K, V>> keyValues, int size,
      V combine(V x, V y), int hash, int depth) {
    assert(size == keyValues.length);
    // newsize is incremented as a side effect of insertPair
    int newsize = length;

    LinkedList<Pair<K, V>> insertPair(Pair<K, V> toInsert,
                                      LinkedList<Pair<K, V>> pairs) {
      LinkedListBuilder<Pair<K, V>> builder =
          new LinkedListBuilder<Pair<K, V>>();
      LinkedList<Pair<K, V>> it = pairs;
      while (it.isCons) {
        Cons<Pair<K, V>> cons = it.asCons;
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

    LinkedList<Pair<K, V>> insertPairs(
        LinkedList<Pair<K, V>> toInsert, LinkedList<Pair<K, V>> pairs) {
      LinkedList<Pair<K, V>> res = pairs;
      LinkedList<Pair<K, V>> it = toInsert;
      while (it.isCons) {
        Cons<Pair<K, V>> cons = it.asCons;
        Pair<K, V> elem = cons.elem;
        res = insertPair(elem, res);
        it = cons.tail;
      }
      assert(newsize == res.length);
      return res;
    }

    if (depth > 5) {
      assert(_hash == hash);
      final LinkedList<Pair<K, V>> newPairs = insertPairs(keyValues, _pairs);
      return new _Leaf<K, V>.ensureOwner(this, owner, hash, newPairs, newsize);
    } else {
      if (hash == _hash) {
        final LinkedList<Pair<K, V>> newPairs = insertPairs(keyValues, _pairs);
        return new _Leaf<K, V>.ensureOwner(this, owner, hash, newPairs, newsize);
      } else {
        int branch = (_hash >> (depth * 5)) & 0x1f;
        List<_ANodeBase<K, V>> array = new List.filled(1, this);
        return new _SubMap<K, V>.abc(owner, 1 << branch, array, length)
            ._insertWith(owner, keyValues, size, combine, hash, depth);
      }
    }
  }

  _NodeBase<K, V> _intersectWith(_Owner owner, LinkedList<Pair<K, V>> keyValues, int size,
      V combine(V x, V y), int hash, int depth) {
    assert(size == keyValues.length);
    // TODO(polux): possibly faster implementation
    Map<K, V> map = toMap();
    LinkedListBuilder<Pair<K, V>> builder = new LinkedListBuilder<Pair<K, V>>();
    int newsize = 0;
    keyValues.foreach((Pair<K, V> pair) {
      if (map.containsKey(pair.fst)) {
        builder.add(new Pair<K, V>(pair.fst, combine(map[pair.fst], pair.snd)));
        newsize++;
      }
    });
    return new _Leaf.ensureOwner(this, owner, _hash, builder.build(), newsize);
  }

  _NodeBase<K, V> _delete(_Owner owner, K key, int hash, int depth, bool safe) {
    if (hash != _hash) {
      if(!safe) _ThrowKeyError(key);
      return this;
    }
    bool found = false;
    LinkedList<Pair<K, V>> newPairs = _pairs.strictWhere((p) {
      if (p.fst == key) {
        found = true;
        return false;
      }
      return true;
    });

    if(!found && !safe) _ThrowKeyError(key);
    return newPairs.isNil
        ? new _EmptyMap<K, V>(owner)
        : new _Leaf<K, V>.ensureOwner(this, owner, _hash, newPairs, found ? length - 1 : length);
  }

  _NodeBase<K, V> _adjust(_Owner owner, K key, V update(V), int hash, int depth, bool safe) {
    LinkedList<Pair<K, V>> adjustPairs() {
      LinkedListBuilder<Pair<K, V>> builder =
          new LinkedListBuilder<Pair<K, V>>();
      LinkedList<Pair<K, V>> it = _pairs;
      while (it.isCons) {
        Cons<Pair<K, V>> cons = it.asCons;
        Pair<K, V> elem = cons.elem;
        if (elem.fst == key) {
          builder.add(new Pair<K, V>(key, update(elem.snd)));
          return builder.build(cons.tail);
        }
        builder.add(elem);
        it = cons.tail;
      }
      if(!safe) _ThrowKeyError(key);
      return builder.build();
    }

    return (hash != _hash)
        ? (safe ? this : _ThrowKeyError(key))
        : new _Leaf<K, V>.ensureOwner(this, owner, _hash, adjustPairs(), length);
  }

  _NodeBase<K, V>
      _unionWith(_Owner owner, _ANodeBase<K, V> m, V combine(V x, V y), int depth) =>
          m._unionWithLeaf(owner, this, combine, depth);

  _NodeBase<K, V>
      _unionWithEmptyMap(_Owner owner, _EmptyMap<K, V> m, V combine(V x, V y), int depth) =>
          this;

  _NodeBase<K, V>
      _unionWithLeaf(_Owner owner, _Leaf<K, V> m, V combine(V x, V y), int depth) =>
          m._insertWith(owner, _pairs, length, combine, _hash, depth);

  _NodeBase<K, V>
      _unionWithSubMap(_Owner owner, _SubMap<K, V> m, V combine(V x, V y), int depth) =>
          m._insertWith(owner, _pairs, length, combine, _hash, depth);

  _NodeBase<K, V> _intersectionWith(_Owner owner, _ANodeBase<K, V> m,
                                        V combine(V x, V y), int depth) =>
      m._intersectionWithLeaf(owner, this, combine, depth);

  _NodeBase<K, V> _intersectionWithEmptyMap(_Owner owner, _EmptyMap<K, V> m,
                                                V combine(V x, V y),
                                                int depth) =>
      m;

  _NodeBase<K, V> _intersectionWithLeaf(_Owner owner, _Leaf<K, V> m, V combine(V x, V y),
                                            int depth) =>
      m._intersectWith(owner, _pairs, length, combine, _hash, depth);

  _NodeBase<K, V> _intersectionWithSubMap(_Owner owner, _SubMap<K, V> m,
                                              V combine(V x, V y), int depth) =>
      m._intersectWith(owner, _pairs, length, combine, _hash, depth);

  V _lookup(K key, int hash, int depth) {
    if (hash != _hash)
      return _none;
    LinkedList<Pair<K, V>> it = _pairs;
    while (it.isCons) {
      Cons<Pair<K, V>> cons = it.asCons;
      Pair<K, V> elem = cons.elem;
      if (elem.fst == key) return elem.snd;
      it = cons.tail;
    }
    return _none;
  }

  _NodeBase mapValues(_Owner owner, f(V)) =>
      new _Leaf.ensureOwner(this, owner, _hash,
                _pairs.strictMap((p) => new Pair(p.fst, f(p.snd))), length);

  void forEachKeyValue(f(K, V)) {
    _pairs.foreach((Pair<K, V> pair) => f(pair.fst, pair.snd));
  }

  bool operator ==(_NodeBase<K, V> other) {
    if (identical(this, other)) return true;
    if (other is! _Leaf) return false;
    _Leaf otherLeaf = other;
    if (_hash != otherLeaf._hash) return false;
    if (length != otherLeaf.length) return false;
    Map<K, V> thisAsMap = toMap();
    int counter = 0;
    LinkedList<Pair<K, V>> it = otherLeaf._pairs;
    while (it.isCons) {
      Cons<Pair<K, V>> cons = it.asCons;
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

  Iterator<Pair<K, V>> get iterator => _pairs.iterator;

  Pair<K, V> _elementAt(int index) {
    var tail = _pairs;
    for (int i = 0; i < index; i++) {
      tail = tail.asCons.tail;
    }
    return tail.asCons.elem;
  }

  Pair<K, V> get last {
    Cons pairs = _pairs.asCons;
    while (!pairs.tail.isNil) {
      pairs = pairs.tail;
    }
    return pairs.elem;
  }

  toDebugString() => "_Leaf($_hash, $_pairs)";
}

class _SubMapIterator<K, V> implements Iterator<Pair<K, V>> {
  List<_ANodeBase<K, V>> _array;
  int _index = 0;
  // invariant: _currentIterator != null => _currentIterator.current != null
  Iterator<Pair<K, V>> _currentIterator = null;

  _SubMapIterator(this._array);

  Pair<K, V> get current =>
      (_currentIterator != null) ? _currentIterator.current : null;

  bool moveNext() {
    while (_index < _array.length) {
      if (_currentIterator == null) {
        _currentIterator = _array[_index].iterator;
      }
      if (_currentIterator.moveNext()) {
        return true;
      } else {
        _currentIterator = null;
        _index++;
      }
    }
    return false;
  }
}

class _SubMap<K, V> extends _ANodeBase<K, V> {
  int _bitmap;
  List<_ANodeBase<K, V>> _array;

  _SubMap.abc(_Owner owner, this._bitmap, this._array, int size) : super(owner, size, false);

  factory _SubMap.ensureOwner(_SubMap old, _Owner owner, bitmap, array, int size) {
    if(_ownerEquals(owner, old._owner)) {
      old._bitmap = bitmap;
      old._array = array;
      old._length = size;
    }
    return new _SubMap.abc(owner , bitmap, array, size);
  }

  static _popcount(int n) {
    n = n - ((n >> 1) & 0x55555555);
    n = (n & 0x33333333) + ((n >> 2) & 0x33333333);
    n = (n + (n >> 4)) & 0x0F0F0F0F;
    n = n + (n >> 8);
    n = n + (n >> 16);
    return n & 0x0000003F;
  }

  V _lookup(K key, int hash, int depth) {
    int branch = (hash >> (depth * 5)) & 0x1f;
    int mask = 1 << branch;
    if ((_bitmap & mask) != 0) {
      int index = _popcount(_bitmap & (mask - 1));
      _ANodeBase<K, V> map = _array[index];
      return map._lookup(key, hash, depth + 1);
    } else {
      return _none;
    }
  }

  _NodeBase<K, V> _insertWith(_Owner owner, LinkedList<Pair<K, V>> keyValues, int size,
      V combine(V x, V y), int hash, int depth) {
    assert(size == keyValues.length);

    int branch = (hash >> (depth * 5)) & 0x1f;
    int mask = 1 << branch;
    int index = _popcount(_bitmap & (mask - 1));

    if ((_bitmap & mask) != 0) {
      _ANodeBase<K, V> m = _array[index];
      int oldSize = m.length;
      _ANodeBase<K, V> newM =
                m._insertWith(owner, keyValues, size, combine, hash, depth + 1);
      if(identical(m, newM)) {
        if(oldSize != m.length) this._length += m.length - oldSize;
        return this;
      }
      List<_ANodeBase<K, V>> newarray = _makeCopyIfNeeded(owner, this._owner, _array);
      newarray[index] = newM;
      int delta = newM.length - oldSize;
      return new _SubMap<K, V>.ensureOwner(this, owner, _bitmap, newarray, length + delta);
    } else {
      int newlength = _array.length + 1;
      List<_ANodeBase<K, V>> newarray =
          new List<_ANodeBase<K, V>>(newlength);
      // TODO: find out if there's a "copy array" native function somewhere
      for (int i = 0; i < index; i++) { newarray[i] = _array[i]; }
      for (int i = index; i < newlength - 1; i++) { newarray[i+1] = _array[i]; }
      newarray[index] = new _Leaf<K, V>.abc(owner, hash, keyValues, size);
      return new _SubMap<K, V>.ensureOwner(this, owner, _bitmap | mask, newarray, length + size);
    }
  }

  _NodeBase<K, V> _intersectWith(_Owner owner, LinkedList<Pair<K, V>> keyValues, int size,
      V combine(V x, V y), int hash, int depth) {
    assert(size == keyValues.length);

    int branch = (hash >> (depth * 5)) & 0x1f;
    int mask = 1 << branch;

    if ((_bitmap & mask) != 0) {
      int index = _popcount(_bitmap & (mask - 1));
      _ANodeBase<K, V> m = _array[index];
      return m._intersectWith(owner, keyValues, size, combine, hash, depth + 1);
    } else {
      return new _EmptyMap(owner);
    }
  }

  _NodeBase<K, V> _delete(owner, K key, int hash, int depth, bool safe) {
    int branch = (hash >> (depth * 5)) & 0x1f;
    int mask = 1 << branch;

    if ((_bitmap & mask) != 0) {
      int index = _popcount(_bitmap & (mask - 1));
      _ANodeBase<K, V> m = _array[index];
      int oldSize = m.length;
      _ANodeBase<K, V> newm = m._delete(owner, key, hash, depth + 1, safe);
      int delta = newm.length - oldSize;
      if (identical(m, newm)) {
        this._length += delta;
        return this;
      }
      if (newm is _EmptyMap) {
        if (_array.length > 2) {
          int newsize = _array.length - 1;
          List<_ANodeBase<K, V>> newarray =
              new List<_ANodeBase<K, V>>(newsize);
          for (int i = 0; i < index; i++) { newarray[i] = _array[i]; }
          for (int i = index; i < newsize; i++) { newarray[i] = _array[i + 1]; }
          assert(newarray.length >= 2);
          return new _SubMap.ensureOwner(this, owner, _bitmap ^ mask, newarray, length + delta);
        } else {
          assert(_array.length == 2);
          assert(index == 0 || index == 1);
          _ANodeBase<K, V> onlyValueLeft = _array[1 - index];
          return onlyValueLeft._isLeaf
              ? onlyValueLeft
              : new _SubMap.ensureOwner(this, owner, _bitmap ^ mask,
                            <_ANodeBase<K, V>>[onlyValueLeft],
                            length + delta);
        }
      } else if (newm._isLeaf){
        if (_array.length == 1) {
          return newm;
        } else {
          List<_ANodeBase<K, V>> newarray = _makeCopyIfNeeded(owner, this._owner, _array);
          newarray[index] = newm;
          return new _SubMap.ensureOwner(this, owner, _bitmap, newarray, length + delta);
        }
      } else {
        List<_ANodeBase<K, V>> newarray = _makeCopyIfNeeded(owner, this._owner, _array);
        newarray[index] = newm;

        return new _SubMap.ensureOwner(this, owner, _bitmap, newarray, length + delta);
      }
    } else {
      if(!safe) _ThrowKeyError(key);
      return this;
    }
  }

  _NodeBase<K, V> _adjust(_Owner owner, K key, V update(V), int hash, int depth, bool safe) {
    int branch = (hash >> (depth * 5)) & 0x1f;
    int mask = 1 << branch;
    if ((_bitmap & mask) != 0) {
      int index = _popcount(_bitmap & (mask - 1));
      _ANodeBase<K, V> m = _array[index];
      _ANodeBase<K, V> newm = m._adjust(owner, key, update, hash, depth + 1, safe);
      if (identical(newm, m)) {
        return this;
      }
      List<_ANodeBase<K, V>> newarray = _makeCopyIfNeeded(owner, this._owner, _array);
      newarray[index] = newm;

      return new _SubMap.ensureOwner(this, owner, _bitmap, newarray, length);
    } else {
      return (safe ? this : _ThrowKeyError(key));
    }
  }

  _NodeBase<K, V>
      _unionWith(_Owner owner, _ANodeBase<K, V> m, V combine(V x, V y), int depth) =>
          m._unionWithSubMap(owner, this, combine, depth);

  _NodeBase<K, V>
      _unionWithEmptyMap(_Owner owner, _EmptyMap<K, V> m, V combine(V x, V y), int depth) =>
          this;

  _NodeBase<K, V>
      _unionWithLeaf(_Owner owner, _Leaf<K, V> m, V combine(V x, V y), int depth) =>
          this._insertWith(owner, m._pairs, m.length, (V v1, V v2) => combine(v2, v1),
              m._hash, depth);

  _NodeBase<K, V>
      _unionWithSubMap(_Owner owner, _SubMap<K, V> m, V combine(V x, V y), int depth) {
    int ormap = _bitmap | m._bitmap;
    int andmap = _bitmap & m._bitmap;
    List<_ANodeBase<K, V>> newarray =
        new List<_ANodeBase<K, V>>(_popcount(ormap));
    int mask = 1, i = 0, i1 = 0, i2 = 0;
    int newSize = 0;
    while (mask <= ormap) {
      if ((andmap & mask) != 0) {
        _array[i1];
        m._array[i2];
        _ANodeBase<K, V> newMap =
            m._array[i2]._unionWith(owner, _array[i1], combine, depth + 1);
        newarray[i] = newMap;
        newSize += newMap.length;
        i1++;
        i2++;
        i++;
      } else if ((_bitmap & mask) != 0) {
        _ANodeBase<K, V> newMap = _array[i1];
        newarray[i] = newMap;
        newSize += newMap.length;
        i1++;
        i++;
      } else if ((m._bitmap & mask) != 0) {
        _ANodeBase<K, V> newMap = m._array[i2];
        newarray[i] = newMap;
        newSize += newMap.length;
        i2++;
        i++;
      }
      mask <<= 1;
    }
    return new _SubMap<K, V>.ensureOwner(this, owner, ormap, newarray, newSize);
  }

  _NodeBase<K, V> _intersectionWith(_Owner owner, _ANodeBase<K, V> m,
                                        V combine(V x, V y), int depth) =>
      m._intersectionWithSubMap(owner, this, combine, depth);

  _NodeBase<K, V> _intersectionWithEmptyMap(_Owner owner, _EmptyMap<K, V> m,
                                                V combine(V x, V y),
                                                int depth) =>
      m;

  _NodeBase<K, V> _intersectionWithLeaf(_Owner owner, _Leaf<K, V> m, V combine(V x, V y),
                                            int depth) =>
      _intersectWith(owner, m._pairs,  m.length, (V v1, V v2) => combine(v2, v1),
                     m._hash, depth);

  _NodeBase<K, V> _intersectionWithSubMap(_Owner owner,
      _SubMap<K, V> m, V combine(V x, V y), int depth) {
    int andmap = _bitmap & m._bitmap;
    List<_ANodeBase<K, V>> newarray = new List<_ANodeBase<K, V>>();
    int mask = 1, i1 = 0, i2 = 0;
    int newSize = 0;
    int newMask = 0;
    while (mask <= _bitmap) {
      if ((andmap & mask) != 0) {
        _array[i1];
        m._array[i2];
        _ANodeBase<K, V> newMap =
            m._array[i2]._intersectionWith(owner, _array[i1], combine, depth + 1);
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
      return new _SubMap<K, V>.ensureOwner(this, owner, newMask, newarray, newSize);
    } else if (newarray.length == 1) {
      _ANodeBase<K, V> onlyValueLeft = newarray[0];
      return onlyValueLeft._isLeaf
          ? onlyValueLeft
          : new _SubMap<K, V>.ensureOwner(this, owner, newMask, newarray, newSize);
    } else {
      return new _EmptyMap(owner);
    }
  }

  _NodeBase mapValues(_Owner owner, f(V)) {
    List<_ANodeBase<K, V>> newarray = _makeCopyIfNeeded(owner, this._owner, _array);
    for (int i = 0; i < _array.length; i++) {
      _ANodeBase<K, V> mi = _array[i];
        newarray[i] = mi.mapValues(owner, f);
    }
    return new _SubMap.ensureOwner(this, owner, _bitmap, newarray, length);
  }

  forEachKeyValue(f(K, V)) {
    _array.forEach((mi) => mi.forEachKeyValue(f));
  }

  bool operator ==(_NodeBase<K, V> other) {
    if (identical(this, other)) return true;
    if (other is! _SubMap) return false;
    _SubMap otherSubMap = other;
    if (_bitmap != otherSubMap._bitmap) return false;
    if (length != otherSubMap.length) return false;
    assert(_array.length == otherSubMap._array.length);
    for (int i = 0; i < _array.length; i++) {
      _ANodeBase<K, V> mi = _array[i];
      _ANodeBase<K, V> omi = otherSubMap._array[i];
      if (mi != omi) {
        return false;
      }
    }
    return true;
  }

  Iterator<Pair<K, V>> get iterator => new _SubMapIterator(_array);

  Pair<K, V> get last => _array.last.last;

  Pair<K, V> _elementAt(int index) {
    int newIndex = index;
    for (final subMap in _array) {
      int subLength = subMap.length;
      if (newIndex < subLength) {
        return subMap._elementAt(newIndex);
      }
      newIndex -= subLength;
    }
    throw new StateError("never happens");
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
