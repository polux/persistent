// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

part of persistent;

final _random = new Random();

class PersistentMapImpl<K, V>
        extends IterableBase<Pair<K, V>>
        implements PersistentMap<K, V> {
  NodeBase _root;

  int _hash;

  int get hashCode {
    if(_hash != null) return _hash;
    _hash = 0;
    this.forEachKeyValue((key, value) {
      _hash += key.hashCode ^ value.hashCode;
    });
    return _hash;
  }

  bool operator==(other) {
    if (other is! PersistentMapImpl) return false;
    if(other.hashCode != this.hashCode || this.length != other.length)
      return false;
    bool equals = true;
    this.forEachKeyValue((key, dynamic value) {
      equals = equals && other.containsKey(key) && other[key] == value;
    });
    return equals;
  }

  PersistentMapImpl() {
    this._root = new _EmptyMap<K, V>(null);
  }

  PersistentMapImpl.fromMap(Map<K, V> map) {
    _root = new _EmptyMap<K, V>(null);
    Owner owner = new Owner();
    map.forEach((K key, V value) {
      _root = _root.insert(owner, key, value);
    });
  }

  PersistentMapImpl.fromPairs(Iterable<Pair<K, V>> pairs) {
    _root = new _EmptyMap<K, V>(null);
    Owner owner = new Owner();
    pairs.forEach((pair) {
      _root = _root.insert(owner, pair.fst, pair.snd);
    });
  }

  PersistentMapImpl.fromTransient(TransientMapImpl map) {
    this._root = map._root;
  }

  PersistentMapImpl._new(NodeBase this._root);

  PersistentMapImpl<K, V>
      insert(K key, V value, [V combine(V oldvalue, V newvalue)]) {
        return new PersistentMapImpl._new(_root.insert(null, key, value, combine));
      }

  PersistentMapImpl<K, V>
        insertIn(List<K> path, V value, [V combine(V oldvalue, V newvalue), offset = 0]) {
      if(offset +1 == path.length)
        return new PersistentMapImpl._new(_root.insert(null, path.last, value));

      return new PersistentMapImpl._new(
          _root.adjust(null, path[offset],
          (e) => e.insertIn(path, value, combine, offset+1)));
    }

  PersistentMapImpl<K, V> delete(K key) {
    return new PersistentMapImpl._new(_root.delete(null, key));
  }

  PersistentMapImpl<K, V> deleteIn(List<K> path, [offset = 0]) {
    if(offset +1 == path.length)
      return new PersistentMapImpl._new(_root.delete(null, path.last));

    return new PersistentMapImpl._new(
        _root.adjust(null, path[offset],
        (e) => e.deleteIn(path, offset+1)));
  }

  V lookup(K key, [dynamic orElse()]) {
    var val = _root.lookup(key);
    if(isNone(val)) return (orElse == null ? null : orElse());
    return val;
  }

  lookupIn(List<K> path, [offset = 0, dynamic orElse()]) {
    dynamic e = lookup(path[offset]);
    offset++;

    if(path.length == offset) return e;
    else return !isNone(e) ?
        e.lookupIn(path, offset, orElse)
      :
        (orElse == null ? null : orElse());
  }

  V operator [](K key) =>
      lookup(key, () => throw new Exception('Key is not defined'));


  void forEachKeyValue(f(K key, V value)) => _root.forEachKeyValue(f);

  PersistentMapImpl<K, V> adjust(K key, V update(V value)) {
    return  new PersistentMapImpl._new(_root.adjust(null, key, update));
  }

  PersistentMapImpl adjustIn(List<K> path, V update(V value), [offset = 0]) {
    if(path.length == offset +1) return adjust(path[offset], update);

    return new PersistentMapImpl._new(
           _root.adjust(null, path[offset],
           (e) => e.adjustIn(path, update, offset+1)));
  }

  PersistentMapImpl mapValues(f(V value)) {
    Owner owner = new Owner();
    return new PersistentMapImpl._new(_root.mapValues(owner, f));
  }

  PersistentMapImpl<K, V>
      union(PersistentMapImpl<K, V> other, [V combine(V left, V right)]) {
        Owner owner = new Owner();
        return new PersistentMapImpl._new(_root.union(owner, other._root, combine));
      }

  PersistentMapImpl<K, V>
    intersection(PersistentMapImpl<K, V> other, [V combine(V left, V right)]) =>
      new PersistentMapImpl._new(_root.intersection(null, other._root, combine));

  Map<K, V> toMap() {
    return _root.toMap();
  }

  Iterable<K> get keys => _root.keys;

  Iterable<V> get values => _root.values;

  Pair<K, V> pickRandomEntry([Random random]) => _root.pickRandomEntry(random);

  PersistentMapImpl strictMap(Pair f(Pair<K, V> pair)) =>
      new PersistentMapImpl.fromPairs(this.map(f));

  PersistentMapImpl<K, V> strictWhere(bool f(Pair<K, V> pair)) =>
      new PersistentMapImpl<K, V>.fromPairs(this.where(f));

  Iterator get iterator => _root.iterator;

  int get length => _root.length;

  bool containsKey(key) {
    final value = this.lookup(key);
    return !isNone(value);
  }

  TransientMap asTransient() {
    return new TransientMapImpl.fromPersistent(this);
  }

  PersistentMapImpl withTransient(dynamic f(TransientMap map)) {
    TransientMap transient = this.asTransient();
    f(transient);
    return transient.asPersistent();
  }
  toString() => 'PersistentMap$_root';
}

class TransientMapImpl<K, V>
        extends IterableBase<Pair<K, V>>
        implements Iterable<Pair<K, V>>, TransientMap<K, V> {
  NodeBase _root;
  Owner _owner;
  get owner => _owner != null ?
      _owner
    :
      throw new Exception('Cannot modify TransientMap after calling asPersistent.');

  factory TransientMapImpl() => new TransientMapImpl.fromPersistent(new PersistentMap());

  /**
   * Creates an immutable copy of [map] using the default implementation of
   * [TransientMap].
   */
  TransientMapImpl.fromPersistent(PersistentMapImpl<K, V> map) {
    _owner = new Owner();
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

  TransientMap<K, V>
     doInsertIn(List<K> path, V value, [V combine(V oldvalue, V newvalue), offset = 0]) {
       if(offset +1 == path.length)
         return _adjustRootAndReturn(_root.insert(null, path.last, value));

       return _adjustRootAndReturn(
           _root.adjust(null, path[offset],
           (e) => e.insertIn(path, offset+1)));
     }

  TransientMap<K, V> doDelete(K key) {
    return _adjustRootAndReturn(_root.delete(owner, key));
  }

  TransientMap<K, V> doDeleteIn(List<K> path, [offset = 0]) {
    if(offset +1 == path.length)
      return doDelete(path.last);

    return _adjustRootAndReturn(
        _root.adjust(null, path[offset],
        (e) => e.deleteIn(path, offset+1)));
  }

  V doLookup(K key, [dynamic orElse()]) {
    var val = _root.lookup(key);
    if(isNone(val)) return (orElse == null ? null : orElse());
    return val;
  }

  doLookupIn(List<K> path, [offset = 0, dynamic orElse()]) {
    dynamic e = doLookup(path[offset]);
    offset++;

    if(path.length == offset) return e;
    else return !isNone(e) ?
        e.lookupIn(path, offset, orElse)
      :
        (orElse == null ? null : orElse());
  }

  V operator [](K key) =>
      doLookup(key, () => throw new Exception('Key is not defined'));

  void doForEachKeyValue(f(K key, V value)) => _root.forEachKeyValue(f);

  TransientMap<K, V> doAdjust(K key, V update(V value)) {
    return _adjustRootAndReturn(_root.adjust(owner, key, update));
  }

  TransientMap doAdjustIn(List<K> path, V update(V value), [offset = 0]) {
    if(path.length == offset +1) return doAdjust(path[offset], update);

    return _adjustRootAndReturn(
             _root.adjust(null, path[offset], (e) => e.adjustIn(path, update, offset+1)));
  }

  TransientMap doMapValues(f(V value)) {
    return _adjustRootAndReturn(_root.mapValues(owner, f));
  }

  TransientMap<K, V>
      doUnion(TransientMapImpl<K, V> other, [V combine(V left, V right)]) =>
          _adjustRootAndReturn(_root.union(owner, other._root, combine));

  TransientMap<K, V>
    doIntersection(TransientMapImpl<K, V> other, [V combine(V left, V right)]) =>
        _adjustRootAndReturn(_root.intersection(owner, other._root, combine));

  /// Returns a mutable copy of `this`.
  Map<K, V> toMap() {
    return _root.toMap();
  }

  /// The keys of `this`.
  Iterable<K> get keys => _root.keys;

  /// The values of `this`.
  Iterable<V> get values => _root.values;

  /// Randomly picks an entry of `this`.
  Pair<K, V> doPickRandomEntry([Random random]) => _root.pickRandomEntry(random);

  Iterator get iterator => _root.iterator;

  int get length => _root.length;

  bool containsKey(key) {
    final value = this.doLookup(key);
    return !isNone(value);
  }

  PersistentMap asPersistent() {
    _owner = null;
    return new PersistentMapImpl.fromTransient(this);
  }

  toString() => 'TransientMap(${owner.hashCode}, $_root)';
}

/**
 * Exception used for aborting forEach loops.
 */
class _Stop implements Exception {}

class Owner {}

abstract class NodeBase<K, V>
    extends IterableBase<Pair<K, V>> {

  int _length;
  get length => _length;

  NodeBase(this._length);

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

  NodeBase<K, V>
    insert(Owner owner, K key, V value, [V combine(V oldvalue, V newvalue)]);

  NodeBase<K, V> delete(Owner owner, K key);

  V lookup(K key);

  void forEachKeyValue(f(K key, V value));

  NodeBase<K, V> adjust(Owner owner, K key, V update(V value));

  NodeBase mapValues(Owner owner, f(V value));

  NodeBase<K, V>
      union(Owner owner, NodeBase<K, V> other, [V combine(V left, V right)]);

  NodeBase<K, V>
      intersection(Owner owner, NodeBase<K, V> other, [V combine(V left, V right)]);

}

/**
 * Superclass for _EmptyMap, _Leaf and _SubMap.
 */
abstract class _ANodeBase<K, V> extends NodeBase<K, V> {
  Owner _owner;

  _ANodeBase(this._owner, length, this._isLeaf) : super(length);

  final bool _isLeaf;

  V _lookup(K key, int hash, int depth);
  NodeBase<K, V> _insertWith(Owner owner, LinkedList<Pair<K, V>> keyValues, int size,
      V combine(V x, V y), int hash, int depth);
  NodeBase<K, V> _intersectWith(Owner owner, LinkedList<Pair<K, V>> keyValues, int size,
      V combine(V x, V y), int hash, int depth);
  NodeBase<K, V> _delete(Owner owner, K key, int hash, int depth);
  NodeBase<K, V> _adjust(Owner owner, K key, V update(V), int hash, int depth);

  _ANodeBase<K, V>
      _unionWith(Owner owner, _ANodeBase<K, V> m, V combine(V x, V y), int depth);
  _ANodeBase<K, V>
      _unionWithEmptyMap(Owner owner, _EmptyMap<K, V> m, V combine(V x, V y), int depth);
  _ANodeBase<K, V>
      _unionWithLeaf(Owner owner, _Leaf<K, V> m, V combine(V x, V y), int depth);
  _ANodeBase<K, V>
      _unionWithSubMap(Owner owner, _SubMap<K, V> m, V combine(V x, V y), int depth);

  _ANodeBase<K, V>
      _intersectionWith(Owner owner, _ANodeBase<K, V> m, V combine(V x, V y),
                        int depth);
  _ANodeBase<K, V>
      _intersectionWithEmptyMap(Owner owner, _EmptyMap<K, V> m, V combine(V x, V y),
                                int depth);
  _ANodeBase<K, V>
      _intersectionWithLeaf(Owner owner, _Leaf<K, V> m, V combine(V x, V y), int depth);
  _ANodeBase<K, V>
      _intersectionWithSubMap(Owner owner, _SubMap<K, V> m, V combine(V x, V y), int depth);

  Pair<K, V> _elementAt(int index);

  LinkedList<Pair<K, V>> _onePair(K key, V value) =>
      new Cons<Pair<K, V>>(new Pair<K, V>(key, value), new Nil<Pair<K, V>>());

  V lookup(K key) =>
      _lookup(key, key.hashCode & 0x3fffffff, 0);

  NodeBase<K, V> insert(Owner owner, K key, V value, [V combine(V x, V y)]) =>
      _insertWith(owner, _onePair(key, value),
          1,
          (combine != null) ? combine : (V x, V y) => y,
          key.hashCode & 0x3fffffff, 0);

  NodeBase<K, V> delete(Owner owner, K key) =>
      _delete(owner ,key, key.hashCode & 0x3fffffff, 0);

  NodeBase<K, V> adjust(Owner owner, K key, V update(V)) =>
      _adjust(owner, key, update, key.hashCode & 0x3fffffff, 0);

  NodeBase<K, V> union(Owner owner, NodeBase<K, V> other, [V combine(V x, V y)]) =>
    this._unionWith(owner, other, (combine != null) ? combine : (V x, V y) => y, 0);

  NodeBase<K, V>
      intersection(Owner owner, NodeBase<K, V> other, [V combine(V left, V right)]) =>
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
  _EmptyMap(Owner owner) : super(owner, 0, false);

  V _lookup(K key, int hash, int depth) => none();

  NodeBase<K, V> _insertWith(Owner owner,
      LinkedList<Pair<K, V>> keyValues, int size, V combine(V x, V y), int hash,
      int depth) {
    assert(size == keyValues.length);
    return new _Leaf<K, V>.abc(owner, hash, keyValues, size);
  }

  NodeBase<K, V> _intersectWith(Owner owner, LinkedList<Pair<K, V>> keyValues, int size,
      V combine(V x, V y), int hash, int depth) {
    assert(size == keyValues.length);
    return this;
  }

  NodeBase<K, V> _delete(Owner owner, K key, int hash, int depth) => this;

  NodeBase<K, V> _adjust(Owner owner, K key, V update(V), int hash, int depth) => this;

  NodeBase<K, V>
      _unionWith(Owner owner, NodeBase<K, V> m, V combine(V x, V y), int depth) => m;

  NodeBase<K, V>
      _unionWithEmptyMap(Owner owner, _EmptyMap<K, V> m, V combine(V x, V y), int depth) {
    throw "should never be called";
  }

  NodeBase<K, V>
      _unionWithLeaf(Owner owner, _Leaf<K, V> m, V combine(V x, V y), int depth) => m;

  NodeBase<K, V>
      _unionWithSubMap(Owner owner, _SubMap<K, V> m, V combine(V x, V y), int depth) => m;

  NodeBase<K, V>
      _intersectionWith(Owner owner, _ANodeBase<K, V> m, V combine(V x, V y),
                        int depth) => this;

  NodeBase<K, V>
      _intersectionWithEmptyMap(Owner owner, _EmptyMap<K, V> m, V combine(V x, V y),
                                int depth) {
        throw "should never be called";
  }

  NodeBase<K, V> _intersectionWithLeaf(Owner owner,
      _Leaf<K, V> m, V combine(V x, V y), int depth) => this;

  NodeBase<K, V> _intersectionWithSubMap(Owner owner,
      _SubMap<K, V> m, V combine(V x, V y), int depth) => this;

  NodeBase mapValues(Owner owner, f(V)) => this;

  void forEachKeyValue(f(K, V)) {}

  bool operator ==(NodeBase<K, V> other) => other is _EmptyMap;

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

  _Leaf.abc(Owner owner, this._hash, pairs, int size) : super(owner, size, true) {
    this._pairs = pairs;
    assert(size == pairs.length);
  }

  factory _Leaf.ensureOwner(_Leaf old, Owner owner, hash, pairs, int size) {
    if(ownerEquals(owner, old._owner)) {
      old._pairs = pairs;
      old._hash = hash;
      old._length = size;
      return old;
    }
    return new _Leaf.abc(owner, hash, pairs, size);
  }

  NodeBase<K, V> _insertWith(Owner owner, LinkedList<Pair<K, V>> keyValues, int size,
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

  NodeBase<K, V> _intersectWith(Owner owner, LinkedList<Pair<K, V>> keyValues, int size,
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

  NodeBase<K, V> _delete(Owner owner, K key, int hash, int depth) {
    if (hash != _hash)
      return this;
    bool found = false;
    LinkedList<Pair<K, V>> newPairs = _pairs.strictWhere((p) {
      if (p.fst == key) {
        found = true;
        return false;
      }
      return true;
    });
    return newPairs.isNil
        ? new _EmptyMap<K, V>(owner)
        : new _Leaf<K, V>.ensureOwner(this, owner, _hash, newPairs, found ? length - 1 : length);
  }

  NodeBase<K, V> _adjust(Owner owner, K key, V update(V), int hash, int depth) {
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
      return builder.build();
    }

    return (hash != _hash)
        ? this
        : new _Leaf<K, V>.ensureOwner(this, owner, _hash, adjustPairs(), length);
  }

  NodeBase<K, V>
      _unionWith(Owner owner, _ANodeBase<K, V> m, V combine(V x, V y), int depth) =>
          m._unionWithLeaf(owner, this, combine, depth);

  NodeBase<K, V>
      _unionWithEmptyMap(Owner owner, _EmptyMap<K, V> m, V combine(V x, V y), int depth) =>
          this;

  NodeBase<K, V>
      _unionWithLeaf(Owner owner, _Leaf<K, V> m, V combine(V x, V y), int depth) =>
          m._insertWith(owner, _pairs, length, combine, _hash, depth);

  NodeBase<K, V>
      _unionWithSubMap(Owner owner, _SubMap<K, V> m, V combine(V x, V y), int depth) =>
          m._insertWith(owner, _pairs, length, combine, _hash, depth);

  NodeBase<K, V> _intersectionWith(Owner owner, _ANodeBase<K, V> m,
                                        V combine(V x, V y), int depth) =>
      m._intersectionWithLeaf(owner, this, combine, depth);

  NodeBase<K, V> _intersectionWithEmptyMap(Owner owner, _EmptyMap<K, V> m,
                                                V combine(V x, V y),
                                                int depth) =>
      m;

  NodeBase<K, V> _intersectionWithLeaf(Owner owner, _Leaf<K, V> m, V combine(V x, V y),
                                            int depth) =>
      m._intersectWith(owner, _pairs, length, combine, _hash, depth);

  NodeBase<K, V> _intersectionWithSubMap(Owner owner, _SubMap<K, V> m,
                                              V combine(V x, V y), int depth) =>
      m._intersectWith(owner, _pairs, length, combine, _hash, depth);

  V _lookup(K key, int hash, int depth) {
    if (hash != _hash)
      return none();
    LinkedList<Pair<K, V>> it = _pairs;
    while (it.isCons) {
      Cons<Pair<K, V>> cons = it.asCons;
      Pair<K, V> elem = cons.elem;
      if (elem.fst == key) return elem.snd;
      it = cons.tail;
    }
    return none();
  }

  NodeBase mapValues(Owner owner, f(V)) =>
      new _Leaf.ensureOwner(this, owner, _hash,
                _pairs.strictMap((p) => new Pair(p.fst, f(p.snd))), length);

  void forEachKeyValue(f(K, V)) {
    _pairs.foreach((Pair<K, V> pair) => f(pair.fst, pair.snd));
  }

  bool operator ==(NodeBase<K, V> other) {
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

  _SubMap.abc(Owner owner, this._bitmap, this._array, int size) : super(owner, size, false);

  factory _SubMap.ensureOwner(_SubMap old, Owner owner, bitmap, array, int size) {
    if(ownerEquals(owner, old._owner)) {
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
      return none();
    }
  }

  NodeBase<K, V> _insertWith(Owner owner, LinkedList<Pair<K, V>> keyValues, int size,
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
      List<_ANodeBase<K, V>> newarray = makeCopyIfNeeded(owner, this._owner, _array);
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

  NodeBase<K, V> _intersectWith(Owner owner, LinkedList<Pair<K, V>> keyValues, int size,
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

  NodeBase<K, V> _delete(owner, K key, int hash, int depth) {
    int branch = (hash >> (depth * 5)) & 0x1f;
    int mask = 1 << branch;

    if ((_bitmap & mask) != 0) {
      int index = _popcount(_bitmap & (mask - 1));
      _ANodeBase<K, V> m = _array[index];
      int oldSize = m.length;
      _ANodeBase<K, V> newm = m._delete(owner, key, hash, depth + 1);
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
          List<_ANodeBase<K, V>> newarray = makeCopyIfNeeded(owner, this._owner, _array);
          newarray[index] = newm;
          return new _SubMap.ensureOwner(this, owner, _bitmap, newarray, length + delta);
        }
      } else {
        List<_ANodeBase<K, V>> newarray = makeCopyIfNeeded(owner, this._owner, _array);
        newarray[index] = newm;

        return new _SubMap.ensureOwner(this, owner, _bitmap, newarray, length + delta);
      }
    } else {
      return this;
    }
  }

  NodeBase<K, V> _adjust(Owner owner, K key, V update(V), int hash, int depth) {
    int branch = (hash >> (depth * 5)) & 0x1f;
    int mask = 1 << branch;
    if ((_bitmap & mask) != 0) {
      int index = _popcount(_bitmap & (mask - 1));
      _ANodeBase<K, V> m = _array[index];
      _ANodeBase<K, V> newm = m._adjust(owner, key, update, hash, depth + 1);
      if (identical(newm, m)) {
        return this;
      }
      List<_ANodeBase<K, V>> newarray = makeCopyIfNeeded(owner, this._owner, _array);
      newarray[index] = newm;

      return new _SubMap.ensureOwner(this, owner, _bitmap, newarray, length);
    } else {
      return this;
    }
  }

  NodeBase<K, V>
      _unionWith(Owner owner, _ANodeBase<K, V> m, V combine(V x, V y), int depth) =>
          m._unionWithSubMap(owner, this, combine, depth);

  NodeBase<K, V>
      _unionWithEmptyMap(Owner owner, _EmptyMap<K, V> m, V combine(V x, V y), int depth) =>
          this;

  NodeBase<K, V>
      _unionWithLeaf(Owner owner, _Leaf<K, V> m, V combine(V x, V y), int depth) =>
          this._insertWith(owner, m._pairs, m.length, (V v1, V v2) => combine(v2, v1),
              m._hash, depth);

  NodeBase<K, V>
      _unionWithSubMap(Owner owner, _SubMap<K, V> m, V combine(V x, V y), int depth) {
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

  NodeBase<K, V> _intersectionWith(Owner owner, _ANodeBase<K, V> m,
                                        V combine(V x, V y), int depth) =>
      m._intersectionWithSubMap(owner, this, combine, depth);

  NodeBase<K, V> _intersectionWithEmptyMap(Owner owner, _EmptyMap<K, V> m,
                                                V combine(V x, V y),
                                                int depth) =>
      m;

  NodeBase<K, V> _intersectionWithLeaf(Owner owner, _Leaf<K, V> m, V combine(V x, V y),
                                            int depth) =>
      _intersectWith(owner, m._pairs,  m.length, (V v1, V v2) => combine(v2, v1),
                     m._hash, depth);

  NodeBase<K, V> _intersectionWithSubMap(Owner owner,
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

  NodeBase mapValues(Owner owner, f(V)) {
    List<_ANodeBase<K, V>> newarray = makeCopyIfNeeded(owner, this._owner, _array);
    for (int i = 0; i < _array.length; i++) {
      _ANodeBase<K, V> mi = _array[i];
        newarray[i] = mi.mapValues(owner, f);
    }
    return new _SubMap.ensureOwner(this, owner, _bitmap, newarray, length);
  }

  forEachKeyValue(f(K, V)) {
    _array.forEach((mi) => mi.forEachKeyValue(f));
  }

  bool operator ==(NodeBase<K, V> other) {
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

ownerEquals(Owner a, Owner b) {
  return a != null && a == b;
}

makeCopyIfNeeded(Owner a, Owner b, List c) {
  if(ownerEquals(a, b))
    return c;
  else return c.sublist(0);
}
