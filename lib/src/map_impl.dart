// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

part of persistent;

final _random = new Random();


//const branching = 16;
//const branchingBits = 4;
//const branchingMask = 0xf;

const branching = 32;
const branchingBits = 5;
const branchingMask = 0x1f;

const leafSize = branching * 3;
const leafSizeMin = branching * 2;

const binSearchThr = 4;
const recsize = 3; //0 - key, 1 - val, 2 - hash


_ThrowKeyError(key) => throw new Exception('Key Error: ${key} is not defined');

_ThrowUpdateKeyError(key, exception) => throw new Exception('Key $key was not found, calling update with no arguments threw: $exception');

_getUpdateValue(key, updateF) {
  try {
    return updateF();
  } catch(e) {
    _ThrowUpdateKeyError(key, e);
  }
}

_reverseHash(hash){
  var _tmp = hash ^ (hash << 8);
  return ((_tmp ^ (_tmp << 16)) & 0x3fffffff);
}

_getBranch(hash, depth){
  return (hash >> (depth*branchingBits)) & branchingMask;
}



class _TransientMapImpl<K, V>
        extends IterableBase<Pair<K, V>>
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
  _TransientMapImpl.fromPersistent(_Node<K, V> map) {
    _owner = new _Owner();
    _root = map;
  }

  TransientMap _adjustRootAndReturn(newRoot) {
    _root = newRoot;
    return this;
  }

  TransientMap<K, V>
      doAssoc(K key, V value) {
        return _adjustRootAndReturn(_root._assoc(owner, key, value));
      }

  operator []=(key, value){
    this.doAssoc(key, value);
  }

  TransientMap<K, V> doDelete(K key, {bool missingOk: false}) {
    return _adjustRootAndReturn(_root._delete(owner, key, _reverseHash(key.hashCode), 5, missingOk));
  }

  TransientMap<K, V> doUpdate(K key, dynamic updateF) {
    return _adjustRootAndReturn(_root._update(owner, key, updateF));
  }

  PersistentMap asPersistent() {
    _owner = null;
    return this._root;
  }

  toString() => 'TransientMap($_root)';

  V get(K key, [V notFound = _none]) => _root.get(key, notFound);

  V operator [](K key) => _root.get(key);

  void forEachKeyValue(f) => _root.forEachKeyValue(f);

  Map<K, V> toMap() =>_root.toMap();

  Iterable<K> get keys => _root.keys;

  Iterable<V> get values => _root.values;

  Iterator get iterator => _root.iterator;

  int get length => _root.length;

  bool containsKey(key) => _root.containsKey(key);

  bool hasKey(key) => _root.hasKey(key);

}


/**
 * Superclass for _EmptyMap, _Leaf and _SubMap.
 */
abstract class _Node<K, V> extends IterableBase<Pair<K, V>> implements PersistentMap<K, V> {
  _Owner _owner;
  int _length;
  int _hash;
  get length => _length;

  _Node(this._owner, this._length);

  factory _Node.fromMap(map){
    _Node root = new _Leaf.empty(null);
    map.forEach((K key, V value) {
      root = root._assoc(null, key, value);
    });
    return root;
  }

  factory _Node.fromPairs(pairs){
    var _root = new _Leaf.empty(null);
    pairs.forEach((pair) {
      _root = _root._assoc(null, pair.first, pair.second);
    });
    return _root;
  }

  _forEachKVSegment(f);

  V _get(K key, int hash, int depth);
  _Node<K, V> _insertOneWith(_Owner owner, key, val, hash, int depth, [update]);

  int get hashCode;

  _Node<K, V> _update(_Owner owner, K key, dynamic updateF){
    return _insertOneWith(owner, key, null, _reverseHash(key.hashCode), 5, updateF);
  }

  PersistentMap<K, V> update(K key, dynamic updateF) =>
    _insertOneWith(null, key, null, _reverseHash(key.hashCode), 5, updateF);

  _Node<K, V> _assoc(_Owner owner, K key, V value) =>
      _insertOneWith(owner, key, value, _reverseHash(key.hashCode), 5);

  PersistentMap assoc(K key, V value) => _assoc(null, key, value);

  _Node<K, V> _delete(_Owner owner, K key, int hash, int depth, bool missingOk);

  PersistentMap delete(K key, {bool missingOk: false}) => _delete(null, key, _reverseHash(key.hashCode), 5, missingOk);

//  intersection(other){
//    _Leaf me = this;
//    other
//  }

//  bool operator ==(other) {
//    if (other is! _Node) return false;
//    if (identical(this, other)) return true;
//    if (this.length != other.length) {
//      return false;
//    }
//    for (Pair p in this) {
//      if (other.get(p.first, _none) != p.second) {
//        return false;
//      }
//    }
//    return true;
//  }

  bool operator ==(other) {
    if (other is! _Node) return false;
    if (identical(this, other)) return true;
    if (this.length != other.length) {
      return false;
    }
    bool res = true;
    this.forEachKeyValue((k,v){
      res = res && (other.get(k, _none) == v);
    });
    return res;
  }


//  bool operator ==(other) {
//    if (other is! _Node) return false;
//    if (identical(this, other)) return true;
//    _Node me = this;
//    if (me.length != other.length) {
//      return false;
//    }
//    if (me is _Leaf && other is _Leaf) {
//      List mekv = (me as _Leaf)._kv;
//      List okv = other._kv;
//      var lastMatch=0;
//      for (int i=0; i<mekv.length; i+=recsize) {
//        if (mekv[i+2] == okv[i+2]) {
//          // same hash
//          if (mekv[i] == okv[i]) {
//            // same hash, same key
//            if (mekv[i+1] == okv[i+1]) {
//              // match
//              lastMatch = i;
//              continue;
//            } else {
//              // same key, but not value
//              return false;
//            }
//          } else {
////            if (_isNone((me as _Leaf).sameHashGet(mekv[i], mekv[i+2], lastMatch))){
////              return false;
////            }
//            // same hash, different key
//            var hash = mekv[i+2];
//            // find boundaries of same-hash regions
//            var bm, bo;
//            for (var j=i; j<mekv.length; j+=recsize){
//              var fm = mekv[j+2] == hash;
//              var fo = okv[j+2] == hash;
//              if (fm) bm=j;
//              if (fo) bo=j;
//              if (!fm && !fo) break;
//            }
//            if (bm != bo) return false;
//            // scan the same-hash regions
//            for (var j=i; j<=bm; j+=recsize) {
//              bool res = false;
//              for (var k=i; k<bo; k+=recsize) {
//                res = res || (mekv[j] == okv[k]) && (mekv[j+1] == okv[k+1]);
//              }
//              if (!res) {
//                return false;
//              }
//            }
//          }
//        } else {
//          // different hash
//          return false;
//        }
//      }
//      return true;
//    }
//    if (me is _SubMap && other is _SubMap) {
//      for (int i=0; i<branching; i++) {
//        if((me as _SubMap)._array[i] != other._array[i]) {
//          return false;
//        }
//      }
//      return true;
//    }
//    if (me is _SubMap && other is _Leaf) {
//      var _tmp = other;
//      other = me;
//      me = _tmp;
//    }
//    if (me is _Leaf && other is _SubMap) {
//      for (Pair p in other) {
//        if (me[p.first] != p.second) {
//          return false;
//        }
//      }
//      return true;
//    }
//    throw new Exception('Should not get here');
//    return null;
//  }

  // method must be called only on top-level _Node
  V get(K key, [V notFound = _none]) {
    var val = _get(key, _reverseHash(key.hashCode), 5);
    if(_isNone(val)){
      if (_isNone(notFound)) {
        _ThrowKeyError(key);
        return null;
      } else {
        return notFound;
      }
    } else {
      return val;
    }
  }

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

  void forEachKeyValue(f(K key, V value));

  bool containsKey(key) {
    final _none = new Object();
    final value = this.get(key, _none);
    return value != _none;
  }

  bool hasKey(key) => containsKey(key);

  TransientMap asTransient() {
    return new _TransientMapImpl.fromPersistent(this);
  }

  PersistentMap withTransient(dynamic f(TransientMap map)) {
    TransientMap transient = this.asTransient();
    f(transient);
    return transient.asPersistent();
  }

  V operator [](K key) => get(key);

  PersistentMap strictMap(Pair<K, V> f(Pair<K, V> pair)) =>
      new PersistentMap.fromPairs(this.map(f));

  PersistentMap<K, V> strictWhere(bool f(Pair<K, V> pair)) =>
      new PersistentMap<K, V>.fromPairs(this.where(f));
}

class _Leaf<K, V> extends _Node<K, V> {
  List _kv;
  get private_kv => _kv;

  get iterator {
    List<Pair<K, V>> pairs = [];
    for (int i=0; i<_kv.length; i+=recsize){
      pairs.add(new Pair(_kv[i], _kv[i+1]));
    }
    return pairs.iterator;
  }

  void sanityCheck(){
    var lasthash = double.NEGATIVE_INFINITY;
    for (int i=0; i<_kv.length; i+=recsize){
      if (lasthash>_kv[i+2]) {
        throw new Exception('invariant violated');
      }
      lasthash = _kv[i+2];
    }
  }

  int get hashCode {
    if(_hash != null) return _hash;
    _hash = 0;
    for(int i=0; i<_kv.length; i+=recsize){
      _hash ^= hash2(_kv[i+2], _kv[i+1].hashCode);

    }
    return _hash;
  }

  _Leaf.abc(_Owner owner, _kv) : super(owner, _kv.length ~/ recsize){
    this._kv = _kv;
  }

  _Leaf.empty(_Owner owner): super(owner, 0) {
    this._kv = [];
  }

  factory _Leaf.ensureOwner(_Leaf old, _Owner owner, kv, int length) {
    if(_ownerEquals(owner, old._owner)) {
      old._kv = kv;
      old._length = length;
      return old;
    }
    return new _Leaf.abc(owner, kv);
  }

  dynamic sameHashGet(key, hash, start) {
    for (int i=start; i<_kv.length; i+=recsize){
      if (hash == _kv[i+2]) {
        if (key==_kv[i]) {
          return _kv[i+1];
        } else {
          return _none;
        }
      }
    }
    return _none;
  }

  int sameHashRegionLength(hash, start) {
    int res;
    for (int i=start; i<_kv.length; i+=recsize){
      if (hash == _kv[i+2]) {
        res = i;
      }
    }
    return res;
  }

//  _Node<K, V> polish(_Owner owner, int depth, List _kv) {
//    assert(_kv.length % recsize == 0);
//    if (_kv.length < recsize*leafSize) {
//      return new _Leaf.abc(owner, _kv, _kv.length ~/ recsize);
//    } else {
//      List<List> kvs = new List.generate(branching, (_) => []);
//      var from = 0;
//      var to;
//      for (int branch=0; branch < branching; branch++){
//        to = from;
//        while(to < _kv.length && _getBranch(_kv[to+2], depth) == branch){
//          to += recsize;
//        }
//        kvs[branch].length = to-from;
//        kvs[branch].setRange(0, to-from, _kv, from);
//        from = to;
//      }
//
//      List <_Node<K, V>> array = new List.generate(branching,
//          (i) => new _Leaf.abc(owner, kvs[i], kvs[i].length ~/ recsize));
//      return new _SubMap.abc(owner, array, _kv.length ~/ recsize);
//    }
//  }


  _Node<K, V> polish(_Owner owner, int depth, List _kv) {
    assert(_kv.length % recsize == 0);
    // depth == 0 means we are at the bottom level; we consumed all
    // information from 'hash' and we have to extend the _Leaf no matter how
    // long it gets
    if (_kv.length < recsize * leafSize || depth == 0) {
      return new _Leaf.abc(owner, _kv);
    } else {
      List<List> kvs = new List.generate(branching, (_) => []);
      for (int i=0; i<_kv.length; i+=recsize){
        int branch = _getBranch(_kv[i+2], depth);
        kvs[branch].add(_kv[i]);
        kvs[branch].add(_kv[i + 1]);
        kvs[branch].add(_kv[i + 2]);
      }
      List <_Node<K, V>> array = new List.generate(branching,
          (i) => new _Leaf.abc(owner, kvs[i]));
      return new _SubMap.abc(owner, array, _kv.length ~/ recsize);
    }
  }

  _insert(List into, key, val, hash, [update]){
    assert(into.length % recsize == 0);
    if (into.length == 0) {
      into.addAll([key, val, hash]);
      return;
    }
    int from = 0;
    int to = into.length - recsize;
    while(from - to > recsize * binSearchThr){
      int mid = (from + to) ~/ 2;
      if (into[mid + 2] > hash){
        to = mid;
      } else if (into[mid + 2] < hash){
        from = mid;
      } else {
        break;
      }
    }

    for (int i=from; i<=to; i+=recsize) {
      assert(i%recsize == 0);
      if (hash <= into[i+2]) {
        if (hash < into[i+2]) {
          into.insertAll(i, [key, val, hash]);
          return;
        }
        if (key == into[i]) {
          if (update == null) {
            into[i+1] = val;
          } else {
            into[i+1] = update(into[i+1]);
          }
          return;
        }
      }
    }

    if (update == null) {
      into.addAll([key, val, hash]);
    } else {
      into.addAll([key, _getUpdateValue(key, update), hash]);
    }
    assert(into.length % recsize == 0);
  }

  _Node<K, V> _insertOneWith(_Owner owner, key, val, hash, int depth, [update]) {
    List nkv = _makeCopyIfNeeded(owner, this._owner, _kv);
    _insert(nkv, key, val, hash, update);
    return polish(owner, depth, nkv);
  }

  _Node<K, V> _delete(_Owner owner, K key, int hash, int depth, bool missingOk) {
    bool found = false;
    List nkv = _makeCopyIfNeeded(owner, this._owner, _kv);
    for (int i=0; i<nkv.length; i+=recsize){
      if (nkv[i+2] == hash && nkv[i] == key){
        nkv.removeRange(i, i+3);
        found = true;
        break;
      }
    }
    assert(nkv.length % recsize == 0);

    if(!found){
      if (missingOk) {
        return this;
      } else {
        _ThrowKeyError(key);
        // won't get here, just to make Dart Editor happy
        return null;
      }
    } else {
        return new _Leaf<K, V>.ensureOwner(this, owner, nkv, nkv.length ~/ recsize);
    }
  }

  V _get(K key, int hash, int depth) {
    int from = 0;
    int to = _kv.length - recsize;
    while(from - to > recsize * binSearchThr){
      int mid = (from + to) ~/ 2;
      if (_kv[mid + 2] > hash){
        to = mid;
      } else if (_kv[mid + 2] < hash){
        from = mid;
      } else {
        break;
      }
    }
    for(int i=from; i<=to; i+=recsize){
      if (_kv[i+2] == hash && _kv[i] == key) {
        return _kv[i+1];
      }
    }
    return _none;
  }

  void forEachKeyValue(f(K, V)) {
    for (int i=0; i<_kv.length; i+=recsize) {
      f(_kv[i], _kv[i+1]);
    }
  }

  toDebugString() => "_Leaf($_kv)";

  _forEachKVSegment(f){
    f(_kv);
  }

}

class _SubMapIterator<K, V> implements Iterator<Pair<K, V>> {
  List<_Node<K, V>> _array;
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


class _SubMap<K, V> extends _Node<K, V> {
  List<_Node<K, V>> _array;

  Iterator<Pair<K, V>> get iterator => new _SubMapIterator(_array);

  _SubMap.abc(_Owner owner, this._array, int length) : super(owner, length);

  factory _SubMap.ensureOwner(_SubMap old, _Owner owner, array, int length) {
    if(_ownerEquals(owner, old._owner)) {
      old._array = array;
      old._length = length;
    }
    return new _SubMap.abc(owner, array, length);
  }

  int get hashCode {
    if(_hash != null) return _hash;
    _hash = 0;
    for(var child in _array){
      _hash ^= child.hashCode;

    }
    return _hash;
  }

  V _get(K key, int hash, int depth) {
    int branch = _getBranch(hash, depth);
    _Node<K, V> map = _array[branch];
    return map._get(key, hash, depth - 1);
  }

  _Node<K, V> _insertOneWith(_Owner owner, key, val, hash, int depth, [update]) {
    int branch = _getBranch(hash, depth);
    _Node<K, V> m = _array[branch];
    int oldSize = m.length;
    _Node<K, V> newM = m._insertOneWith(owner, key, val, hash, depth - 1, update);
    if(identical(m, newM)) {
      if(oldSize != m.length) this._length += m.length - oldSize;
      return this;
    }
    List<_Node<K, V>> newarray = _makeCopyIfNeeded(owner, this._owner, _array);
    newarray[branch] = newM;
    int delta = newM.length - oldSize;
    return new _SubMap<K, V>.ensureOwner(this, owner, newarray, length + delta);
  }

  _Node<K, V> _delete(owner, K key, int hash, int depth, bool missingOk) {
    int branch = _getBranch(hash, depth);
    _Node<K, V> child = _array[branch];
    int childLength = child.length;
    // need to remember child length as this may modify
    // the child (if working with transient)
    _Node<K, V> newChild = child._delete(owner, key, hash, depth - 1, missingOk);
    int newLength = this.length + (newChild.length - childLength);
    if (identical(child, newChild)) {
      this._length = newLength;
      return this;
    }
    List<_Node<K, V>> newarray = new List<_Node<K, V>>.from(_array);
    newarray[branch] = newChild;
    _Node res = new _SubMap.ensureOwner(this, owner, newarray, newLength);

    // if submap is too small, let's replace it by _Leaf
    if (res._length >= leafSizeMin) {
      return res;
    } else {
      List _kv = [];
      res._forEachKVSegment((kv){
        _kv.addAll(kv);
      });
      var nres = new _Leaf.abc(owner, _kv);
//      nres.sanityCheck();
      return nres;
    }
  }

  _forEachKVSegment(f) {
    _array.forEach((child) => child._forEachKVSegment(f));
  }

  forEachKeyValue(f(K, V)) {
    _array.forEach((mi) => mi.forEachKeyValue(f));
  }

  toDebugString() => "_SubMap($_array)";
}

_ownerEquals(_Owner a, _Owner b) {
  return a != null && a == b;
}

/// usually, we need to copy some arrays when associng. However, when working
/// with transients (and owners match), it is safe just to modify the array
_makeCopyIfNeeded(_Owner a, _Owner b, List c) {
  if(_ownerEquals(a, b))
    return c;
  else return c.sublist(0);
}
