// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

part of persistent;

abstract class _SetImplBase<E> extends ReadSetBase<E> {
  ReadMap<E, Null> get _map;

  bool contains(E element) => !isNone(_map.lookup(element, orElse: getNone));

  void forEach(f(E element)) => _map.forEachKeyValue((E k, v) => f(k));

  _SetImplBase<E> map(f(E element)) {
    _PersistentSetImpl<E> result = new _PersistentSetImpl<E>();
    _map.forEachKeyValue((E k, Null v) { result = result.insert(f(k)); });
    return result;
  }

  _SetImplBase<E> filter(bool f(E element)) {
    _PersistentSetImpl<E> result = new _PersistentSetImpl<E>();
    _map.forEachKeyValue((E k, v) {
      if (f(k)) {
        result = result.insert(k);
      }
    });
    return result;
  }

  int get length => _map.length;

  _SetImplBase<E> union(_SetImplBase<E> persistentSet) =>
      new _PersistentSetImpl._internal(_map.union(persistentSet._map));

  _SetImplBase<E> difference(_SetImplBase<E> persistentSet) {
    _PersistentSetImpl<E> result = new _PersistentSetImpl<E>();
    _map.forEachKeyValue((E k, v) {
      if (!persistentSet.contains(k)) {
        result = result.insert(k);
      }
    });
    return result;
  }

  _SetImplBase<E> intersection(_SetImplBase<E> persistentSet) =>
      new _PersistentSetImpl<E>._internal(_map.intersection(persistentSet._map));

  PersistentSet<Pair> cartesianProduct(_SetImplBase<E> persistentSet) {
    _PersistentSetImpl<Pair<E,dynamic>> result =
        new _PersistentSetImpl<Pair<E,dynamic>>();
    _map.forEachKeyValue((E e1, _) {
      persistentSet._map.forEachKeyValue((e2, _) {
        result = result.insert(new Pair<E,dynamic>(e1, e2));
      });
    });
    return result;
  }

  bool operator ==(_SetImplBase<E> other) => _map == other._map;

  Iterator<E> get iterator =>
      _map.map((Pair<E, Object> pair) => pair.fst).iterator;

  E pickRandomElement([Random random]) => _map.pickRandomEntry(random).fst;

  // PersistentMap's "last" is optimized
  E get last => _map.last.fst;

  // PersistentMap's "elementAt" is optimized
  E elementAt(int index) => _map.elementAt(index).fst;
}


class _PersistentSetImpl<E> extends _SetImplBase<E> implements PersistentSet {
  final PersistentMap<E, Null> _map;

  _PersistentSetImpl._internal(this._map);

  factory _PersistentSetImpl() =>
      new _PersistentSetImpl._internal(new PersistentMap<E, Object>());

  _PersistentSetImpl<E> insert(E element) =>
      new _PersistentSetImpl._internal(_map.insert(element, null));

  _PersistentSetImpl<E> delete(E element, {bool safe:false}) =>
      new _PersistentSetImpl._internal(_map.delete(element, safe:safe));
  
  TransientSet asTransient() {
    return new _TransientSetImpl._internal(_map.asTransient());
  }

  PersistentSet withTransient(void change(TransientSet set)) {
    TransientSet result = this.asTransient();
    change(result);
    return result.asPersistent();
  }
  
  bool operator==(ReadSet<E> other) =>
      other is TransientSet ?
        this.length == other.length &&
        this.fold(true, (test, v)=> test && other.contains(v))
      :
        super == other;
    
  int get hashCode => this._map.hashCode;
}

class _TransientSetImpl<E> extends _SetImplBase<E> implements TransientSet {
  final TransientMap<E, Null> _map;

  _TransientSetImpl._internal(this._map);

  factory _TransientSetImpl() =>
      new _TransientSetImpl._internal(new TransientMap<E, Object>());

  void doInsert(E element){
    _map.doInsert(element, null);
  }

  void doDelete(E element, {bool safe:false}){
    _map.doDelete(element, safe:safe);
  }

  PersistentSet asPersistent() {
    return new _PersistentSetImpl._internal(_map.asPersistent());
  }
  
}


