// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

part of persistent;

/**
 * A base class for implementations of [ReadSet].
 */
abstract class _ReadSetBase<E>
    extends IterableBase<E>
    implements ReadSet<E> {

  String toString() {
    StringBuffer buffer = new StringBuffer('{');
    bool comma = false;
    this.forEach((E e) {
      if (comma) buffer.write(', ');
      buffer.write(e.toString());
      comma = true;
    });
    buffer.write('}');
    return buffer.toString();
  }
}

abstract class _PSetMixim<E>
    implements PSet<E> {

  PSet<E> union(PSet<E> PSet) =>
      this.withTransient((set)=>
        PSet.where((e) => !this.contains(e)).forEach((e)=>
            set.doInsert(e)
        )
      );

  PSet<E> operator +(PSet<E> pset) =>
      union(pset);

  PSet<E> difference(PSet<E> pset) =>
      new PSet.from(this.where((e) => !pset.contains(e)));

  PSet<E> operator -(PSet<E> pset) =>
      difference(pset);

  Iterable<Pair> cartesianProduct(PSet<E> PSet) =>
      this.expand((a) => PSet.map((b) => new Pair(a,b)));

  Iterable<Pair> operator *(PSet PSet) =>
      cartesianProduct(PSet);

  PSet<E> intersect(PSet<E> pset) =>
      new PSet.from(this.where((e) => pset.contains(e)));

  PSet strictMap(f(E element)) =>
      new PSet.from(this.map(f));

  PSet<E> strictWhere(bool f(E element)) =>
      new PSet<E>.from(this.where(f));
}

abstract class _SetImplBase<E> extends _ReadSetBase<E> {
  ReadMap<E, Null> get _map;

  bool contains(E element) => _map.containsKey(element);

  bool hasKey(E key) => contains(key);

  E get(E element, [E notFound = _none]) =>
      contains(element) ?
        element
      :
        notFound == _none ?
          _ThrowKeyError(element)
        :
          notFound;

  void forEach(f(E element)) => _map.forEachKeyValue((E k, v) => f(k));

  Iterable map(f(E element)) {
    return _map.map((pair)=>f(pair.fst));
  }

  int get length => _map.length;

  bool operator ==(other) =>
      other is _SetImplBase ? _map == other._map : false;

  Iterator<E> get iterator =>
      _map.map((Pair<E, Object> pair) => pair.fst).iterator;

  E get last => _map.last.fst;

  E elementAt(int index) => _map.elementAt(index).fst;
}


class _PSetImpl<E>
    extends _SetImplBase<E>
    with _PSetMixim<E> {

  final PMap<E, Null> _map;

  _PSetImpl._internal(this._map);

  factory _PSetImpl() =>
      new _PSetImpl._internal(new PMap<E, Object>());

  _PSetImpl<E> insert(E element) =>
      new _PSetImpl._internal(_map.assoc(element, null));

  _PSetImpl<E> delete(E element, {bool missingOk:false}) =>
      new _PSetImpl._internal(_map.delete(element, missingOk:missingOk));

  TSet asTransient() {
    return new _TSetImpl._internal(_map.asTransient());
  }


  PSet<E> union(PSet<E> PSet){
    if(PSet is _PSetImpl<E>){
      return new _PSetImpl._internal(
          _map.union(PSet._map));
    } else {
      return super.union(PSet);
    }
  }

  PSet<E> intersection(PSet<E> PSet){
    if(PSet is _PSetImpl<E>){
      return new _PSetImpl._internal(
          _map.intersection(PSet._map));
    } else {
      return super.intersection(PSet);
    }
  }

  PSet withTransient(void change(TSet set)) {
    TSet result = this.asTransient();
    change(result);
    return result.asPersistent();
  }

  bool operator==(other) => other is PSet ? super == other : false;

  int get hashCode => this._map.hashCode;
}

class _TSetImpl<E> extends _SetImplBase<E> implements TSet<E> {
  final TMap<E, Null> _map;

  _TSetImpl._internal(this._map);

  factory _TSetImpl() =>
      new _TSetImpl._internal(new TMap<E, Object>());

  void doInsert(E element){
    _map.doAssoc(element, null);
  }

  void doDelete(E element, {bool missingOk:false}){
    _map.doDelete(element, missingOk:missingOk);
  }

  PSet asPersistent() {
    return new _PSetImpl._internal(_map.asPersistent());
  }

}


