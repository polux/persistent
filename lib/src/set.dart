// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

part of persistent;

abstract class ReadSet<E> implements Iterable<E> {
  
}

abstract class PersistentSet<E> implements ReadSet<E> {

  /// Creates an empty [PersistentSet] using its default implementation.
  factory PersistentSet() => new _PersistentSetImpl<E>();

  /**
   * Creates an immutable copy of [elements] using the default implementation
   * of [PersistentSet].
   */
  factory PersistentSet.from(Iterable<E> elements) {
    PersistentSet<E> result = new _PersistentSetImpl<E>();
    for (E element in elements) {
      result = result.insert(element);
    }
    return result;
  }

  PersistentSet<E> insert(E element);

  PersistentSet<E> delete(E element, {bool safe:false});
  
  PersistentSet<E> withTransient(void change(TransientSet<E> set));
  
  PersistentSet<E> union(PersistentSet<E> persistentSet);

  /// Alias for [union].
  PersistentSet<E> operator +(PersistentSet<E> persistentSet);

  PersistentSet<E> difference(PersistentSet<E> persistentSet);

  /// Alias for [difference].
  PersistentSet<E> operator -(PersistentSet<E> persistentSet);

  Iterable<Pair<E,dynamic>> cartesianProduct(PersistentSet persistentSet);

  /// Alias for [cartesianProduct].
  Iterable<Pair<E,dynamic>> operator *(PersistentSet persistentSet);

  PersistentSet<E> intersection(PersistentSet<E> persistentSet);

  /// Randomly picks an element of [this].
  E pickRandomElement([Random random]);

  /// A strict (non-lazy) version of [map].
  PersistentSet strictMap(f(E element));

  /// A strict (non-lazy) version of [where].
  PersistentSet<E> strictWhere(bool f(E element));
  
  bool operator==(PersistentSet<E> other);
  
  int get hashCode;
  
  TransientSet<E> asTransient();
}

abstract class TransientSet<E> implements ReadSet<E> {

  void doInsert(E element);

  void doDelete(E element, {bool safe:false});
  
  PersistentSet<E> asPersistent();
}

/**
 * A base class for implementations of [ReadSet].
 */
abstract class ReadSetBase<E>
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
    
abstract class PersistentSetMixim<E>
    implements PersistentSet<E> {
      
  PersistentSet<E> operator +(PersistentSet<E> persistentSet) =>
      union(persistentSet);

  PersistentSet<E> operator -(PersistentSet<E> persistentSet) =>
      difference(persistentSet);

  Iterable<Pair> operator *(PersistentSet persistentSet) =>
      cartesianProduct(persistentSet);

  PersistentSet strictMap(f(E element)) =>
      new PersistentSet.from(this.map(f));

  PersistentSet<E> strictWhere(bool f(E element)) =>
      new PersistentSet<E>.from(this.where(f));
}
