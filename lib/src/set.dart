// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

part of persistent;

abstract class ReadSet<E> implements Iterable<E> {
  
  ReadSet<E> union(ReadSet<E> persistentSet);

  /// Alias for [union].
  ReadSet<E> operator +(ReadSet<E> persistentSet);

  ReadSet<E> difference(ReadSet<E> persistentSet);

  /// Alias for [difference].
  ReadSet<E> operator -(ReadSet<E> persistentSet);

  ReadSet<Pair<E,dynamic>> cartesianProduct(ReadSet persistentSet);

  /// Alias for [cartesianProduct].
  ReadSet<Pair<E,dynamic>> operator *(ReadSet persistentSet);

  ReadSet<E> intersection(ReadSet<E> persistentSet);

  /// Randomly picks an element of [this].
  E pickRandomElement([Random random]);

  /// A strict (non-lazy) version of [map].
  ReadSet strictMap(f(E element));

  /// A strict (non-lazy) version of [where].
  ReadSet<E> strictWhere(bool f(E element));
  
  PersistentSet<E> asPersistent();
  
  TransientSet<E> asTransient();
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
  
  bool operator==(PersistentSet<E> other);
  
  int get hashCode;
}

abstract class TransientSet<E> implements ReadSet<E> {

  void doInsert(E element);

  void doDelete(E element, {bool safe:false});
  
}

/**
 * A base class for implementations of [PersistentSet].
 */
abstract class ReadSetBase<E>
    extends IterableBase<E>
    implements ReadSet<E> {

  ReadSet<E> operator +(ReadSet<E> persistentSet) =>
      union(persistentSet);

  ReadSet<E> operator -(ReadSet<E> persistentSet) =>
      difference(persistentSet);

  ReadSet<Pair> operator *(ReadSet persistentSet) =>
      cartesianProduct(persistentSet);

  ReadSet strictMap(f(E element)) =>
      new PersistentSet.from(this.map(f));

  ReadSet<E> strictWhere(bool f(E element)) =>
      new PersistentSet<E>.from(this.where(f));

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
