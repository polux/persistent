// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

part of persistent;

abstract class PersistentSet<E> implements Iterable<E> {

  /** Creates an empty [PersistentSet] using its default implementation. */
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

  PersistentSet<E> delete(E element);

  PersistentSet<E> union(PersistentSet<E> persistentSet);

  /** Alias for [union] */
  PersistentSet<E> operator +(PersistentSet<E> persistentSet);

  PersistentSet<E> difference(PersistentSet<E> persistentSet);

  /** Alias for [difference] */
  PersistentSet<E> operator -(PersistentSet<E> persistentSet);

  PersistentSet<Pair> cartesianProduct(PersistentSet persistentSet);

  /** Alias for [cartesianProduct] */
  PersistentSet<Pair> operator *(PersistentSet persistentSet);

  PersistentSet<E> intersection(PersistentSet<E> persistentSet);
}

/**
 * A base class for implementations of [PersistentSet].
 */
abstract class PersistentSetBase<E>
    extends Iterable<E>
    implements PersistentSet<E> {

  PersistentSet<E> operator +(PersistentSet<E> persistentSet) =>
      union(persistentSet);

  PersistentSet<E> operator -(PersistentSet<E> persistentSet) =>
      difference(persistentSet);

  PersistentSet<Pair> operator *(PersistentSet persistentSet) =>
      cartesianProduct(persistentSet);

  String toString() {
    StringBuffer buffer = new StringBuffer('{');
    bool comma = false;
    this.forEach((E e) {
      if (comma) buffer.add(', ');
      buffer.add(e.toString());
      comma = true;
    });
    buffer.add('}');
    return buffer.toString();
  }
}
