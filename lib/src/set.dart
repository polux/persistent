// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

part of persistent;

/**
 * A read-only set, unordered collection of distinct elements of type [E].
 *
 * There is no default implementation of [ReadVector], since it just
 * specifies the common interface of [PersistentVector] and [TransientVector].
 */
abstract class ReadSet<E> implements Iterable<E> {
  
  /**
   * Returns a new set of all the elements that are included
   * in either `this` or [other]
   */
  ReadSet<E> union(ReadSet<E> other);

  /// Alias for [union].
  ReadSet<E> operator +(ReadSet<E> other);

  /**
   * Returns a new set of all the elements that are included in `this` but
   * not in [other]
   */
  ReadSet<E> difference(ReadSet<E> other);

  /// Alias for [difference].
  ReadSet<E> operator -(ReadSet<E> other);

  /**
   * Returns a new set of all the pairs `Pair<first, second>` such that
   * `first` is included in `this` and
   * `second` is included in [other]
   */
  ReadSet<Pair<E,dynamic>> cartesianProduct(ReadSet other);

  /// Alias for [cartesianProduct].
  ReadSet<Pair<E,dynamic>> operator *(ReadSet other);

  /**
   * Returns a new set of all the elements that are included
   * in both `this` and [other]
   */
  ReadSet<E> intersection(ReadSet<E> other);

  /// Randomly picks an element of [this].
  E pickRandomElement([Random random]);

  /// A strict (non-lazy) version of [map].
  ReadSet strictMap(f(E element));

  /// A strict (non-lazy) version of [where].
  ReadSet<E> strictWhere(bool f(E element));
  
  /// Returns any [PersistentSet] with same content as `this`.
  PersistentSet<E> asPersistent();
  
  // TODO(syslo) Fix to satisfy the documentation
  /// Returns a new [TransientSet] with same content as `this`.
  TransientSet<E> asTransient();
}

/**
 * A persistent set, unordered collection of distinct elements of type [E].
 *
 * Persistent data structure is an immutable structure, that provides effective
 * creation of slightly mutated copies.
 */
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

  /**
   * Returns a set identical to `this` except that it contains [element].
   * 
   * If [element] is already in `this`, the same set is returned.
   */
  PersistentSet<E> insert(E element);

  /**
   * Returns a set identical to `this` except that it does not contain [element].
   * 
   * If `this` does not contain [element] and [safe]
   * is not specified, the error is thrown.
   * If `this` does not contain [element] and [safe]
   * is `true`, the same set is returned.
   */
  PersistentSet<E> delete(E element, {safe: false});
  
  /**
   * Creates transient copy of `this`, lets it to be modified by [change]
   * and returns persistent result.
   */
  PersistentSet<E> withTransient(void change(TransientSet<E> set));
  
  /**
   * The equality operator.
   *
   * Two persistent sets are equal if and only if for each element 
   * in any of them exists an equal element in the other one.
   */
  bool operator==(PersistentSet<E> other);
  
  /*
   * The documentation is inherited from the Object
   */
  int get hashCode;
}

/**
 * A transient set, unordered collection of distinct elements of type [E].
 *
 * Transient data structure is a mutable structure, that can be effectively
 * converted to the persistent data structure. It is ussualy created from
 * a persistent structure to apply some changes and obtain a new persistent
 * structure.
 */
abstract class TransientSet<E> implements ReadSet<E> {

  /**
   * Adds [element] to `this`.
   * 
   * If `this` contains [element], nothing happens.
   */
  void doInsert(E element);

  /**
   * Removes [element] from `this`.
   * 
   * If `this` does not contain [element] and [safe]
   * is not specified, the error is thrown.
   * If `this` does not contain [element] and [safe]
   * is `true`, nothing happens.
   */
  void doDelete(E element, {safe: false});
  
}

/**
 * A base class for implementations of [PersistentSet].
 */
abstract class ReadSetBase<E>
    extends IterableBase<E>
    implements ReadSet<E> {

  ReadSet<E> operator +(ReadSet<E> other) =>
      union(other);

  ReadSet<E> operator -(ReadSet<E> other) =>
      difference(other);

  ReadSet<Pair> operator *(ReadSet other) =>
      cartesianProduct(other);

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
