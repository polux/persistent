// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

part of persistent;

/**
 * A read-only set, unordered collection of distinct elements of type [E].
 *
 * There is no default implementation of [ReadVector], since it just
 * specifies the common interface of [PersistentVector] and [TransientVector].
 */
abstract class ReadSet<E> implements Iterable<E> {

  bool hasKey(E key);

  /**
   * If it contains given [element], it is returned. Otherwise returns [notFound]
   */
  E get(E element, [E notFound]);
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
   * If `this` does not contain [element], `this` is returned or Exception is thrown
   * dependent on what is value of [allowMissing] flag.
   */
  PersistentSet<E> delete(E element, {bool allowMissing: false});

  /**
   * Creates transient copy of `this`, lets it to be modified by [change]
   * and returns persistent result.
   */
  PersistentSet<E> withTransient(void change(TransientSet<E> set));

  /**
   * Returns a new set of all the elements that are included
   * in either `this` or [other]
   */
  PersistentSet<E> union(PersistentSet<E> other);

  /// Alias for [union].
  PersistentSet<E> operator +(PersistentSet<E> other);

  /**
   * Returns a new set of all the elements that are included in `this` but
   * not in [other]
   */
  PersistentSet<E> difference(PersistentSet<E> other);

  /// Alias for [difference].
  PersistentSet<E> operator -(PersistentSet<E> other);

  /**
   * Returns a lazy iterable with all the pairs `Pair(first, second)`
   * such that `first` is included in `this` and
   * `second` is included in [other]
   */
  Iterable<Pair<E,dynamic>> cartesianProduct(PersistentSet other);

  /// Alias for [cartesianProduct].
  Iterable<Pair<E,dynamic>> operator *(PersistentSet other);

  /**
   * Returns a new set of all the elements that are included
   * in both `this` and [other]
   */
  PersistentSet<E> intersection(PersistentSet<E> persistentSet);

  /// A strict (non-lazy) version of [map].
  PersistentSet strictMap(f(E element));

  /// A strict (non-lazy) version of [where].
  PersistentSet<E> strictWhere(bool f(E element));

  /**
   * The equality operator.
   *
   * Two persistent sets are equal if and only if for each element
   * in any of them exists an equal element in the other one.
   */
  bool operator==(other);

  /*
   * The documentation is inherited from the Object
   */
  int get hashCode;

  TransientSet<E> asTransient();
}

/**
 * A transient set, unordered collection of distinct elements of type [E].
 *
 * Transient data structure is a mutable structure, that can be effectively
 * converted to the persistent data structure. It is usually created from
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
   * If `this` does not contain [element] and [allowMissing]
   * is not specified or false, the error is thrown.
   * If `this` does not contain [element] and [allowMissing]
   * is `true`, nothing happens.
   */
  void doDelete(E element, {bool allowMissing: false});

  PersistentSet<E> asPersistent();
}
