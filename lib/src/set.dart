// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

part of persistent;

/**
 * A read-only set, unordered collection of distinct elements of type [E].
 *
 * There is no default implementation of [ReadVector], since it just
 * specifies the common interface of [PVec] and [TVec].
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
abstract class PSet<E> implements ReadSet<E>, PersistentCollection {

  /// Creates an empty [PSet] using its default implementation.
  factory PSet() => new _PSetImpl<E>();

  /**
   * Creates an immutable copy of [elements] using the default implementation
   * of [PSet].
   */
  factory PSet.from(Iterable<E> elements) {
    PSet<E> result = new _PSetImpl<E>();
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
  PSet<E> insert(E element);

  /**
   * Returns a set identical to `this` except that it does not contain [element].
   *
   * If `this` does not contain [element], `this` is returned or Exception is thrown
   * dependent on what is value of [missingOk] flag.
   */
  PSet<E> delete(E element, {bool missingOk: false});

  /**
   * Creates transient copy of `this`, lets it to be modified by [change]
   * and returns persistent result.
   */
  PSet<E> withTransient(void change(TSet<E> set));

  /**
   * Returns a new set of all the elements that are included
   * in either `this` or [other]
   */
  PSet<E> union(PSet<E> other);

  /// Alias for [union].
  PSet<E> operator +(PSet<E> other);

  /**
   * Returns a new set of all the elements that are included in `this` but
   * not in [other]
   */
  PSet<E> difference(PSet<E> other);

  /// Alias for [difference].
  PSet<E> operator -(PSet<E> other);

  /**
   * Returns a lazy iterable with all the pairs `Pair(first, second)`
   * such that `first` is included in `this` and
   * `second` is included in [other]
   */
  Iterable<Pair<E,dynamic>> cartesianProduct(PSet other);

  /// Alias for [cartesianProduct].
  Iterable<Pair<E,dynamic>> operator *(PSet other);

  /**
   * Returns a new set of all the elements that are included
   * in both `this` and [other]
   */
  PSet<E> intersection(PSet<E> PSet);

  /// A strict (non-lazy) version of [map].
  PSet strictMap(f(E element));

  /// A strict (non-lazy) version of [where].
  PSet<E> strictWhere(bool f(E element));

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

  TSet<E> asTransient();
}

/**
 * A transient set, unordered collection of distinct elements of type [E].
 *
 * Transient data structure is a mutable structure, that can be effectively
 * converted to the persistent data structure. It is usually created from
 * a persistent structure to apply some changes and obtain a new persistent
 * structure.
 */
abstract class TSet<E> implements ReadSet<E> {

  /**
   * Adds [element] to `this`.
   *
   * If `this` contains [element], nothing happens.
   */
  void doInsert(E element);

  /**
   * Removes [element] from `this`.
   *
   * If `this` does not contain [element] and [missingOk]
   * is not specified or false, the error is thrown.
   * If `this` does not contain [element] and [missingOk]
   * is `true`, nothing happens.
   */
  void doDelete(E element, {bool missingOk: false});

  PSet<E> asPersistent();
}
