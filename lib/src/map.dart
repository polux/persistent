// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

part of persistent;

/**
 * The interface is defined mainly for documenting read-functions of PersistentMap
 * and TransientMap; the interface is not supposed to be actually used
 * (as you usually know, which Map you are using).
 *
 * [ReadMap] binds keys of type [K] to values of type [V]. Null
 * values are supported but null keys are not.
 *
 * There is no default implementation of [ReadMap], since it just
 * specifies common interface of [PersistentMap] and [TransientMap].
 *
 * In all the examples below `{k1: v1, k2: v2, ...}` is a shorthand for
 * `new PersistentMap.fromMap({k1: v1, k2: v2, ...})`.
 */
abstract class ReadMap<K, V> implements Iterable<Pair<K, V>> {

  /**
   * Returns the value bound to [key].
   *
   * If [key] is not bound, [notFound] is returned; if [notFound] is not set, Exception
   * is thrown.
   */
  V get(K key, [V notFound]);

  /**
   * Returns the value bound to [key].
   *
   * Throws exception if [key] is not bound.
   */
  V operator [](K key);

  /**
   * Evaluates `f(key, value)` for each (`key`, `value`) pair in `this`.
   */
  void forEachKeyValue(f(K key, V value));

  /// Returns a mutable copy of `this`.
  Map<K, V> toMap();

  /// The keys of `this`.
  Iterable<K> get keys;

  /// Returns true if contains [key]
  bool containsKey(K key);

  /// Returns true if contains [key]
  bool hasKey(K key);

  /// The values of `this`.
  Iterable<V> get values;

  /// An iterator through the entries of `this`.
  Iterator<Pair<K, V>> get iterator;

  /// The number of entries of `this`.
  int get length;
}

/**
 * A persistent map, binding keys of type [K] to values of type [V]. Null
 * values are supported but null keys are not.
 *
 * Persistent data structure is an immutable structure, that provides effective
 * creation of slightly mutated copies.
 *
 * In all the examples below `{k1: v1, k2: v2, ...}` is a shorthand for
 * `new PersistentMap.fromMap({k1: v1, k2: v2, ...})`.
 */

abstract class PersistentMap<K, V> implements ReadMap<K, V>, PersistentIndexedCollection {

  /** Creates an empty [PersistentMap] using its default implementation. */
  factory PersistentMap() => new _Leaf.empty(null);

  /**
   * Creates an immutable copy of [map] using the default implementation of
   * [PersistentMap].
   */
  factory PersistentMap.fromMap(Map<K, V> map) {
    _Node root = new _Leaf.empty(null);
    map.forEach((K key, V value) {
      root = root._assoc(null, key, value);
    });
    return root;
  }

  /**
   * Creates a [PersistentMap] from an [Iterable] of [Pair]s using the default
   * implementation of [PersistentMap].
   */
  factory PersistentMap.fromPairs(Iterable<Pair<K, V>> pairs) {
      var _root = new _Leaf.empty(null);
      pairs.forEach((pair) {
        _root = _root._assoc(null, pair.first, pair.second);
      });
      return _root;
  }

  /**
   * The equality operator.
   *
   * Two persistent maps are equal if and only if their sets of keys are equal,
   * and the equal keys are bound to the equal values.
   *
   * Two sets of keys are equal if and only if for each key exists
   * an equal key in the other set.
   */
  bool operator== (other);

  /*
   * The documentation is inherited from the Object
   */
  int get hashCode;

  /**
   * Returns a new map identical to `this` except that it binds [key] to
   * [value].
   *
   * If [key] was bound to some `oldvalue` in `this`, it is nevertheless bound
   * to [value] in the new map.
   *
   *     {'a': 1}.assoc('b', 2) == {'a': 1, 'b': 2}
   *     {'a': 1, 'b': 2}.assoc('b', 3) == {'a': 3, 'b': 3}
   */
  PersistentMap<K, V>
      assoc(K key, V value);

  /**
   * Returns a new map identical to `this` except that it doesn't bind [key]
   * anymore.
   *
   * If [key] is not bound and [missingOk] is not `true`, an Exception is thrown.
   * If [key] is not bound and [missingOk] is specified as `true`,
   * the same map is returned. [missingOk] defaults to `false`.
   *
   *     {'a': 1, 'b': 2}.delete('b') == {'a': 1}
   *     {'a': 1}.delete('b') // throws an Exception
   */
  PersistentMap<K, V> delete(K key, {bool missingOk: false});

  /**
   * Returns a new map identical to `this` except that the value it possibly
   * binds to [key] has been adjusted by [f].
   *
   * [f] should have one of the following signatures: V f(V value), V f([V value])
   *
   * If [key] is not bound, [f] with no arguments will be called, and the result
   * will be associated with [key]. If [key] is not bound and [f] cannot take no arguments,
   * an Exception will be thrown.
   *
   *     {'a': 1, 'b': 2}.update('b', (x) => x + 1) == {'a': 1, 'b': 3}
   *     {'a': 1}.update('b', (x) => x + 1) // throws
   *     {'a': 2}.update('b', ([x]) => x == null ? 0 : x + 1) == {'a': 2, 'b': 0}
   */
//  PersistentMap<K, V> update(K key, dynamic f);

  /**
   * Returns a new map identical to `this` where each value has been updated by
   * [f].
   *
   *     {'a': 1, 'b': 2}.mapValues((x) => x + 1) == {'a': 2, 'b': 3}
   *     {}.mapValues((x) => x + 1) == {}
   */
//  ReadMap mapValues(f(V value));

  /**
   * Returns a transient copy of `this`.
   *
   * This is usually called to make many changes and
   * then create a new [PersistentMap].
   *
   *     var persistent1 = new PersistentMap.from({'a':1});
   *     var transient = persistent1.asTransient();
   *     transient.doAssoc({'b':2});
   *     var persistent2 = new transient.asPersistent();
   */
  TransientMap<K, V> asTransient();

  /**
   * Creates a transient copy of `this`, lets it to be modified by [change]
   * and returns a persistent result.
   *
   *     var persistent1 = new PersistentMap.from({'a':1});
   *     var persistent2 = persistent1.withTransient((m){
   *       m.doAssoc({'b':2});
   *     });
   */
  PersistentMap<K, V> withTransient(dynamic change(TransientMap));

  /**
   * Returns a new map whose (key, value) pairs are the union of those of `this`
   * and [other].
   *
   * The union is right-biased: if a key is present in both `this` and [other],
   * the value from [other] is retained. If [combine] is provided, the retained
   * value for a `key` present in both `this` and [other] is then
   * `combine(leftvalue, rightvalue)` where `leftvalue` is the value bound to
   * `key` in `this` and `rightvalue` is the one bound to `key` in [other].
   *
   *     {'a': 1}.union({'b': 2}) == {'a': 1, 'b': 2}
   *     {'a': 1}.union({'a': 3, 'b': 2}) == {'a': 3, 'b': 2}
   *     {'a': 1}.union({'a': 3, 'b': 2}, (x,y) => x + y) == {'a': 4, 'b': 2}
   *
   * Note that [union] is commutative if and only if [combine] is provided and
   * if it is commutative.
   */
//  PersistentMap<K, V>
//      union(PersistentMap<K, V> other, [V combine(V left, V right)]);

  /**
   * Returns a new map whose (key, value) pairs are the intersection of those of
   * `this` and [other].
   *
   * The intersection is right-biased: values from [other] are retained. If
   * [combine] is provided, the retained value for a `key` present in both
   * `this` and [other] is then `combine(leftvalue, rightvalue)` where
   * `leftvalue` is the value bound to `key` in `this` and `rightvalue` is the
   * one bound to `key` in [other].
   *
   *     {'a': 1}.intersection({'b': 2}) == {}
   *     {'a': 1}.intersection({'a': 3, 'b': 2}) == {'a': 3}
   *     {'a': 1}.intersection({'a': 3, 'b': 2}, (x,y) => x + y) == {'a': 4}
   *
   * Note that [intersection] is commutative if and only if [combine] is
   * provided and if it is commutative.
   */
//  PersistentMap<K, V>
//      intersection(PersistentMap<K, V> other, [V combine(V left, V right)]);

  /// A strict (non-lazy) version of [map].
  PersistentMap strictMap(Pair f(Pair<K, V> pair));

  /// A strict (non-lazy) version of [where].
  PersistentMap<K, V> strictWhere(bool f(Pair<K, V> pair));

  PersistentMap<K, V> update(K key, dynamic updateF);

}

/**
 * A transient map, binding keys of type [K] to values of type [V]. Null values
 * are supported but null keys are not.
 *
 * Transient data structure is a mutable structure, which can be efficiently
 * converted to the persistent data structure. It is usually created from
 * a persistent structure to apply some changes and obtain a new persistent
 * structure. The less changes are done, the more efficient is the conversion.
 */
abstract class TransientMap<K, V> implements ReadMap<K, V> {

  /**
   * Creates an empty map using the default implementation of
   * [TransientMap].
   */
  factory TransientMap() => new _TransientMapImpl();

  /**
   * Binds [key] to [value].
   *
   * If [key] was bound to some `oldvalue`, it is nevertheless bound
   * to [value].
   *
   *     var map = PersistentMap.fromMap({'a': 1}).asTransient();
   *     map.doAssoc('b', 2); // map is now {'a': 1, 'b': 2}
   *     map.doAssoc('b', 3); // map is now {'a': 1, 'b': 3}
   */
  TransientMap<K, V>
      doAssoc(K key, V value);

  /**
   * Unbinds [key].
   *
   * If [key] is not bound and [missingOk] is `false`, exception is thrown.
   * If [key] is not bound and [missingOk] is specified as `true`, nothing happens.
   * [missingOk] defaults to `false`.
   *
   *     var map = PersistentMap.fromMap({'a': 1, 'b': 2}).asTransient();
   *     map.doDelete('b', 2); // map is now {'a': 1}
   *     map.doDelete('b', 2, missingOk: true); // map is still {'a': 1}
   */
  TransientMap<K, V> doDelete(K key, {bool missingOk: false}) ;

  // TODO: check how this works
  /**
   * Adjusts the value that is possibly bound to [key] by applying [f].
   *
   * [f] should have one of the following signatures: V f(V value), V f([V value])
   *
   * If [key] is not bound, result of calling [f] with no arguments is associated with [key]
   * If [key] is not bound and [f] can not be called with no arguments, [Exception] is thrown.
   *
   *     var map = PersistentMap.fromMap({'a': 1}).asTransient();
   *     map.doUpdate('b', (x) => x + 1); // throws
   *     map.doUpdate('b', ([x]) => x == null ? 2 : x + 1); // map is now {'a': 1, 'b': 2}
   *     map.doUpdate('b', (x) => x + 1); // map is now {'a': 1, 'b': 3}
   */
//  TransientMap<K, V> doUpdate(K key, dynamic f);

  /**
   * Updates all values by passing them to [f] and replacing them by results.
   *
   *     var map = PersistentMap.fromMap({'a': 1, 'b': 2}).asTransient();
   *     map.mapValues((x) => x + 1) // map is now {'a': 2, 'b': 3}
   */
//  TransientMap doMapValues(f(V value));

  /**
   * Returns a persistent copy of `this`.
   *
   * This is ussualy called when changes to `this`
   * are finished
   *
   *     var persistent1 = new PersistentMap.from({'a':1});
   *     var transient = persistent1.asTransient();
   *     transient.doAssoc({'b':2});
   *     var persistent2 = new transient.asPersistent();
   */
  PersistentMap asPersistent();

  operator []=(key, value);
}
