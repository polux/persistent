// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors:
//   Paul Brauner (polux@google.com)
//   Rafael Brandão (rafa.bra@gmail.com)

part of persistent;

/**
 * An immutable map, binding keys of type [K] to values of type [V]. Null values
 * are supported but null keys are not.
 *
 * In all the examples below `{k1: v1, k2: v2, ...}` is a shorthand for
 * `PersistentMap.fromMap({k1: v1, k2: v2, ...})`.
 */
abstract class PersistentMap<K, V>
        implements Iterable<Pair<K, V>> {

  /** Creates an empty [PersistentMap] using its default implementation. */
  factory PersistentMap() => new PersistentMapImpl();

  /**
   * Creates an immutable copy of [map] using the default implementation of
   * [PersistentMap].
   */
  factory PersistentMap.fromMap(Map<K, V> map) =>
      new PersistentMapImpl.fromMap(map);

  /**
   * Creates a [PersistentMap] from an [Iterable] of [Pair]s using the default
   * implementation of [PersistentMap].
   */
  factory PersistentMap.fromPairs(Iterable<Pair<K, V>> pairs) =>
      new PersistentMapImpl.fromPairs(pairs);

  /**
   * Returns a new map identical to `this` except that it binds [key] to
   * [value].
   *
   * If [key] was bound to some `oldvalue` in `this`, it is nevertheless bound
   * to [value] in the new map. If [key] was bound to some `oldvalue` in `this`
   * and if [combine] is provided then [key] it is bound to
   * `combine(oldvalue, value)` in the new map.
   *
   *     {'a': 1}.insert('b', 2) == {'a': 1, 'b', 2}
   *     {'a': 1, 'b': 2}.insert('b', 3) == {'a': 3, 'b', 3}
   *     {'a': 1, 'b': 2}.insert('b', 3, (x,y) => x - y) == {'a': 3, 'b', -1}
   */
  PersistentMap<K, V>
      insert(K key, V value, [V combine(V oldvalue, V newvalue)]);
  PersistentMap<K, V> insertIn(List path, V value, [V combine(V oldvalue, V newvalue)]);

  /**
   * Returns a new map identical to `this` except that it doesn't bind [key]
   * anymore.
   *
   *     {'a': 1, 'b': 2}.delete('b') == {'a': 1}
   *     {'a': 1}.delete('b') == {'a': 1}
   */
  PersistentMap<K, V> delete(K key);
  PersistentMap<K, V> deleteIn(List path);

  /**
   * Looks up the value possibly bound to [key] in `this`. Returns
   * `Option.some(value)` if it exists, `Option.none()` otherwise.
   *
   *     {'a': 1}.lookup('b') == Option.none()
   *     {'a': 1, 'b': 2}.lookup('b') == Option.some(2)
   */
  Option<V> lookup(K key);
  Option<V> lookupIn(List path);

  /**
   * Returns the value for the given [key] or [:null:] if [key]
   * is not in the map.
   */
  V operator [](K key);

  /**
   * Evaluates `f(key, value)` for each (`key`, `value`) pair in `this`.
   */
  void forEachKeyValue(f(K key, V value));

  /**
   * Returns a new map identical to `this` except that the value it possibly
   * binds to [key] has been adjusted by [update].
   *
   *     {'a': 1, 'b': 2}.adjust('b', (x) => x + 1) == {'a', 1, 'b', 3}
   *     {'a': 1}.adjust('b', (x) => x + 1) == {'a', 1}
   */
  PersistentMap<K, V> adjust(K key, V update(V value));
  PersistentMap<K, V> adjustIn(List path, V update(V value));

  /**
   * Returns a new map identical to `this` where each value has been updated by
   * [f].
   *
   *     {'a': 1, 'b': 2}.mapValues((x) => x + 1) == {'a', 2, 'b', 3}
   *     {}.mapValues((x) => x + 1) == {}
   */
  PersistentMap mapValues(f(V value));

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
  PersistentMap<K, V>
      union(PersistentMap<K, V> other, [V combine(V left, V right)]);

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
  PersistentMap<K, V>
      intersection(PersistentMap<K, V> other, [V combine(V left, V right)]);

  /// Returns a mutable copy of `this`.
  Map<K, V> toMap();

  /// The keys of `this`.
  Iterable<K> get keys;

  /// The values of `this`.
  Iterable<V> get values;

  /// Randomly picks an entry of `this`.
  Pair<K, V> pickRandomEntry([Random random]);

  /// A strict (non-lazy) version of [map].
  PersistentMap strictMap(Pair f(Pair<K, V> pair)) =>
      new PersistentMap.fromPairs(this.map(f));

  /// A strict (non-lazy) version of [where].
  PersistentMap<K, V> strictWhere(bool f(Pair<K, V> pair)) =>
      new PersistentMap<K, V>.fromPairs(this.where(f));

  Iterator get iterator;

  int get length;

  TransientMap asTransient();

  PersistentMap withTransient(dynamic f(TransientMap));
}

abstract class TransientMap<K, V>
        implements Iterable<Pair<K, V>> {

  /**
   * Creates an mutable copy of [map] using the default implementation of
   * [TransientMap].
   */
  factory TransientMap() => new TransientMapImpl();

  TransientMap<K, V>
      doInsert(K key, V value, [V combine(V oldvalue, V newvalue)]);

  TransientMap<K, V>
           doInsertIn(List<K> path, V value, [V combine(V oldvalue, V newvalue)]);

  TransientMap<K, V> doDelete(K key) ;

  TransientMap<K, V> doDeleteIn(List<K> path);

  Option<V> doLookup(K key);

  Option doLookupIn(List<K> path);

  V operator [](K key);

  void doForEachKeyValue(f(K key, V value));

  TransientMap<K, V> doAdjust(K key, V update(V value));

  TransientMap<K, V> doAdjustIn(List key, V update(V value));


  TransientMap doMapValues(f(V value)) ;

  TransientMap<K, V>
      doUnion(TransientMap<K, V> other, [V combine(V left, V right)]);

  TransientMap<K, V>
    doIntersection(TransientMap<K, V> other, [V combine(V left, V right)]);

  /// Returns a mutable copy of `this`.
  Map<K, V> toMap();

  /// The keys of `this`.

  Iterable<K> get keys;
  /// The values of `this`.

  Iterable<V> get values;
  /// Randomly picks an entry of `this`.
  Pair<K, V> doPickRandomEntry([Random random]);
  Iterator get iterator;

  PersistentMap asPersistent() => null;
}
