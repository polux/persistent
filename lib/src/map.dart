// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors:
//   Paul Brauner (polux@google.com)
//   Rafael Brandão (rafa.bra@gmail.com)

part of persistent;

/**
 * An persistent map, binding keys of type [K] to values of type [V]. Null
 * values are supported but null keys are not.
 *
 * Persistent data structure is an immutable structure, that provides effective
 * creation of slightly mutated copies.
 *
 * In all the examples below `{k1: v1, k2: v2, ...}` is a shorthand for
 * `new PersistentMap.fromMap({k1: v1, k2: v2, ...})`.
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
   * The equality operator.
   *
   * Two persistent maps are equal if and only if their sets of keys are equal,
   * and the equal keys are bound to the equal values.
   *
   * Two sets of keys are equal if and only if for each key exists
   * an equal key in the other set.
   */
  bool operator== (PersistentMap other);

  /*
   * The documentation is inherited from the Object
   */
  int get hashCode;

  /**
   * Returns a new map identical to `this` except that it binds [key] to
   * [value].
   *
   * If [key] was bound to some `oldvalue` in `this`, it is nevertheless bound
   * to [value] in the new map. If [key] was bound to some `oldvalue` in `this`
   * and if [combine] is provided then [key] it bound to
   * `combine(oldvalue, value)` in the new map.
   *
   *     {'a': 1}.insert('b', 2) == {'a': 1, 'b': 2}
   *     {'a': 1, 'b': 2}.insert('b', 3) == {'a': 3, 'b': 3}
   *     {'a': 1, 'b': 2}.insert('b', 3, (x,y) => x - y) == {'a': 3, 'b': -1}
   */
  PersistentMap<K, V>
      insert(K key, V value, [V combine(V oldvalue, V newvalue)]);

  /**
   * Calls [insert] recursively using [path] elemenets as keys.
   */
  PersistentMap<K, V> insertIn(List path, V value, [V combine(V oldvalue, V newvalue)]);

  /**
   * Returns a new map identical to `this` except that it doesn't bind [key]
   * anymore.
   *
   *     {'a': 1, 'b': 2}.delete('b') == {'a': 1}
   *     {'a': 1}.delete('b') == {'a': 1}
   */
  PersistentMap<K, V> delete(K key, {bool safe: false});

  /**
   * Calls [delete] recursively using [path] elemenets as keys.
   */
  PersistentMap<K, V> deleteIn(List path, {bool safe: false});

  /**
   * Looks up the value possibly bound to [key] in `this`. Returns
   * [new Option.some(value)] if it exists, [new Option.none()] otherwise.
   *
   *     {'a': 1}.lookup('b') == Option.none()
   *     {'a': 1, 'b': 2}.lookup('b') == Option.some(2)
   */
  V lookup(K key, [dynamic orElse()]);

  /**
   * Calls [lookup] recursively using [path] elemenets as keys.
   */
  lookupIn(List path, [dynamic orElse()]);

  /**
   * Returns the value for the given [key] or throws if [key]
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
   *     {'a': 1, 'b': 2}.adjust('b', (x) => x + 1) == {'a': 1, 'b': 3}
   *     {'a': 1}.adjust('b', (x) => x + 1) == {'a': 1}
   */
  PersistentMap<K, V> adjust(K key, V update(V value));

  /**
   * Calls [adjust] recursively using [path] elemenets as keys.
   */
  PersistentMap<K, V> adjustIn(List path, V update(V value));

  /**
   * Returns a new map identical to `this` where each value has been updated by
   * [f].
   *
   *     {'a': 1, 'b': 2}.mapValues((x) => x + 1) == {'a': 2, 'b': 3}
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

  /// An iterator through the entries of `this`.
  Iterator<Pair<K, V>> get iterator;

  /// The number of entries of `this`.
  int get length;

  /**
   * Returns a transient copy of `this`.
   *
   * This is ussualy called to do some changes and
   * then create a new [PersistentMap].
   *
   *     var persistent1 = new PersistentMap.from({'a':1});
   *     var transient = persistent1.asTransient();
   *     transient.doInsert({'b':2});
   *     var persistent2 = new transient.asPersistent();
   */
  TransientMap<K, V> asTransient();

  /**
   * Creates transient copy of `this`, lets it to be modified by [change]
   * and returns persistent result.
   *
   *     var persistent1 = new PersistentMap.from({'a':1});
   *     var persistent2 = persistent1.withTransient((m){
   *       m.doInsert({'b':2});
   *     });
   */
  PersistentMap<K, V> withTransient(dynamic change(TransientMap));
}

/**
 * A transient map, binding keys of type [K] to values of type [V]. Null values
 * are supported but null keys are not.
 *
 * Transient data structure is a mutable structure, that can be effectively
 * converted to the persistent data structure. It is ussualy created from
 * a persistent structure to apply some changes and obtain a new persistent
 * structure. The less changes are done, the more efficient the conversion
 * is.
 */
abstract class TransientMap<K, V>
        implements Iterable<Pair<K, V>> {

  /**
   * Creates an empty map using the default implementation of
   * [TransientMap].
   */
  factory TransientMap() => new TransientMapImpl();

  /**
   * Binds [key] to [value].
   *
   * If [key] was bound to some `oldvalue`, it is nevertheless bound
   * to [value]. If [key] was bound to some `oldvalue`
   * and if [combine] is provided then [key] is bound to
   * `combine(oldvalue, value)`.
   *
   *     var map = PersistentMap.fromMap({'a': 1}).asTransient();
   *     map.doInsert('b', 2); // map is now {'a': 1, 'b': 2}
   *     map.doInsert('b', 3); // map is now {'a': 1, 'b': 3}
   *     map.doInsert('b', 2, (x,y) => x - y); // map is now {'a': 3, 'b': 1}
   */
  TransientMap<K, V>
      doInsert(K key, V value, [V combine(V oldvalue, V newvalue)]);

  /**
   * Calls [doInsert] recursively using [path] elemenets as keys.
   */
  TransientMap<K, V>
      doInsertIn(List<K> path, V value, [V combine(V oldvalue, V newvalue)]);

  /**
   * Unbinds [key].
   *
   * If [key] isn't bound this function has no effect.
   *
   *     var map = PersistentMap.fromMap({'a': 1, 'b': 2}).asTransient();
   *     map.doDelete('b', 2); // map is now {'a': 1}
   *     map.doDelete('b', 2); // map is still {'a': 1}
   */
  TransientMap<K, V> doDelete(K key, {bool safe: false}) ;

  /**
   * Calls [doDelete] recursively using [path] elemenets as keys.
   */
  TransientMap<K, V> doDeleteIn(List<K> path, {bool safe: false});

  /**
   * Looks up the value possibly bound to [key] in `this`. Returns
   * [new Option.some(value)] if it exists, [new Option.none()] otherwise.
   *
   *     var map = PersistentMap.fromMap({'a': 1, 'b': 2}).asTransient();
   *     map.doLookup('b') == Option.none();
   *     map.doInsert('b', 2);
   *     map.doLookup('b') == Option.some(2);
   */
  V doLookup(K key, [dynamic orElse()]);

  /**
   * Calls [doLookup] recursively using [path] elemenets as keys.
   */
  doLookupIn(List<K> path, [dynamic orElse()]);

  /**
   * Returns the value for the given [key] or [:null:] if [key]
   * is not in the map.
   *
   *     var map = PersistentMap.fromMap({'a': 1, 'b': 2}).asTransient();
   *     map['b'] == null;
   *     map.doInsert('b', 2);
   *     map['b'] == 2;
   */
  V operator [](K key);

  /**
   * Evaluates `f(key, value)` for each (`key`, `value`) pair in `this`.
   */
  void doForEachKeyValue(f(K key, V value));

  /**
   * Adjusts the value that is possibly bound to [key] by [update].
   *
   *     var map = PersistentMap.fromMap({'a': 1}).asTransient();
   *     map.doAdjust('b', (x) => x + 1); // map is still {'a': 1}
   *     map.doUpdate('b', 2);
   *     map.doAdjust('b', (x) => x + 1); // map is now {'a': 1, 'b': 2}
   */
  TransientMap<K, V> doAdjust(K key, V update(V value));

  /**
   * Calls [doAdjust] recursively using [path] elemenets as keys.
   */
  TransientMap<K, V> doAdjustIn(List path, V update(V value));

  /**
   * Updates all values by passing them to [f] and replacing them by results.
   *
   *     var map = PersistentMap.fromMap({'a': 1, 'b': 2}).asTransient();
   *     map.mapValues((x) => x + 1) // map is now {'a': 2, 'b': 3}
   */
  TransientMap doMapValues(f(V value)) ;

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
   *     var mapA = PersistentMap.fromMap({'a': 1}).asTransient();
   *     var mapB = PersistentMap.fromMap({'b': 2}).asTransient();
   *     var mapAB = PersistentMap.fromMap({'a': 3, 'b': 2}).asTransient();
   *     mapA.doUnion(mapB) // returns {'a': 1, 'b': 2}
   *     mapA.doUnion(mapAB) // returns {'a': 3, 'b': 2}
   *     mapA.doUnion(mapAB, (x,y) => x + y) // returns {'a': 4, 'b': 2}
   */
  TransientMap<K, V>
      doUnion(TransientMap<K, V> other, [V combine(V left, V right)]);

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
   *     var mapA = PersistentMap.fromMap({'a': 1}).asTransient();
   *     var mapB = PersistentMap.fromMap({'b': 2}).asTransient();
   *     var mapAB = PersistentMap.fromMap({'a': 3, 'b': 2}).asTransient();
   *     mapA.doIntersection(mapB) // returns {}
   *     mapA.doIntersection(mapAB) // returns {'a': 1}
   *     mapA.doIntersection(mapAB, (x,y) => x + y) // returns {'a': 4}
   */
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

  /// An iterator through the entries of `this`.
  Iterator<Pair<K,V>> get iterator;

  /**
   * Returns a persistent copy of `this`.
   *
   * This is ussualy called when changes to `this`
   * are finished
   *
   *     var persistent1 = new PersistentMap.from({'a':1});
   *     var transient = persistent1.asTransient();
   *     transient.doInsert({'b':2});
   *     var persistent2 = new transient.asPersistent();
   */
  PersistentMap asPersistent();
}
