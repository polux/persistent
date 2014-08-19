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
class PersistentMap<K, V>
        extends IterableBase<Pair<K, V>>
        implements Iterable<Pair<K, V>> {
  NodeBase _root;

  /** Creates an empty [PersistentMap] using its default implementation. */
  PersistentMap() {
    this._root = new _EmptyMap<K, V>(null);
  }

  /**
   * Creates an immutable copy of [map] using the default implementation of
   * [PersistentMap].
   */
  PersistentMap.fromMap(Map<K, V> map) {
    _root = new _EmptyMap<K, V>(null);
    map.forEach((K key, V value) {
      _root = _root.insert(null, key, value);
    });
  }

  /**
   * Creates a [PersistentMap] from an [Iterable] of [Pair]s using the default
   * implementation of [PersistentMap].
   */
  PersistentMap.fromPairs(Iterable<Pair<K, V>> pairs) {
    _root = new _EmptyMap<K, V>();
    pairs.forEach((pair) {
      _root = _root.insert(null, pair.fst, pair.snd);
    });
  }

  PersistentMap._new(NodeBase this._root);

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
      insert(K key, V value, [V combine(V oldvalue, V newvalue)]) {
        return new PersistentMap._new(_root.insert(null, key, value, combine));
      }

  /**
   * Returns a new map identical to `this` except that it doesn't bind [key]
   * anymore.
   *
   *     {'a': 1, 'b': 2}.delete('b') == {'a': 1}
   *     {'a': 1}.delete('b') == {'a': 1}
   */
  PersistentMap<K, V> delete(K key) {
    return new PersistentMap._new(_root.delete(null, key));
  }

  /**
   * Looks up the value possibly bound to [key] in `this`. Returns
   * `Option.some(value)` if it exists, `Option.none()` otherwise.
   *
   *     {'a': 1}.lookup('b') == Option.none()
   *     {'a': 1, 'b': 2}.lookup('b') == Option.some(2)
   */
  Option<V> lookup(K key) {
    return _root.lookup(key);
  }

  /**
   * Returns the value for the given [key] or [:null:] if [key]
   * is not in the map.
   */
  V operator [](K key) {
    return _root.lookup(key).asNullable;
  }

  /**
   * Evaluates `f(key, value)` for each (`key`, `value`) pair in `this`.
   */
  void forEachKeyValue(f(K key, V value)) => _root.forEachKeyValue(f);

  /**
   * Returns a new map identical to `this` except that the value it possibly
   * binds to [key] has been adjusted by [update].
   *
   *     {'a': 1, 'b': 2}.adjust('b', (x) => x + 1) == {'a', 1, 'b', 3}
   *     {'a': 1}.adjust('b', (x) => x + 1) == {'a', 1}
   */
  PersistentMap<K, V> adjust(K key, V update(V value)) {
    return  new PersistentMap._new(_root.adjust(null, key, update));
  }

  /**
   * Returns a new map identical to `this` where each value has been updated by
   * [f].
   *
   *     {'a': 1, 'b': 2}.mapValues((x) => x + 1) == {'a', 2, 'b', 3}
   *     {}.mapValues((x) => x + 1) == {}
   */
  PersistentMap mapValues(f(V value)) {
    return new PersistentMap._new(_root.mapValues(null, f));
  }

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
      union(PersistentMap<K, V> other, [V combine(V left, V right)]) =>
        new PersistentMap._new(_root.union(null, other._root, combine));

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
      intersection(PersistentMap<K, V> other, [V combine(V left, V right)]) =>
        new PersistentMap._new(_root.intersection(null, other._root, combine));

  /// Returns a mutable copy of `this`.
  Map<K, V> toMap() {
    return _root.toMap();
  }

  /// The keys of `this`.
  Iterable<K> get keys => _root.keys;

  /// The values of `this`.
  Iterable<V> get values => _root.values;

  /// Randomly picks an entry of `this`.
  Pair<K, V> pickRandomEntry([Random random]) => _root.pickRandomEntry(random);

  /// A strict (non-lazy) version of [map].
  PersistentMap strictMap(Pair f(Pair<K, V> pair)) =>
      new PersistentMap.fromPairs(this.map(f));

  /// A strict (non-lazy) version of [where].
  PersistentMap<K, V> strictWhere(bool f(Pair<K, V> pair)) =>
      new PersistentMap<K, V>.fromPairs(this.where(f));

  Iterator get iterator => _root.iterator;

  int get length => _root.length;
}

/**
 * A base class for implementations of [PersistentMap].
 */
abstract class NodeBase<K, V>
    extends IterableBase<Pair<K, V>> {

  int _length;
  get length => _length;

  NodeBase(this._length);

  Map<K, V> toMap() {
    Map<K, V> result = new Map<K, V>();
    this.forEachKeyValue((K k, V v) { result[k] = v; });
    return result;
  }

  String toString() {
    StringBuffer buffer = new StringBuffer('{');
    bool comma = false;
    this.forEachKeyValue((K k, V v) {
      if (comma) buffer.write(', ');
      buffer.write('$k: $v');
      comma = true;
    });
    buffer.write('}');
    return buffer.toString();
  }

  // Optimized version of Iterable's contains
  bool contains(Pair<K, V> entry) {
    final value = this.lookup(entry.fst);
    return value.isDefined && value.value == entry.snd;
  }

  Iterable<K> get keys => this.map((Pair<K, V> pair) => pair.fst);

  Iterable<V> get values => this.map((Pair<K, V> pair) => pair.snd);

  Pair<K, V> pickRandomEntry([Random random]) =>
      elementAt((random != null ? random : _random).nextInt(this.length));

  V operator [](K key) => this.lookup(key).asNullable;

  NodeBase<K, V>
    insert(Owner owner, K key, V value, [V combine(V oldvalue, V newvalue)]);

  NodeBase<K, V> delete(Owner owner, K key);

  Option<V> lookup(K key);

  void forEachKeyValue(f(K key, V value));

  NodeBase<K, V> adjust(Owner owner, K key, V update(V value));

  NodeBase mapValues(Owner owner, f(V value));

  NodeBase<K, V>
      union(Owner owner, NodeBase<K, V> other, [V combine(V left, V right)]);

  NodeBase<K, V>
      intersection(Owner owner, NodeBase<K, V> other, [V combine(V left, V right)]);

}
