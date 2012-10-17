// Copyright 2012 Google Inc. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Author: Paul Brauner (polux@google.com)

/**
 * An immutable map, binding keys of type [K] to values of type [V]. Null values
 * are supported but null keys are not.
 *
 * In all the examples below [{k1: v1, k2: v2, ...}] is a shorthand for
 * [ImmutableMap.fromMap({k1: v1, k2: v2, ...})].
 */
abstract class ImmutableMap<K,V> {

  /** Creates an empty [ImmutableMap] using its default implementation. */
  factory ImmutableMap() => new _EmptyMap();

  /**
   * Creates an immutable copy of [map] using the default implementation of
   * [ImmutableMap].
   */
  factory ImmutableMap.fromMap(Map<K,V> map) {
    ImmutableMap<K,V> result = new _EmptyMap<K,V>();
    map.forEach((K key, V value) {
      result = result.insert(key, value);
    });
    return result;
  }

  /**
   * Returns a new map identical to [this] except that it binds [key] to
   * [value].
   *
   * If [key] was bound to some [oldvalue] in [this], it is nevertheless bound
   * to [value] in the new map. If [key] was bound to some [oldvalue] in
   * [this] and if [combine] is provided then [key] it is bound to
   * [combine(oldvalue, value)] in the new map.
   *
   *     {'a': 1}.insert('b', 2) == {'a': 1, 'b', 2}
   *     {'a': 1, 'b': 2}.insert('b', 3) == {'a': 3, 'b', 3}
   *     {'a': 1, 'b': 2}.insert('b', 3, (x,y) => x - y) == {'a': 3, 'b', -1}
   */
  ImmutableMap<K,V> insert(K key, V value, [V combine(V oldvalue, V newvalue)]);

  /**
   * Returns a new map identical to [this] except that it doesn't bind [key]
   * anymore.
   *
   *     {'a': 1, 'b': 2}.delete('b') == {'a': 1}
   *     {'a': 1}.delete('b') == {'a': 1}
   */
  ImmutableMap<K,V> delete(K key);

  /**
   * Looks up the value possibly bound to [key] in [this]. Returns
   * [Option.some(value)] if it exists, [Option.none()] otherwise.
   *
   *     {'a': 1}.lookup('b') == Option.none()
   *     {'a': 1, 'b': 2}.lookup('b') == Option.some(2)
   */
  Option<V> lookup(K key);

  /**
   * Evaluates [f(key, value)] for each ([key], [value]) pair in [this].
   */
  void forEach(f(K key, V value));

  /**
   * Returns a new map identical to [this] except that the value it possibly
   * binds to [key] has been adjusted by [update].
   *
   *     {'a': 1, 'b': 2}.update('b', (x) => x + 1) == {'a', 1, 'b', 3}
   *     {'a': 1}.update('b', (x) => x + 1) == {'a', 1}
   */
  ImmutableMap<K,V> adjust(K key, V update(V value));

  /**
   * Returns a new map identical to [this] where each value has been updated by
   * [f].
   *
   *     {'a': 1, 'b': 2}.map((x) => x + 1) == {'a', 2, 'b', 3}
   *     {}.map((x) => x + 1) == {}
   */
  ImmutableMap mapValues(f(V value));

  /**
   * Returns the number of (key, value) pairs in [this].
   *
   *     {}.size() == 0
   *     {'a': 1}.size() == 1
   *     {'a': 1, 'b': 2}.size() == 2
   */
  int size();

  /**
   * Returns a new map whose (key, value) pairs are the union of those of [this]
   * and [other].
   *
   * The union is right-biased: if a key is present in both [this] and [other],
   * the value from [other] is retained. If [combine] is provided, the retained
   * value for a [key] present in both [this] and [other] is then
   * [combine(leftvalue, rightvalue)] where [leftvalue] is the value bound to
   * [key] in [this] and [rightvalue] is the one bound to [key] in [other].
   *
   *     {'a': 1}.union({'b': 2}) == {'a': 1, 'b': 2}
   *     {'a': 1}.union({'a': 3, 'b': 2}) == {'a': 3, 'b': 2}
   *     {'a': 1}.union({'a': 3, 'b': 2}, (x,y) => x + y) == {'a': 4, 'b': 2}
   *
   * Note that [union] is commutative if and only if [combine] is provided and
   * if it is commutative.
   */
  ImmutableMap<K,V>
      union(ImmutableMap<K,V> other, [V combine(V left, V right)]);

  /**
   * Returns a mutable copy of [this].
   */
  Map<K,V> toMap();
}

/**
 * A base class for implementations of [ImmutableMap].
 */
abstract class ImmutableMapBase<K,V>
    implements ImmutableMap<K,V> {
  Map<K,V> toMap() {
    Map<K,V> result = new Map<K,V>();
    this.forEach((K k, V v) { result[k] = v; });
    return result;
  }

  String toString() {
    StringBuffer buffer = new StringBuffer('{');
    bool comma = false;
    this.forEach((K k, V v) {
      if (comma) buffer.add(', ');
      buffer.add('$k: $v');
      comma = true;
    });
    buffer.add('}');
    return buffer.toString();
  }
}
