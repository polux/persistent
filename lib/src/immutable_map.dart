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
 * An immutable map from keys of type [K] to values of type [V]. Null values are
 * supported but null keys aren't.
 */
abstract class ImmutableMap<K extends Hashable,V> {
  /** Creates an empty [ImmutableMap] with the default implementation */
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
   * Associates [value] to [key] in [this]. If [key] was already associated to
   * [: oldvalue :] in [this] it is replaced by [value] unless [combine] is
   * provided, in which case it is replaced by [: combine(oldvalue, value) :].
   * Thus, [: m.insert(k, v) :] is equivalent to
   * [: m.insert(k, v , (x, y) => y) :].
   */
  ImmutableMap<K,V> insert(K key, V value, [V combine(V x, V y)]);
  ImmutableMap<K,V> delete(K key);
  Option<V> lookup(K key);

  void forEach(f(K,V));
  ImmutableMap<K,V> adjust(K key, V update(V));
  // forall T, ImmutableMap<K,T> mapValues(T f(V))
  ImmutableMap mapValues(f(V));

  int size();
  /** m.union(k, v) == m.union(k, v, (x, y) => y) */
  ImmutableMap<K,V> union(ImmutableMap<K,V> other, [V combine(V x, V y)]);

  Map<K,V> toMap();
}

abstract class ImmutableMapBase<K extends Hashable,V>
    implements ImmutableMap<K,V> {
  Map<K,V> toMap() {
    Map<K,V> result = new Map<K,V>();
    this.forEach((K k, V v) { result[k] = v; });
    return result;
  }

  String toString() => toMap().toString();
}
