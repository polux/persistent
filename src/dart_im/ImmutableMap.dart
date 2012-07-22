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

interface ImmutableMap<K extends Hashable,V>
    default _ImmutableMapFactory<K extends Hashable,V> {
  ImmutableMap();
  ImmutableMap.fromMap(Map<K,V> map);

  /** m.insert(k, v) == m.insert(k, v, (x, y) => y) */
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

class _Stop implements Exception {}

abstract class AImmutableMap<K extends Hashable,V>
    implements ImmutableMap<K,V> {
  Map<K,V> toMap() {
    Map<K,V> result = new Map<K,V>();
    this.forEach((K k, V v) { result[k] = v; });
    return result;
  }

  bool operator ==(ImmutableMap<K,V> other) {
    Map<K,V> mm1 = this.toMap();
    Map<K,V> mm2 = other.toMap();
    if (mm1.length != mm2.length) return false;
    try {
      mm1.forEach((K k, V v) {
        if (!mm2.containsKey(k)) throw new _Stop();
        if (mm2[k] != v) throw new _Stop();
      });
    } catch (_Stop e) {
      return false;
    }
    return true;
  }

  String toString() => toMap().toString();
}
