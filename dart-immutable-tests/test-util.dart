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
 * A datatype with an imperfect hash function
 */
class Key implements Hashable {
  int i;
  bool b;
  Key(this.i, this.b);
  bool operator ==(Key other) {
    if (other is! Key) return false;
    return i == other.i && b == other.b;
  }
  int hashCode() => i.hashCode();
  toString() => "Key($i,$b)";
}

class ImmutableMapGen {
  Random _rand;

  ImmutableMapGen(int seed): _rand = new Random(seed);

  Key randomKey(int intSize) =>
    new Key(_rand.nextInt(2 * intSize + 1) - intSize, _rand.nextBool());

  ImmutableMap<Key, int> randomMap(int size, int intSize) {
    ImmutableMap<Key, int> result = new ImmutableMap<Key, int>();
    for (int i = 0; i < size; i++) {
      result = result.insert(
        randomKey(intSize),
        _rand.nextInt(2 * intSize + 1) - intSize);
    }
    return result;
  }
}

void _tryAndPropagate(bool f(x), var x) {
  bool res;
  try {
    res = f(x);
  } catch (List<String> e) {
    e.add("failed for $x");
    throw e;
  }
  if (!res){
    throw ["failed for $x"];
  }
}

/**
 * Calls f on ever growing random [ImmutableMap]s from size 0 to maxSize.
 * updateCallback is regularily called with a status message.
 */
bool forAllMap(bool f(ImmutableMap<Key, int> m),
    [seed = 0, maxSize = 100, intSize = 100, updateCallback(String) = null]) {
  ImmutableMapGen gen = new ImmutableMapGen(seed);
  for (int size = 0; size <= maxSize; size++) {
    if (updateCallback != null) updateCallback("$size / $maxSize");
    ImmutableMap<Key, int> map = gen.randomMap(size, intSize);
    _tryAndPropagate(f, map);
  }
  return true;
}

/**
 * Calls f on ever growing random integers from [0..0] to [-maxSize..maxSize].
 * updateCallback is regularily called with a status message.
 */
bool forAllInt(bool f(int n),
               [seed = 0, maxSize = 100, updateCallback(String) = null]) {
  Random rand = new Random(seed);
  for (int size = 0; size <= maxSize; size++) {
    if (updateCallback != null) updateCallback("$size / $maxSize");
    _tryAndPropagate(f, rand.nextInt(2 * size + 1) - size);
  }
  return true;
}

/**
 * Calls f on ever growing random keys from [0..0] to [-maxSize..maxSize].
 * updateCallback is regularily called with a status message.
 */
bool forAllKey(bool f(Key n),
               [seed = 0, maxSize = 100, updateCallback(String) = null]) {
  ImmutableMapGen gen = new ImmutableMapGen(seed);
  for (int size = 0; size <= maxSize; size++) {
    if (updateCallback != null) updateCallback("$size / $maxSize");
    _tryAndPropagate(f, gen.randomKey(size));
  }
  return true;
}
