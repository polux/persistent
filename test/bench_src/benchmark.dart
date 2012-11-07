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

part of map_bench;

class Benchmark {

  int size;

  Benchmark([this.size = 1000]);

  Map<String, int> bench() {
    var res = {};
    res["Linked List"] = _bench(() => new SimplePersistentMap());
    res["Mutable Map"] = _bench(() => new SimplePersistentMap2());
    res["Hash Trie"] = _bench(() => new PersistentMap());
    return res;
  }

  int _bench(PersistentMap empty()) {
    int start = new Date.now().millisecondsSinceEpoch;

    PersistentMap map = empty();
    for (int i = 0; i < size; i++) {
      map = map.insert("key$i", "foo", (String x, String y) => x.concat(y));
      map = map.insert("key$i", "bar", (String x, String y) => x.concat(y));
    }

    for (int i = size * 2; i >= 0; i--) {
      map.lookup("key$i");
    }
    for (int i = 0; i <= size * 2; i++) {
      map.lookup("key$i");
    }

    PersistentMap saved = map;
    for (int i = size * 2; i >= 0; i--) {
      map = map.delete("key$i");
    }
    map = saved;
    for (int i = 0; i < size * 2; i++) {
      map = map.delete("key$i");
    }

    return new Date.now().millisecondsSinceEpoch - start;
  }
}
