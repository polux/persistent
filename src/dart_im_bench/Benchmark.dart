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

class Benchmark {

  int _size;

  Benchmark([this._size = 1000]);

  Map<String, int> bench() {
    var res = {};
    res["Linked List"] = _bench(() => new SimpleImmutableMap());
    res["Mutable Map"] = _bench(() => new SimpleImmutableMap2());
    res["Hash Trie"] = _bench(() => new ImmutableMap());
    return res;
  }

  int _bench(ImmutableMap empty()) {
    int start = Clock.now();

    ImmutableMap map = empty();
    for (int i = 0; i < _size; i++) {
      map = map.insert("key$i", "foo", (String x, String y) => x.concat(y));
      map = map.insert("key$i", "bar", (String x, String y) => x.concat(y));
    }

    for (int i = _size * 2; i >= 0; i--) {
      map.lookup("key$i");
    }
    for (int i = 0; i <= _size * 2; i++) {
      map.lookup("key$i");
    }
    
    ImmutableMap saved = map;
    for (int i = _size * 2; i >= 0; i--) {
      map = map.delete("key$i");
    }
    map = saved;
    for (int i = 0; i < _size * 2; i++) {
      map = map.delete("key$i");
    }

    return Clock.now() - start;
  }
}
