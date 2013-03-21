// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

part of map_bench;

class Benchmark {

  int size;

  Benchmark([this.size = 1000]);

  static warmup() {
    new Benchmark(1000).bench();
  }

  Map<String, int> bench() {
    var res = {};
    res["Linked List"] = _bench(() => new SimplePersistentMap());
    res["Mutable Map"] = _bench(() => new SimplePersistentMap2());
    res["Hash Trie"] = _bench(() => new PersistentMap());
    return res;
  }

  int _bench(PersistentMap empty()) {
    final stopwatch = new Stopwatch();
    stopwatch.start();

    PersistentMap map = empty();
    for (int i = 0; i < size; i++) {
      map = map.insert("key$i", "foo", (String x, String y) => x + y);
      map = map.insert("key$i", "bar", (String x, String y) => x + y);
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

    stopwatch.stop();
    return stopwatch.elapsedMicroseconds;
  }
}
