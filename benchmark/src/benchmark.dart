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
    res["Hash Trie Transient"] = _benchT(() => new PersistentMap().asTransient());
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

  int _benchT(TransientMap empty()) {
    final stopwatch = new Stopwatch();
    stopwatch.start();

    TransientMap map = empty();
    for (int i = 0; i < size; i++) {
      map = map.doInsert("key$i", "foo", (String x, String y) => x + y);
      map = map.doInsert("key$i", "bar", (String x, String y) => x + y);
    }

    for (int i = size * 2; i >= 0; i--) {
      map.doLookup("key$i");
    }
    for (int i = 0; i <= size * 2; i++) {
      map.doLookup("key$i");
    }
    PersistentMap mapP = map.asPersistent();
    TransientMap saved = mapP.asTransient();
    map = mapP.asTransient();

    for (int i = size * 2; i >= 0; i--) {
      map = map.doDelete("key$i");
    }
    map = saved;
    for (int i = 0; i < size * 2; i++) {
      map = map.doDelete("key$i");
    }

    stopwatch.stop();
    return stopwatch.elapsedMicroseconds;
  }
}
