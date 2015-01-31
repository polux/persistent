// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

library map_example;

import 'package:vacuum_persistent/persistent.dart';

main() {

    PMap map = new PMap.from({"a":1, "b":2});

    print(map["a"]); // 1
    print(map.get("b")); // 2
    print(map.get("c", ":(")); // :(

    print(map.assoc("c", 3)); // {a: 1, b: 2, c: 3}
    print(map.assoc("d", 4)); // {a: 1, b: 2, d: 4}

    print(map.update("a", (x) => x+1)); // {a: 2, b: 2}
    print(map.delete("b")); // {a: 1}
    print(map.delete("not-a-key", missingOk: true)); // does not throw

    print(map); // {a: 1, b: 2} (i.e. the map stays unchanged)

    // Transiency:

    final vec = new PVec.from(["a", "b"]);

    print(vec.push("c")); // (a, b, c)
    print(vec.push("d")); // (a, b, d)

    var temp = vec.asTransient();
    temp.doPush("c");
    temp.doPush("d");
    temp[0] = "A";
    final vec2 = temp.asPersistent();
    print(vec2); // (A, b, c, d)

}
