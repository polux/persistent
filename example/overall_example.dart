// Copyright (c) 2014, VaccumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

library overall_example;

import 'package:persistent/persistent.dart';

main() {

  // Persistency:

  final map1 = new PersistentMap.fromMap({"a":1, "b":2});
  final map2 = new PersistentMap.fromMap({"b":3, "c":4});

  print(map1["a"]); // 1
  print(map1.lookup("b")); // 2
  print(map1.lookup("c", orElse: ()=>":(")); // :(

  print(map1.insert("c", 3)); // {a: 1, b: 2, c: 3}
  print(map1.insert("d", 4)); // {a: 1, b: 2, d: 4}

  final map3 = map2.insert("c", 3, (x,y) => x+y);
  print(map3.delete("b")); // {c: 7}
  print(map3.delete("a", safe: true)); // {b: 3, c: 7}

  print(map1); // {a: 1, b: 2}
  print(map2); // {b: 3, c: 4}
  print(map3); // {b: 3, c: 7}

  // Transiency:

  final vector1 = new PersistentVector.from(["x", "y"]);

  print(vector1.push("z")); // (x, y, z)
  print(vector1.push("q")); // (x, y, q)

  var temp = vector1.asTransient();
  temp.doPush("z");
  temp.doPush("q");
  temp[1] = "Y";
  final vector2 = temp.asPersistent();

  final vector3 = vector2.withTransient((TransientVector v){
    v.doSet(2, "Z");
    v.doPop();
    v[0] = "X";
  });

  print(vector1); // (x, y)
  print(vector2); // (x, Y, z, q)
  print(vector3); // (X, Y, Z)

  // Features

  print(map1.toList()); // [Pair(a, 1), Pair(b, 2)]

  final set1 = new PersistentSet.from(["a", "b"]);
  final set2 = new PersistentSet.from([1, 2, 3]);
  print((set1 * set2).toList());
  // [Pair(a, 2), Pair(a, 1), Pair(b, 3), Pair(b, 2), Pair(b, 1), Pair(a, 3)]

}