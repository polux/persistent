// Copyright (c) 2014, VaccumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

library overall_example;

import 'package:vacuum_persistent/persistent.dart';

example(){
  var couple = new PMap.fromMap({'father': 'Homer', 'mother': 'Marge'});
  var withChild = couple.assoc('boy', 'Bart');
  print(couple); // {mother: Marge, father: Homer}
  print(withChild); // {boy: Bart, mother: Marge, father: Homer}
}

example2(){
  // deeply persist the structure of Maps and Lists
  var a = per({[1,2]: 'tower', [1,3]: 'water'});
  var b = per({[1,2]: 'tower', [1,3]: 'water'});
  assert(a==b);
  // kids, don't try this with standard List, it ain't work
  print(a[per([1, 2])]); // prints hello
}

main() {

  // Persistency:

  final map1 = new PMap.fromMap({"a":1, "b":2});
  final map2 = new PMap.fromMap({"b":3, "c":4});

  print(map1["a"]); // 1
  print(map1.get("b")); // 2
  print(map1.get("c", ":(")); // :(

  print(map1.assoc("c", 3)); // {a: 1, b: 2, c: 3}
  print(map1.assoc("d", 4)); // {a: 1, b: 2, d: 4}

  final map3 = map2._update("c", (x) => x+3);
  print(map3.delete("b")); // {c: 7}
  print(map3.delete("a", missingOk: true)); // {b: 3, c: 7}

  print(map1); // {a: 1, b: 2}
  print(map2); // {b: 3, c: 4}
  print(map3); // {b: 3, c: 7}

  // Transiency:

  final vector1 = new PVec.from(["x", "y"]);

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
  // [Pair(a, 1), Pair(a, 2), Pair(a, 3), Pair(b, 1), Pair(b, 2), Pair(b, 3)]

}
