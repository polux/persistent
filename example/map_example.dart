#library('map_example');
#import('package:dart-immutable/dart_immutable.dart');

main() {
  var emptyMap = new ImmutableMap<String,int>();
  var m1 = emptyMap.insert("a",1).insert("b",2);
  var m2 = new ImmutableMap<String,int>.fromMap({"a": 3, "c":4});

  print(m1);  // {a: 1, b: 2}
  print(m2);  // {c: 4, a: 3}
  print(m1.lookup("a"));  // Option.some(1)
  print(m1.lookup("c"));  // Option.none()

  var m3 = m1.delete("a");
  print(m1);  // {a: 1, b: 2}
  print(m3);  // {b: 2}

  var m4 = m1.union(m2, (n,m) => n + m);
  print(m4);  // {c: 4, a: 4, b: 2}

  var m5 = m1.mapValues((n) => n + 1);
  print(m5);  // {a: 2, b: 3}

  var m6 = m1.adjust("a", (n) => n + 1);
  print(m6);  // {a: 2, b: 2}
}
