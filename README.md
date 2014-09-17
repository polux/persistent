# Efficient Persistent Data Structures

[![Build Status](https://drone.io/github.com/vacuumlabs/persistent/status.png)](https://drone.io/github.com/vacuumlabs/persistent/latest)

Transient and persistent sets, maps and vectors with utilities

The project is forked from
[polux/persistent](https://github.com/polux/persistent).

## Terminology

  * *Persistent* data structure is an immutable structure, that provides effective
    creation of slightly mutated copies.
  * *Transient* data structure is a mutable structure, that can be effectively
    converted to the persistent data structure. It is ussualy created from
    a persistent structure to apply some changes and then obtain a new persistent
    structure.

## Desing

In the following part, `Structure` stands for any of `Map`, `Set`, `Vector`.

### Interface

There are several interfaces for each `Structure`:
`PersistentStructure` and `TransientStructure` that both
implement `ReadStructure`.

#### `ReadStructure`

Declares the common, read-only interface.

#### `PersistentStructure`

Declares methods for creating mutated copies that has form
of `PersistentStructure someMethod(...)`.

Moreover, some mass operations are provided.
(For example, `union`, `intersection`)

#### `TransientStructure`

Declares methods for mutating itself that has form
of `void doSomeMethod(...)`. Each `doXxx` method coresponds
to the `xxx` method of `PersistentStructure` having the same
arguments and behavior.

It's only purpose is to apply bigger amount of small changes to
the persistent structure effectively. Therefore, no mass
operation interface is provided.

### Lifecycle

In the almost all cases `PersistentStructure` should be used.
However, when a bigger amount of changes should be done,
`TransientStructure` comes to the scene. It is created
by `PersistentStructure.asTransient()`, the changes are
aplied and then `TransientStructure.asPersistent()` is
called to obtain the result.

**WARNING!** After `asPersistent()` is called, the
`TransientStructure` may (in the default implementation will)
become outdated and do not allow more changes
(throwing an exception).

The other possibility is to write a `change(TransientMap m)`,
function, that aplies the changes to `m` and pass it
to the `PersistentStructure.withTransient()`.

### Equality and hash

Two persistent structures are equal if they carry the equal data.
This allows them to be used as map keys - the key is the data in the context,
not the object itself.

Two transient structures are equal if they are the same object.

The hash code is consistent with the equality operator.

## Example

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

