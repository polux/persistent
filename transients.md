## Working with Transients

Persistent data structure can 'unfreeze' into *Transient* structure (TMap, TVec, ..), which is mutable. The purpose of this is to gain some speedup while still working with Persistents. The typical workflow is as follows:

    1. TMap trans = pers.asTransient(); 
    2. do a lot of mutations on trans
    3. Persistent result = trans.asPersistent(); 

There are several notable things here:
- When working with transients, methods like `assoc` or `delete` are no longer there. Instead of these, use `doAssoc`, `doDelete`, etc. These methods return void and mutate the structure inplace. 
- once `.asPersistent` is called, you cannot modify transient anymore. If you try doing it, you'll get an exception. 
- conversion Persistent -> Transient and vice-versa is O(1), which means fast, in this case, really fast.
- everything is safe, i.e. basic contract 'there is no way, how to modify Persistent data structure' still holds
- since there is some repeating pattern in steps 1-3, `PersistentStructure.withTransient(modifier)` helper method exists.

### Equality and hash

Two persistent structures are equal if they carry the equal data.
This allows them to be used as map keys - the key is the data in the context,
not the object itself.

Two transient structures are equal in the standard meaning of a word - if they are the same object.

The hash code is consistent with the equality operator.

## Example

    import 'package:persistent/persistent.dart';
    
    main() {
     
      // Persistency:
      
      PMap map1 = new PMap.from({"a":1, "b":2});
      PMap map2 = new PMap.from({"b":3, "c":4});
      
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
