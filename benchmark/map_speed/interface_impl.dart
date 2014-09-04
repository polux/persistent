part of map_bench;

class PersistentMapInterface<K, V>
  extends EncapsulatingInterface<K, V, PersistentMap<K, V>>{
    
  create() => object = new PersistentMap<K, V>();
  
  insert(K key, V value, V combine(V left, V right)) =>
    object = object.insert(key, value, combine);
  
  lookup(K key) => object.lookup(key, orElse: ()=>null);
  
  delete(K key) => object = object.delete(key, safe: true);
  
  _copy() => object;
}

  
class TransientMapInterface<K, V>
  extends EncapsulatingInterface<K, V, TransientMap<K, V>>{
    
  create() => object = new PersistentMap<K, V>().asTransient();
  
  insert(K key, V value, V combine(V left, V right)) =>
    object.doInsert(key, value, combine);
  
  lookup(K key) => object.lookup(key, orElse: ()=>null);
  
  delete(K key) => object.doDelete(key, safe: true);
  
  _copy(){
    var copy = object.asPersistent();
    object = copy.asTransient();
    return copy.asTransient();
  }
}

    
class StandardMapInterface<K, V>
  extends EncapsulatingInterface<K, V, Map<K, V>>{
    
  create() => object = new Map<K, V>();
  
  insert(K key, V value, V combine(V left, V right)) =>
    object[key] =
      object.containsKey(key) ? combine(object[key], value) : value;
  
  lookup(K key) => object[key];
  
  delete(K key) => object.remove(key);
  
  _copy() => new Map.from(object);
}

  
class CopyMapInterface<K, V>
 extends EncapsulatingInterface<K, V, Map<K, V>>{
   
 create() => object = new Map<K, V>();
 
 insert(K key, V value, V combine(V left, V right)){
   object = new Map.from(object);
   object[key] =
     object.containsKey(key) ? combine(object[key], value) : value;
 }
   
 lookup(K key) => object[key];
 
 delete(K key){
   object = new Map.from(object);
   object.remove(key);
 }
 
 _copy() => object;
}

 
class LinkedListInterface<K, V>
  extends EncapsulatingInterface<K, V, LinkedList<Pair<K, V>>>{
    
  create() => object = new Nil<Pair<K, V>>();
  
  insert(K key, V value, V combine(V left, V right)){
    LinkedListBuilder<Pair<K, V>> builder =
        new LinkedListBuilder<Pair<K, V>>();
    LinkedList<Pair<K, V>> it = object;
    while (it.isCons) {
      Cons<Pair<K, V>> cons = it.asCons;
      Pair<K, V> elem = cons.elem;
      if (elem.fst == key) {
        builder.add(new Pair<K, V>(key, combine(elem.snd, value)));
        return builder.build(cons.tail);
      }
      builder.add(elem);
      it = cons.tail;
    }
    builder.add(new Pair<K, V>(key, value));
    object = builder.build();
  }
    
  lookup(K key){
    LinkedList<Pair<K, V>> it = object;
    while (it.isCons) {
      Cons<Pair<K, V>> cons = it.asCons;
      Pair<K, V> elem = cons.elem;
      if (elem.fst == key) return;
      it = cons.tail;
    }
  }
  
  delete(K key){
    object = object.strictWhere((p) => p.fst != key);
  }
  
  _copy() => object;
 }
       
