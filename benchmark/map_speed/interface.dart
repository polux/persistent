part of map_bench;

abstract class BenchmarkInterface<K, V>{
  
  void create();
  void insert(K key, V value, V combine(V left, V right));
  void lookup(K key);
  void delete(K key);
  
  void save();
  void restore();
}


abstract class EncapsulatingInterface<K, V, T>
  extends BenchmarkInterface<K, V>{
  
  T object = null;
  T object_copy = null;
  
  T _copy();
  
  save(){
    object_copy = _copy();
  }
  
  restore(){
    return object_copy;
  }
}
