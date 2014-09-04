part of vector_speed;

abstract class BenchmarkInterface<E>{
  
  void create();
  void push(E value);
  void set(int index, E value);
  void get(int index);
  void pop();
  
  void save();
  void restore();
}


abstract class EncapsulatingInterface<E, T>
  extends BenchmarkInterface<E>{
  
  T object = null;
  T object_copy = null;
  
  T _copy();
  
  save(){
    object_copy = _copy();
  }
  
  restore(){
    object = object_copy;
  }
}
