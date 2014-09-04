part of vector_speed;

class PersistentVectorInterface<E>
  extends EncapsulatingInterface<E, PersistentVector<E>>{
    
  create() => object = new PersistentVector<E>();
  
  push(E value) =>
    object = object.push(value);
  
  get(int index) => object.get(index);
    
  set(int index, E value) => object = object.set(index, value);
    
  pop() => object = object.pop();
  
  _copy() => object;
}

  
class TransientVectorInterface<E>
  extends EncapsulatingInterface<E, TransientVector<E>>{
    
  create() => object = new PersistentVector<E>().asTransient();
  
  push(E value) =>
    object.doPush(value);
  
  get(int index) => object.get(index);
    
  set(int index, E value) => object.doSet(index, value);
    
  pop() => object.doPop();
  
  _copy(){
    return new PersistentVector.from(object).asTransient();
  }
}

    
class ListInterface<E>
  extends EncapsulatingInterface<E, List<E>>{
    
  create() => object = [];
  
  push(E value) =>
    object.add(value);
  
  get(int index) => object[index];
    
  set(int index, E value) => object[index] =  value;
    
  pop() => object.removeLast();
      
  _copy(){
    return new List.from(object);
  }
}

      


  

       
