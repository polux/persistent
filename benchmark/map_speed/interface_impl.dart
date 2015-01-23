// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

part of map_bench;

class PersistentMapInterface<K, V>
  extends EncapsulatingInterface<K, V, PMap<K, V>>{

  PersistentMapInterface(){
    this.object = new PMap<K, V>();
  }

  assoc(K key, V value) =>
    object = object.assoc(key, value);

  get(K key) => object.get(key, null);

  delete(K key) => object = object.delete(key);

  _copy() => object;
}


class TransientMapInterface<K, V>
  extends EncapsulatingInterface<K, V, TMap<K, V>>{

  TransientMapInterface(){
    this.object = new PMap<K, V>().asTransient();
  }

  assoc(K key, V value) =>
    object.doAssoc(key, value);

  get(K key) => object.get(key, null);

  delete(K key) => object.doDelete(key);

  _copy(){
    var copy = object.asPersistent();
    object = copy.asTransient();
    return copy.asTransient();
  }
}


class StandardMapInterface<K, V>
  extends EncapsulatingInterface<K, V, Map<K, V>>{

  StandardMapInterface(){
    object = new Map<K, V>();
  }

  assoc(K key, V value) =>
    object[key] = value;

  get(K key) => object[key];

  delete(K key) => object.remove(key);

  _copy() => new Map.from(object);
}


class CopyMapInterface<K, V>
 extends EncapsulatingInterface<K, V, Map<K, V>>{

 create() => object = new Map<K, V>();

 assoc(K key, V value){
   object = new Map.from(object);
   object[key] = value;
 }

 get(K key) => object[key];

 delete(K key){
   object = new Map.from(object);
   object.remove(key);
 }

 _copy() => object;
}


class LinkedListInterface<K, V>
  extends EncapsulatingInterface<K, V, LinkedList<Pair<K, V>>>{

  create() => object = new Nil<Pair<K, V>>();

  assoc(K key, V value){
    LinkedListBuilder<Pair<K, V>> builder =
        new LinkedListBuilder<Pair<K, V>>();
    LinkedList<Pair<K, V>> it = object;
    while (it.isCons) {
      Cons<Pair<K, V>> cons = it.asCons;
      Pair<K, V> elem = cons.elem;
      if (elem.fst == key) {
        builder.add(new Pair<K, V>(key, value));
        return builder.build(cons.tail);
      }
      builder.add(elem);
      it = cons.tail;
    }
    builder.add(new Pair<K, V>(key, value));
    object = builder.build();
  }

  get(K key){
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

