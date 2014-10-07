// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

part of persistent;

/**
 * A read-only vector, ordered collection of elements of type [E].
 *
 * There is no default implementation of [ReadVector], since it just
 * specifies the common interface of [PersistentVector] and [TransientVector].
 */
abstract class ReadVector<E> implements Iterable<E>, Persistent {

  /**
   * Returns element at given [index].
   *
   * If the [index] is outside the array, [notFound] is returned instead.
   * If [notFound] is not set, [RangeError] is thrown.
   *
   *     var v = new PersistentVector.from(["Hello","world"]);
   *     v.get(0); // returns "Hello"
   *     v.get(2, null); // returns null
   *     v.get(2); // throws RangeError
   */
  E get(int index, [E notFound]);

  /**
   * Returns element at given [index].
   *
   * Throws [RangeError] if the [index] is outside the array.
   *
   *     var v = new PersistentVector.from(["Hello","world"]);
   *     print(v[0]); // prints "Hello"
   *     print(v[2]); // throws RangeError
   */
  E operator[](int index);

  /// The first element of `this`
  E get first;

  /// The last element of `this`
  E get last;

  /// Checks if it contains key [key].
  bool hasKey(int key);
}


/**
 * A persistent vector, resizable ordered collection of elements of type [E].
 *
 * Persistent data structure is an immutable structure, that provides effective
 * creation of slightly mutated copies.
 */
abstract class PersistentVector<E> implements ReadVector<E> {

  /**
   * Returns a new vector identical to `this` except that
   * element at [index] is [value].
   *
   * Throws [RangeError] if the [index] is outside the array.
   *
   *     var v = new PersistentVector.from(["A","B"]);
   *     v.set(1,":)"); // returns ["A",":)"]
   *     v.set(0,":("); // returns [":(","B"]
   *     v.set(2,":D"); // throws RangeError
   */
  PersistentVector<E> set(int index, E value);

  /**
   * Returns a new vector identical to `this` except that
   * the [value] is appended to its end.
   *
   *     var v = new PersistentVector.from(["one","two"]);
   *     v.push("three"); // returns ["one","two","three"]
   *     v.push("four"); // returns ["one","two","four"]
   */
  PersistentVector<E> push(E value);

  /**
   * Returns a new vector identical to `this` except that
   * the last element is removed.
   *
   * Throws [RangeError] if  `this` is empty
   *
   *     var v = new PersistentVector.from(["one","two"]);
   *     v.pop(); // returns ["one"]
   *     v.pop(); // still returns ["one"]
   *     new PersistentVector.from([]).pop(); // throws RangeError
   */
  PersistentVector<E> pop();

  /**
   * Returns a transient copy of `this`.
   *
   * This is ussualy called to do some changes and
   * then create a new [PersistentVector].
   *
   *     var persistent1 = new PersistentVector.from([1]);
   *     var transient = persistent1.asTransient();
   *     transient.doPush(2);
   *     var persistent2 = new transient.asPersistent(); // persistent2 is now [1,2]
   */
  TransientVector<E> asTransient();

  /**
   * Creates an empty [PersistentVector] using its default implementation.
   */
  factory PersistentVector() => new _PersistentVectorImpl.empty();

  /**
   * Creates an [PersistentVector] filled by [values]
   * using its default implementation.
   */
  factory PersistentVector.from(Iterable<E> values) => new _PersistentVectorImpl.from(values);

  /**
   * Creates transient copy of `this`, lets it to be modified by [change]
   * and returns persistent result.
   *
   *     var persistent1 = new PersistentVector.from([1,2]);
   *     var persistent2 = persistent1.withTransient((v){
   *       v.doPush(3);
   *     });
   */
  PersistentVector<E> withTransient(void change(TransientVector<E> vect));


  /**
   * The equality operator.
   *
   * Two persistent vectors are equal if and only if they have same lengths,
   * and for each index, the values at it are equal.
   */
  bool operator==(other);

  /*
   * The documentation is inherited from the Object
   */
  int get hashCode;
}


/**
 * A transient vector, resizable ordered collection of elements of type [E].
 *
 * Transient data structure is a mutable structure, which can be efficiently
 * converted to the persistent data structure. It is usually created from
 * a persistent structure to apply some changes and obtain a new persistent
 * structure.
 */
abstract class TransientVector<E> implements ReadVector<E> {

  /**
   * Sets the element at [index] to be [value].
   *
   * Throws [RangeError] if the [index] is outside the array
   *
   *     var v = new PersistentVector.from(["A","B"]).asTransient();
   *     v.set[1] = ":)"; // v is now ["A",":)"]
   *     v.set[0] = ":("; // v is now [":(",":)"]
   *     v.set[2] = ":D"; // throws RangeError
   *
   */
  void operator []=(int index, E value);

  /**
   * Sets the element at [index] to be [value].
   *
   * Throws [RangeError] if the [index] is outside the array
   *
   *     var v = new PersistentVector.from(["A","B"]).asTransient();
   *     v.doSet(1,":)"); // v is now ["A",":)"]
   *     v.doSet(0,":("); // v is now [":(",":)"]
   *     v.doSet(2,":D"); // throws RangeError
   */
  void doSet(int index, E value);

  /**
   * Appends [value] to the end of `this`.
   *
   *     var v = new PersistentVector.from(["one","two"]).asTransient();
   *     v.doPush("three"); // v is now ["one","two","three"]
   *     v.doPush("four"); // v is now ["one","two","three","four"]
   */
  void doPush(E value);

  /**
   * Removes the last element of `this`.
   *
   * Throws [RangeError] if  `this` is empty
   *
   *     var v = new PersistentVector.from(["one","two"]).asTransient();
   *     v.doPop(); // v is now ["one"]
   *     v.doPop(); // v is now []
   *     v.doPop(); // throws RangeError
   */
  void doPop();

  /**
   * Returns a persistent copy of `this`.
   *
   * This is ussualy called when changes to `this`
   * are finished
   *
   *     var persistent1 = new PersistentVector.from([1]);
   *     var transient = persistent1.asTransient();
   *     transient.doPush(2);
   *     var persistent2 = new transient.asPersistent();
   */
  PersistentVector<E> asPersistent();
}
