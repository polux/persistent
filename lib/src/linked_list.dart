// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

part of persistent;

/**
 * Immutable list of elements of type E. All its predecessors must
 * be accessed before accessing an element.
 *
 * Can be either [Cons] or [Nil].
 * [Nil] is an empty list,
 * while [Cons] contains the first element (`elem`)
 * and the rest of the list (`tail`).
 */
abstract class LinkedList<E> implements Iterable<E> {
  bool get isNil;
  bool get isCons;

  /// Converts this to [Nil] or returns `null`
  Nil<E> get asNil;

  /// Converts this to [Cons] or returns `null`
  Cons<E> get asCons;

  /// Passes all the elements of this to [f].
  void foreach(f(A));

  /// A strict (non-lazy) version of [:map:].
  LinkedList strictMap(f(A));

  /// A strict (non-lazy) version of [:where:].
  LinkedList<E> strictWhere(bool f(A));

  /**
   * The equality operator.
   *
   * Two linked lists are equal if and only if they have same lengths,
   * and for each possition, the elements at it are equal.
   */
  bool operator==(other);

  // Documentation inherited from Object
  int get hashCode;
}

/**
 * [LinkedList] builder.
 *
 * Elements are added from the first.
 */
class LinkedListBuilder<E> {
  List<E> _data = [];

  /// Adds the next element to the list.
  void add(E x) {
    _data.add(x);
  }

  /// Adds all the elements of [iterable] to the list.
  void addAll(Iterable<E> iterable) {
    _data.addAll(iterable);
  }

  /**
   * Creates a new list prepending so far added elements to the
   * optional [tail].
   */
  LinkedList<E> build([tail = null]) {
    if (tail == null)
      tail = new Nil<E>();
    for (E x in _data.reversed){
      tail = new Cons<E>(x, tail);
    }
    return tail;
  }
}

abstract class _LinkedListBase<E> extends IterableBase<E>
    implements LinkedList<E> {

  const _LinkedListBase();

  void foreach(f(A)) {
    LinkedList<E> it = this;
    while (!it.isNil) {
      Cons<E> cons = it.asCons;
      f(cons.elem);
      it = cons.tail;
    }
  }

  LinkedList strictMap(f(A)) {
    LinkedListBuilder<E> builder = new LinkedListBuilder<E>();
    LinkedList<E> it = this;
    while (it.isCons) {
      Cons<E> cons = it.asCons;
      E elem = cons.elem;
      builder.add(f(elem));
      it = cons.tail;
    }
    return builder.build();
  }

  LinkedList<E> strictWhere(bool f(A)) {
    LinkedListBuilder<E> builder = new LinkedListBuilder<E>();
    LinkedList<E> it = this;
    while (it.isCons) {
      Cons<E> cons = it.asCons;
      E elem = cons.elem;
      if (f(elem)) builder.add(elem);
      it = cons.tail;
    }
    return builder.build();
  }
}

class _NilIterator<E> implements Iterator<E> {
  const _NilIterator();
  E get current => null;
  bool moveNext() => false;
}

/**
 * Empty [LinkedList]
 */
class Nil<E> extends _LinkedListBase<E> {
  bool get isNil => true;
  bool get isCons => false;
  Nil<E> get asNil => this;
  Cons<E> get asCons => null;

  const Nil();

  toString() => "nil()";

  int get length => 0;

  Iterator<E> get iterator => const _NilIterator();

  bool operator==(other) => other is LinkedList ? other.isNil : false;

  int get hashCode => 0;
}

class _ConsIterator<E> implements Iterator<E> {
  final LinkedList<E> _head;
  LinkedList<E> _current = null;

  _ConsIterator(this._head);

  E get current => _current.isCons ? _current.asCons.elem : null;

  bool moveNext() {
    if (_current == null) {
      _current = _head;
      return _current.isCons;
    }
    if (_current.isCons) {
      _current = _current.asCons.tail;
      return _current.isCons;
    }
    return false;
  }
}

/**
 * Nonempty [LinkedList]
 */
class Cons<E> extends _LinkedListBase<E> {
  final int length;
  final int hashCode;

  /// The first element of this
  final E elem;

  /// The rest of this - without the first element
  final LinkedList<E> tail;

  Cons(elem, tail):
    elem = elem,
    tail = tail,
    length = tail.length + 1,
    hashCode = hash2(elem.hashCode, tail.hashCode);

  bool get isNil => false;
  bool get isCons => true;
  Nil<E> get asNil => null;
  Cons<E> get asCons => this;

  toString() => "cons($elem, $tail)";

  Iterator<E> get iterator => new _ConsIterator<E>(this);

  bool operator==(other){
    if (other is! LinkedList) return false;
    if ( !other.isCons
      || this.hashCode != other.hashCode
      || this.length != other.length
    ) return false;
    var x = this;
    var y = other;
    while(x.isCons){
      if(x.elem != y.elem) return false;
      x = x.tail;
      y = y.tail;
    }
    return true;
  }

}
