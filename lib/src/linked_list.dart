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
}

/**
 * [LinkedList] builder.
 *
 * Elements are added from the first.
 */
class LinkedListBuilder<E> {
  LinkedList<E> _first = null;
  Cons<E> _last = null;

  /// Adds the next element to the list
  void add(E x) {
    Cons<E> cons = new Cons<E>(x, null);
    if (_first == null) {
      _first = cons;
    } else {
      _last.tail = cons;
    }
    _last = cons;
  }

  /**
   * Creates a new list prepending so far added elements to the
   * optional [tail].
   */
  LinkedList<E> build([tail = null]) {
    if (tail == null)
      tail = new Nil<E>();
    if (_first == null) {
      return tail;
    } else {
      _last.tail = tail;
      return _first;
    }
  }
}

abstract class _LinkedListBase<E> extends IterableBase<E>
    implements LinkedList<E> {

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

  toString() => "nil()";

  int get length => 0;

  Iterator<E> get iterator => const _NilIterator();
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
  int _length = null;

  /// The first element of this
  final E elem;

  /// The rest of this - without the first element
  LinkedList<E> tail;

  Cons(this.elem, this.tail);

  bool get isNil => false;
  bool get isCons => true;
  Nil<E> get asNil => null;
  Cons<E> get asCons => this;

  toString() => "cons($elem, $tail)";

  int get length {
    if (_length == null) {
      _length = tail.length + 1;
    }
    return _length;
  }

  Iterator<E> get iterator => new _ConsIterator<E>(this);
}
