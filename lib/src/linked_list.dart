// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

part of persistent;

abstract class LinkedList<E> implements Iterable<E> {
  bool get isNil;
  bool get isCons;
  Nil<E> get asNil;
  Cons<E> get asCons;

  void foreach(f(A));

  /// A strict (non-lazy) version of [:map:].
  LinkedList strictMap(f(A));

  /// A strict (non-lazy) version of [:where:].
  LinkedList<E> strictWhere(bool f(A));
}

class LinkedListBuilder<E> {
  final List<E> _buffer = [];

  void add(E x) {
    _buffer.add(x);
  }

  LinkedList<E> _build(int i, LinkedList<E> tail) {
    if (i == _buffer.length) return tail;
    return new Cons<E>(_buffer[i], _build(i+1, tail));
  }

  LinkedList<E> build([tail = null]) {
    if (tail == null)
      tail = new Nil<E>();
    return _build(0, tail);
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

class Nil<E> extends _LinkedListBase<E> {
  final int length = 0;
  final bool isNil = true;
  final bool isCons = false;
  final Cons<E> asCons = null;
  Nil<E> get asNil => this;

  const Nil();

  toString() => "nil()";

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

class Cons<E> extends _LinkedListBase<E> {
  final int length;
  final E elem;
  final LinkedList<E> tail;

  const Cons(this.elem, tail)
    : this.tail = tail
    , this.length = tail.length + 1;

  bool get isNil => false;
  bool get isCons => true;
  Nil<E> get asNil => null;
  Cons<E> get asCons => this;

  toString() => "cons($elem, $tail)";

  Iterator<E> get iterator => new _ConsIterator<E>(this);
}
