// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

part of persistent;

abstract class LList<A> implements Iterable<A> {
  factory LList.nil() => new Nil<A>();
  factory LList.cons(A x, LList<A> xs) => new Cons<A>(x, xs);

  bool get isNil;
  bool get isCons;
  Nil<A> get asNil;
  Cons<A> get asCons;

  void foreach(f(A));
  // forall B, LList<B> map(B f(A))
  LList map(f(A));
  LList<A> filter(bool f(A));
}

class LListBuilder<A> {
  LList<A> _first = null;
  Cons<A> _last = null;

  void add(A x) {
    Cons<A> cons = new Cons<A>(x, null);
    if (_first == null) {
      _first = cons;
    } else {
      _last.tail = cons;
    }
    _last = cons;
  }

  LList<A> build([tail = null]) {
    if (tail == null)
      tail = new Nil<A>();
    if (_first == null) {
      return tail;
    } else {
      _last.tail = tail;
      return _first;
    }
  }
}

abstract class _LListBase<A> extends Iterable<A> implements LList<A> {

  void foreach(f(A)) {
    LList<A> it = this;
    while (!it.isNil) {
      Cons<A> cons = it.asCons;
      f(cons.elem);
      it = cons.tail;
    }
  }

  LList map(f(A)) {
    LListBuilder<A> builder = new LListBuilder<A>();
    LList<A> it = this;
    while (it.isCons) {
      Cons<A> cons = it.asCons;
      A elem = cons.elem;
      builder.add(f(elem));
      it = cons.tail;
    }
    return builder.build();
  }

  LList<A> filter(bool f(A)) {
    LListBuilder<A> builder = new LListBuilder<A>();
    LList<A> it = this;
    while (it.isCons) {
      Cons<A> cons = it.asCons;
      A elem = cons.elem;
      if (f(elem)) builder.add(elem);
      it = cons.tail;
    }
    return builder.build();
  }
}

class _NilIterator<A> implements Iterator<A> {
  const _NilIterator();
  A get current => null;
  bool moveNext() => false;
}

class Nil<A> extends _LListBase<A> {
  bool get isNil => true;
  bool get isCons => false;
  Nil<A> get asNil => this;
  Cons<A> get asCons => null;

  toString() => "nil()";

  int get length => 0;

  Iterator<A> get iterator => const _NilIterator();
}

class _ConsIterator<A> implements Iterator<A> {
  final LList<A> _head;
  LList<A> _current = null;

  _ConsIterator(this._head);

  A get current => _current.isCons ? _current.asCons.elem : null;

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

class Cons<A> extends _LListBase<A> {
  int _length = null;

  final A elem;
  LList<A> tail;

  Cons(this.elem, this.tail);

  bool get isNil => false;
  bool get isCons => true;
  Nil<A> get asNil => null;
  Cons<A> get asCons => this;

  toString() => "cons($elem, $tail)";

  int get length {
    if (_length == null) {
      _length = tail.length + 1;
    }
    return _length;
  }

  Iterator<A> get iterator => new _ConsIterator<A>(this);
}
