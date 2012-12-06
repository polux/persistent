// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

part of persistent;

class Option<T> {
  final T _value;
  final bool isDefined;

  Option._internal(this.isDefined, this._value);

  factory Option.none() => new Option._internal(false, null);

  factory Option.some(T value) => new Option._internal(true, value);

  T get value {
    if (isDefined) return _value;
    throw "undefined";
  }

  // forall U, Option<U> map(U f(T))
  Option map(f(T)) =>
      isDefined ? new Option.some(f(this.value)) : this;

  bool operator ==(Option<T> other) =>
      (isDefined && other.isDefined && _value == other._value)
   || (!isDefined && !other.isDefined);

  String toString() =>
      isDefined ? "Option.some($_value)" : "Option.none()";
}
