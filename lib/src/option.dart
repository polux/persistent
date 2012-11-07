// Copyright 2012 Google Inc. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Author: Paul Brauner (polux@google.com)

part of dart_immutable;

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
