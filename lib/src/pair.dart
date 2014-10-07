// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

part of persistent;


/**
 * A pair of two values encapsulated to the single object.
 *
 * Mostly used to represent key-value pairs in dictionaries.
 */
class Pair<A, B> {

  /// First value
  final A first;

  /// Second value
  final B second;

  /**
   * Creates a new pair of given values
   */
  Pair(this.first, this.second);

  /**
   * The equality operator.
   *
   * Two pairs are equal if and only if both their first and second
   * values are equal.
   */
  bool operator ==(other) {
    return (other is Pair<A, B>)
        && first == other.first
        && second == other.second;
  }

  operator [](int pos) {
    switch (pos) {
      case 0: return first;
      case 1: return second;
      default: throw new RangeError("Pair does not contain value on position $pos");
    }
  }

  A get key => first;

  B get value => second;

  int get hashCode => first.hashCode + 31 * second.hashCode;

  String toString() => "Pair($first, $second)";
}
