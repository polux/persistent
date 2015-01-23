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

  /// fst value
  final A fst;

  /// Second value
  final B snd;

  /**
   * Creates a new pair of given values
   */
  Pair(this.fst, this.snd);

  /**
   * The equality operator.
   *
   * Two pairs are equal if and only if both their fst and second
   * values are equal.
   */
  bool operator ==(other) {
    return (other is Pair<A, B>)
        && fst == other.fst
        && snd == other.snd;
  }

  operator [](int pos) {
    switch (pos) {
      case 0: return fst;
      case 1: return snd;
      default: throw new RangeError("Pair does not contain value on position $pos");
    }
  }

  A get key => fst;

  B get value => snd;

  int get hashCode => fst.hashCode + 31 * snd.hashCode;

  String toString() => "Pair($fst, $snd)";
}
