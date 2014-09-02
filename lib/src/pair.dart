// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

part of persistent;


/**
 * A pair of two values encapsulated to the single object.
 * 
 * Mostly used to represent key-value pairs in dictionaries.
 */
class Pair<A, B> {
  
  /// First value
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
   * Two pairs are equal if and only if both their first and second
   * values are equal.
   */
  bool operator ==(Pair<A, B> other) {
    return (other is Pair<A, B>)
        && fst == other.fst
        && snd == other.snd;
  }

  int get hashCode => fst.hashCode + 31 * snd.hashCode;

  String toString() => "Pair($fst, $snd)";
}
