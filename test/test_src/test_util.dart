// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

library test_util;

import 'dart:math';
import 'dart:io';

import 'package:args/args.dart' as args;
import 'package:propcheck/propcheck.dart';
import 'package:enumerators/combinators.dart' as c;
import 'package:enumerators/enumerators.dart' as en;
import 'package:persistent/persistent.dart';
import 'package:unittest/unittest.dart';

part 'map_model.dart';
part 'set_model.dart';

/**
 / A datatype with an imperfect hash function to use as a key for testing maps.
 */
class Key {
  // a list of of 7 integers in [0..31] and 1 integer in [0..3]
  final List<int> key;
  final bool b;
  final int hashCode;

  static int computeHashCode(key) {
    int result = 0;
    for(int i = 0; i < 7; i++) {
      result |= key[i] << (5 * i);
    }
    return result;
  }

  Key(List<int> key, bool b)
      : this.key = key
      , this.b = b
      , this.hashCode = computeHashCode(key) {
    assert(key.length == 7);
  }

  static bool _eqList(List xs, List ys) {
    if (xs.length != ys.length) return false;
    for (int i = 0; i < xs.length; i++) {
      if (xs[i] != ys[i]) return false;
    }
    return true;
  }

  bool operator ==(Key other) {
    if (other is! Key) return false;
    return _eqList(key, other.key) && b == other.b;
  }

  toString() => "Key($key, $b)";
}

/**
 * A datatype with an imperfect hash function to use as an element for testing
 * sets.
 */
class Element {
  final int i;
  final bool b;
  Element(this.i, this.b);
  int get hashCode => i.hashCode;
  bool operator ==(other) =>
    (other is Element)
    && i == other.i
    && b == other.b;
  String toString() => "Element($i, $b)";
}

/**
 * Enumerations of [Key], [Element], [Map<Key, int>] and [Set<Element>].
 */
class Enumerations {
  en.Enumeration<Key> keys;
  en.Enumeration<Element> elements;
  en.Enumeration<int> values;
  en.Enumeration<Map<Key, int>> maps;
  en.Enumeration<Set<Element>> sets;

  static _list7(x1) => (x2) => (x3) => (x4) => (x5)  => (x6) => (x7) =>
      [x1, x2, x3, x4, x5, x6, x7];

  Enumerations() {
    // [{0}, {1}, ..., {31}]
    var smallints = en.empty();
    for (int i = 0; i < 32; i++) {
      var n = en.singleton(i);
      for (int s = 0; s < i; s++) { n = n.pay(); }
      smallints += n;
    }
    // [{0}, {1}, {2}, {3}]
    var twoBits = en.empty();
    for (int i = 0; i < 3; i++) {
      var n = en.singleton(i);
      for (int s = 0; s < i; s++) { n = n.pay(); }
      twoBits += n;
    }
    final smalllists = en.singleton(_list7)
        .apply(smallints).apply(smallints).apply(smallints)
        .apply(smallints).apply(smallints).apply(smallints)
        .apply(twoBits);
    keys = en.singleton((k) => (b) => new Key(k, b))
             .apply(smalllists)
             .apply(c.bools);
    values = c.ints + new en.Enumeration.singleton(null);
    maps = c.mapsOf(keys, values);
    elements = en.singleton((i) => (b) => new Element(i, b))
                 .apply(c.ints)
                 .apply(c.bools);
    sets = c.setsOf(elements);
  }
}

// Copied from Dart's Iterable
naiveLast(Iterable iterable) {
  Iterator it = iterable.iterator;
  if (!it.moveNext()) {
    throw new StateError("No elements");
  }
  var result;
  do {
    result = it.current;
  } while(it.moveNext());
  return result;
}

PersistentMap implemMapFrom(Map m) => new PersistentMap.fromMap(m);
ModelMap modelMapFrom(Map m) => new ModelMap(m);

PersistentSet implemSetFrom(Set s) => new PersistentSet.from(s);
ModelSet modelSetFrom(Set s) => new ModelSet(s);

class _Stop implements Exception {}

bool mapEquals(Map m1, Map m2) {
  if (m1.length != m2.length) return false;
  try {
    m1.forEach((k, v) {
      if (!m2.containsKey(k)) throw new _Stop();
      if (v != m2[k]) throw new _Stop();
    });
  } on _Stop catch(e) {
    return false;
  }
  return true;
}

bool setEquals(Set s1, Set s2) {
  if (s1.length != s2.length) return false;
  try {
    s1.forEach((e) {
      if (!s2.contains(e)) throw new _Stop();
    });
  } on _Stop catch(e) {
    return false;
  }
  return true;
}

bool sameMap(PersistentMap pm, ModelMap mm) => mapEquals(pm.toMap(), mm._map);
bool sameSet(PersistentSet ps, ModelSet ms) => setEquals(ps.toSet(), ms.zet);

bool sameIterator(Iterator it1, Iterator it2) {
  while (it1.moveNext()) {
    if (!it2.moveNext()) return false;
    if (it1.current != it2.current) return false;
  }
  return !it2.moveNext();
}

void testMain(Map<String, Property> properties) {
  final parser = new args.ArgParser();
  parser.addFlag('help', negatable: false);
  parser.addFlag('quiet', negatable: false);
  parser.addOption('quickCheckMaxSize', defaultsTo: '300');
  parser.addOption('smallCheckDepth', defaultsTo: '15');
  parser.addOption('property',
                   help: 'property to test or "all"',
                   allowed: new List.from(properties.keys)..add('all'),
                   allowMultiple: true,
                   defaultsTo: 'all');
  final flags = parser.parse(new Options().arguments);

  if (flags['help']) {
    print(parser.getUsage());
    return;
  }

  final qc = new QuickCheck(
      maxSize: int.parse(flags['quickCheckMaxSize']),
      quiet: flags['quiet']);
  final sc = new SmallCheck(
      depth: int.parse(flags['smallCheckDepth']),
      quiet: flags['quiet']);

  Iterable<String> toTest = flags['property'];
  if (toTest.contains('all')) {
    toTest = properties.keys;
  }

  group('quickcheck', () {
    for (final property in toTest) {
      test(property, () => qc.check(properties[property]));
    }
  });
  group('smallcheck', () {
    for (final property in toTest) {
      test(property, () => sc.check(properties[property]));
    }
  });
}
