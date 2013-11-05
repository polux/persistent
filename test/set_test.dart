// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

library set_test;

import 'package:propcheck/propcheck.dart';
import 'package:persistent/persistent.dart';
import 'test_src/test_util.dart';

// a unary function on Elements
Element times42(Element e) => new Element(e.i * 42, e.b);

// a predicate on Elements
bool pred(Element e) => e.i % 3 == 0;

testIsEmpty(Set<Element> s) =>
    implemSetFrom(s).isEmpty == modelSetFrom(s).isEmpty;

testEquals(Set<Element> s1, Set<Element> s2) =>
    (implemSetFrom(s1) == implemSetFrom(s2)) == setEquals(s1, s2);

testInsert(Set<Element> s, Element elem) =>
    sameSet(implemSetFrom(s).insert(elem), modelSetFrom(s).insert(elem));

testDelete(Set<Element> s, Element elem) {
  PersistentSet ps = implemSetFrom(s).delete(elem);
  return sameSet(ps, modelSetFrom(s).delete(elem))
    // checks that delete's normalizes the set so that == is well defined
    && implemSetFrom(ps.toSet()) == ps;
}

testContains(Set<Element> s, Element elem) =>
    implemSetFrom(s).contains(elem) == modelSetFrom(s).contains(elem);

testLength(Set<Element> s) =>
    implemSetFrom(s).length == modelSetFrom(s).length;

testUnion(Set<Element> s1, Set<Element> s2) =>
    sameSet(implemSetFrom(s1).union(implemSetFrom(s2)),
            modelSetFrom(s1).union(modelSetFrom(s2)));

testDifference(Set<Element> s1, Set<Element> s2) =>
    sameSet(implemSetFrom(s1).difference(implemSetFrom(s2)),
            modelSetFrom(s1).difference(modelSetFrom(s2)));

testIntersection(Set<Element> s1, Set<Element> s2) =>
    sameSet(implemSetFrom(s1).intersection(implemSetFrom(s2)),
            modelSetFrom(s1).intersection(modelSetFrom(s2)));

testCartesianProduct(Set<Element> s1, Set<Element> s2) =>
    sameSet(implemSetFrom(s1).cartesianProduct(implemSetFrom(s2)),
            modelSetFrom(s1).cartesianProduct(modelSetFrom(s2)));

testIterator(Set<Element> s) =>
    setEquals(implemSetFrom(s).toSet(), modelSetFrom(s).toSet());

testElementAt(Set<Element> s) {
  final expected = implemSetFrom(s);
  int i = 0;
  for (final entry in expected) {
    if (entry != expected.elementAt(i)) return false;
    i++;
  }
  return true;
}

testLast(Set<Element> s) {
  if (s.isEmpty) return true;
  final implem = implemSetFrom(s);
  return implem.last == naiveLast(implem);
}

main(List<String> arguments) {
  final e = new Enumerations();
  final properties = {
    'isEmpty'      : forall(e.sets, testIsEmpty),
    'equals'       : forall2(e.sets, e.sets, testEquals),
    'insert'       : forall2(e.sets, e.elements, testInsert),
    'delete'       : forall2(e.sets, e.elements, testDelete),
    'contains'     : forall2(e.sets, e.elements, testContains),
    'length'       : forall(e.sets, testLength),
    'union'        : forall2(e.sets, e.sets, testUnion),
    'difference'   : forall2(e.sets, e.sets, testDifference),
    'intersection' : forall2(e.sets, e.sets, testIntersection),
    'product'      : forall2(e.sets, e.sets, testCartesianProduct),
    'iterator'     : forall(e.sets, testIterator),
    'elementAt'    : forall(e.sets, testElementAt),
    'last'         : forall(e.sets, testLast)
  };
  testMain(arguments, properties);
}
