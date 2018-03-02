// Copyright (c) 2013, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

library option_test;

import 'package:persistent/persistent.dart';
import 'package:unittest/unittest.dart';

none() => new Option.none();
some(x) => new Option.some(x);

main() {
  group('equality', () {
    test('none == none', () => expect(none(), equals(none())));
    test('none != some(42)', () => expect(none(), isNot(equals(some(42)))));
    test('some(42) != none', () => expect(some(42), isNot(equals(none()))));
    test('some(42) == some(42)', () => expect(some(42), equals(some(42))));
    test('some(42) != some(43)',
        () => expect(some(42), isNot(equals(some(43)))));
  });

  group('value', () {
    test('some(42).value == 42', () => expect(some(42).value, equals(42)));
    test(
        'none.value throws StateError',
        () => expect(
            () => none().value, throwsA(new isInstanceOf<StateError>())));
  });

  group('isDefined', () {
    test('some(42) is defined', () => expect(some(42).isDefined, isTrue));
    test('none is not defined', () => expect(some(42).isDefined, isTrue));
  });

  group('asNullable', () {
    test('none.asNullable == null', () => expect(none().asNullable, isNull));
    test('some(42).asNullable == 42',
        () => expect(some(42).asNullable, equals(42)));
  });

  group('orElse', () {
    test('none.orElse(43) == 43', () => expect(none().orElse(43), equals(43)));
    test('some(42).orElse(43) == 42',
        () => expect(some(42).orElse(43), equals(42)));
  });

  group('orElseCompute', () {
    test('none.orElseCompute(() => 43) == 43',
        () => expect(none().orElseCompute(() => 43), equals(43)));
    test(
        'some(42).orElseCompute(() { throw "error"; }) == 42',
        () => expect(
            some(42).orElseCompute(() {
              throw "error";
            }),
            equals(42)));
  });

  group('map', () {
    test(
        'none.map((x) { throw "error"; }) == none',
        () => expect(
            none().map((x) {
              throw "error";
            }),
            equals(none())));
    test('some(42).map((x) => x+1) == some(43)',
        () => expect(some(42).map((x) => x + 1), equals(some(43))));
  });

  group('expand', () {
    test(
        'none.expand((x) { throw "error"; }) == none',
        () => expect(
            none().expand((x) {
              throw "error";
            }),
            equals(none())));
    test('some(42).expand((x) => none) == none',
        () => expect(some(42).expand((x) => none()), equals(none())));
    test('some(42).expand((x) => some(x+1)) == some(43)',
        () => expect(some(42).expand((x) => some(x + 1)), equals(some(43))));
  });

  group('flattened', () {
    test('none.flattened == none',
        () => expect(none().flattened, equals(none())));
    test('some(none).flattened == none',
        () => expect(some(none()).flattened, equals(none())));
    test('some(some(42)).flattened == some(42)',
        () => expect(some(some(42)).flattened, equals(some(42))));
  });

  group('toString', () {
    test('none.toString == "Option.none()"',
        () => expect(none().toString(), equals('Option.none()')));
    test('some(42).toString == "Option.some(42)"',
        () => expect(some(42).toString(), equals('Option.some(42)')));
  });

  group('fromNullable', () {
    test('fromNullable(null) == none',
        () => expect(new Option.fromNullable(null), equals(none())));
    test('fromNullable(42) == some(42)',
        () => expect(new Option.fromNullable(42), equals(some(42))));
  });
}
