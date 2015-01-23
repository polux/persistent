// Copyright (c) 2014, VaccumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

library randomized_vector_test;

import 'package:unittest/unittest.dart';
import 'package:vacuum_persistent/persistent.dart';
import 'dart:core';
import 'utils.dart';

main() {
  run(10000, print_fn: (message) => print(message));
  print('Test successfully finished');
}

run(n, {print_fn}) {
  if (print_fn == null){
    print_fn = (msg) => null;
  }
  doTest(n, print_fn);
}

doTest(operationsCnt, print_fn){

  assertDeeplyEquals(Iterable a, Iterable b) {
    var listA = new List.from(a);
    var listB = new List.from(b);
    expect(listA, orderedEquals(listB));
  }

  assertInstancesAreSame(Map impls) {
    Map prevInst;
    for (String name in impls.keys) {
      Map inst = impls[name];
      if (prevInst != null) {
        assertDeeplyEquals(prevInst['instance'], inst['instance']);
      }
      prevInst = inst;
    }
  }

  Map impls = {
    'persistent': {
      'create': () => new PVec(),
      'bulkInsert': (PVec ve, List updateWith) =>
        updateWith.fold(ve, (ve, e) => ve.push(e)),
      'bulkPop': (PVec ve, int count) =>
        new List.filled(count, null).fold(ve, (ve, e) => ve.pop()),
      'bulkChange': (PVec ve, Map changes) =>
        changes.keys.fold(ve, (ve, key) => ve.set(key, changes[key])),
      'deepCopy': (PVec ve) => ve,
    },
    'model': {
      'create': () => [],
      'bulkInsert': (List ve, List updateWith) =>
        updateWith.fold(ve, (ve, e) => ve.sublist(0)..add(e)),
      'bulkPop': (List ve, int count) =>
        new List.filled(count, null).fold(ve, (ve, e) => ve.sublist(0, ve.length-1)),
      'bulkChange': (List ve, Map changes) =>
        changes.keys.fold(ve.sublist(0), (List ve, key) => ve..removeAt(key)..insert(key, changes[key])),
      'deepCopy': (List ve) => ve.sublist(0),
    },
    'transient': {
      'create': () => new PVec().asTransient(),
      'bulkInsert': (TVec ve, List updateWith) {
        updateWith.forEach((e) => ve.doPush(e));
        return ve;
      },
      'bulkPop': (TVec ve, int count) {
        for (int i = 0; i < count; i++) ve.doPop();
        return ve;
      },
      'bulkChange': (TVec ve, Map changes) {
        changes.forEach((k, v) => ve.doSet(k, v));
        return ve;
      },
      'deepCopy': (TVec ve) => new PVec.from(ve).asTransient(),
    },
    'withTransient': {
      'create': () => new PVec(),
      'bulkInsert': (PVec ve, List updateWith) =>
        ve.withTransient((tv) {
          updateWith.forEach((e) => tv.doPush(e));
        }),
      'bulkPop': (PVec ve, int count) =>
        ve.withTransient((tv) {
          for (int i = 0; i < count; i++) tv.doPop();
        }),
      'bulkChange': (PVec ve, Map changes) =>
        ve.withTransient((tv) {
          changes.forEach((k, v) => tv.doSet(k, v));
        }),
      'deepCopy': (PVec ve) => ve,
    },
  };

  randomlyChangeImpl(m) {
    if (probability(0.1)) {
      if (m is PVec) {
        return m.asTransient();
      } else {
        return m.asPersistent();
      }
    } else {
      return m;
    }
  }

  impl_for(ve) {
    if (ve is TVec) {
      return impls['transient'];
    } else {
      return impls['persistent'];
    }
  }

  impls.addAll({
    'randomlyChangingPersistentTransient': {
      'create': () => new PVec(),
      'bulkInsert': (ve, List updateWith) =>
        randomlyChangeImpl(impl_for(ve)['bulkInsert'](ve, updateWith)),
      'bulkPop': (ve, int count) =>
        randomlyChangeImpl(impl_for(ve)['bulkPop'](ve, count)),
      'bulkChange': (ve, Map changes) =>
        randomlyChangeImpl(impl_for(ve)['bulkChange'](ve, changes)),
      'deepCopy': (ve) => impl_for(ve)['deepCopy'](ve),
    },
  });

  test('Random Vector Test', () {
    Map oldImpls = {};

    impls.forEach((name, impl) {
      oldImpls[name] = {};
      impl['instance'] = impl['create']();
    });

    for (int i = 0; i < operationsCnt; i++) {
      PVec vec = impls['persistent']['instance'];

      if (probability(0.01) || oldImpls['persistent'].isEmpty) {
        print_fn('saving old instances');
        impls.forEach((name, impl) {
          oldImpls[name]['instance'] = impl['deepCopy'](impl['instance']);
        });
      }
      print_fn('$i/$operationsCnt: current length: ${vec.length}');

      assertInstancesAreSame(impls);
      assertInstancesAreSame(oldImpls);

      if (probability(1/3)) {
        // 33% Insert
        int bulkCount = r.nextInt(1000);
        List updateWith = [];
        for (int i = 0; i < bulkCount; i++) {
          updateWith.add(r.nextInt(47474747));
        }
        impls.forEach((name, impl) {
          impls[name]['instance'] = impl['bulkInsert'](impl['instance'], updateWith);
        });
      } else if (probability(1/2)) {
        // 33% Delete
        int maxIndex = impls['persistent']['instance'].length;
        if (maxIndex == 0) continue;
        int bulkCount;
        // sometimes, delete the whole list
        if(probability(0.05)){
          bulkCount = vec.length;
        } else {
          bulkCount = r.nextInt(vec.length);
        }
        impls.forEach((name, impl) {
          impls[name]['instance'] = impl['bulkPop'](impl['instance'], bulkCount);
        });
      } else {
        // 33% Change
        Map updateWith = {};
        int maxIndex = impls['persistent']['instance'].length;
        if (maxIndex == 0) continue;
        int bulkCount = r.nextInt(maxIndex);
        for (int i = 0; i < bulkCount; i++) {
          updateWith[r.nextInt(maxIndex)] = r.nextInt(47474747);
        }
        impls.forEach((name, impl) {
          impls[name]['instance'] = impl['bulkChange'](impl['instance'], updateWith);
        });
      }

      // test iterating, equality and hashCode
      PVec copy = new PVec();
      PVec pv = impls['persistent']['instance'];
      for(var item in pv) {
        copy = copy.push(item);
      }
      expect(pv == copy, isTrue);
      expect(pv.hashCode == copy.hashCode, isTrue);
      PVec not_copy = copy.push('something completely different');
      expect(pv == not_copy, isFalse);
      expect(pv.hashCode == not_copy.hashCode, isFalse);

      // test 'empty'
      num sum = 0;
      for (var impl in impls.keys){
        sum += impls[impl]['instance'].isEmpty?0:1;
      }
      // all impementations must add the same 0 or 1 value to the sum
      expect(sum % impls.length, equals(0));

    }
  });
}