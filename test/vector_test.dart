library vector_test;

import 'package:unittest/unittest.dart';
import 'package:persistent/persistent.dart';
import 'src/test_util.dart';
import 'dart:math';
import 'dart:core';

const int OPERATION_COUNT = 10000;

main() {
  test('random_test', () {
    Random r = new Random();

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
        'create': () => new PersistentVector(),
        'bulkInsert': (PersistentVector ve, List updateWith) =>
          updateWith.fold(ve, (ve, e) => ve.push(e)),
        'bulkPop': (PersistentVector ve, int count) =>
          new List.filled(count, null).fold(ve, (ve, e) => ve.pop()),
        'bulkChange': (PersistentVector ve, Map changes) =>
          changes.keys.fold(ve, (ve, key) => ve.set(key, changes[key])),
        'deepCopy': (PersistentVector ve) => ve,
      },
      'model': {
        'create': () => new ModelVector([]),
        'bulkInsert': (ModelVector ve, List updateWith) =>
          updateWith.fold(ve, (ve, e) => ve.push(e)),
        'bulkPop': (ModelVector ve, int count) =>
          new List.filled(count, null).fold(ve, (ve, e) => ve.pop()),
        'bulkChange': (ModelVector ve, Map changes) =>
          changes.keys.fold(ve, (ve, key) => ve.set(key, changes[key])),
        'deepCopy': (ModelVector ve) => ve,
      },
      'transient': {
        'create': () => new PersistentVector().asTransient(),
        'bulkInsert': (TransientVector ve, List updateWith) {
          updateWith.forEach((e) => ve.doPush(e));
          return ve;
        },
        'bulkPop': (TransientVector ve, int count) {
          for (int i = 0; i < count; i++) ve.doPop();
          return ve;
        },
        'bulkChange': (TransientVector ve, Map changes) {
          changes.forEach((k, v) => ve.doSet(k, v));
          return ve;
        },
        'deepCopy': (TransientVector ve) => new PersistentVector.from(ve).asTransient(),
      },
      'withTransient': {
        'create': () => new PersistentVector(),
        'bulkInsert': (PersistentVector ve, List updateWith) =>
          ve.withTransient((tv) {
            updateWith.forEach((e) => tv.doPush(e));
          }),
        'bulkPop': (PersistentVector ve, int count) =>
          ve.withTransient((tv) {
            for (int i = 0; i < count; i++) tv.doPop();
          }),
        'bulkChange': (PersistentVector ve, Map changes) =>
          ve.withTransient((tv) {
            changes.forEach((k, v) => tv.doSet(k, v));
          }),
        'deepCopy': (PersistentVector ve) => ve,
      },
    };

    Map oldImpls = {};

    impls.forEach((name, impl) {
      impl['instance'] = impl['create']();
    });

    bool probability(double what) => r.nextDouble() < what;

    for (int i = 0; i < OPERATION_COUNT; i++) {
      assertInstancesAreSame(impls);
      assertInstancesAreSame(oldImpls);

      if (probability(0.001)) {
        impls.forEach((name, impl) {
          if (!oldImpls.containsKey(name)) {
            oldImpls[name] = {};
          }
          oldImpls[name]['instance'] = impl['deepCopy'](impl['instance']);
        });
      }

      if (probability(1.0/3.0)) {
        // 33% Insert
        int bulkCount = r.nextInt(100);
        List updateWith = [];
        for (int i = 0; i < bulkCount; i++) {
          updateWith.add(r.nextInt(47474747));
        }
        impls.forEach((name, impl) {
          impls[name]['instance'] = impl['bulkInsert'](impl['instance'], updateWith);
        });
      } else if (probability(1.0/2.0)) {
        // 33% Delete
        int maxIndex = impls['persistent']['instance'].length;
        if (maxIndex == 0) continue;
        int bulkCount = r.nextInt(impls['persistent']['instance'].length);
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
    }
  });
}