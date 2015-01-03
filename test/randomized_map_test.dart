// Copyright (c) 2014, VaccumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

library randomized_map_test;

import 'package:vacuum_persistent/persistent.dart';
import 'package:unittest/unittest.dart';
import 'dart:math';
import 'dart:core';
import 'utils.dart';

final _none = new Object();
final _getNone = () => _none;
bool _isNone(val) => val == _none;

main() {
  run(print_fn: (message) => print(message));
  print('Test successfully finished');
}

run({print_fn}) {
  if (print_fn == null){
    print_fn = (msg) => null;
  }
  doTest(1000, print_fn);
}

doTest(operationsCnt, print_fn){
  Random r = new Random(47);

  assertDeeplyEquals(a, b) {
      expect(a.length, equals(b.length));
      a.keys.forEach((key) => expect(a[key], equals(b[key])));
  }

  // test deepEquality and keys and values iterators
  assertInstancesAreSame(Map impls){
    Map prevInst;
    for(String name in impls.keys){
      Map inst = impls[name];
      if(prevInst != null) {
        assertDeeplyEquals(prevInst['instance'], inst['instance']);
        if (probability(0.1)) {
          expect(prevInst['instance'].keys, unorderedEquals(inst['instance'].keys));
          expect(prevInst['instance'].values, unorderedEquals(inst['instance'].values));
        }
      }
      prevInst = inst;
    }
  }

  Map deepCopyMap(map){
    Map res = new Map();
    fn(k,v){res[k] = v;}
    if (map is Map) {
      map.forEach(fn);
    } else {
      map.forEachKeyValue(fn);
    }
    return res;
  }

  fn_adjust(String a) => '${a} adjusted';

  Map impls = {
      'map': {
        'create': () => {},
        'bulkInsert': (Map me, Map updateWith) {
            updateWith.keys.fold(me, (_, k) => me[k] = updateWith[k]);
            return me;
        },
        'bulkDelete': (Map me, List keys) {
            keys.forEach((k) =>  me.remove(k));
            return me;
        },
        'bulkAdjust': (Map me, List keys, adjust) {
          keys.forEach((k) => me[k] = adjust(me[k]));
          return me;
        },
        'deepCopy': (Map me) => deepCopyMap(me)
      },
      'persistent': {
        'create': () => new PersistentMap(),
        'bulkInsert': (PersistentMap me, Map updateWith) =>
            updateWith.keys.fold(me, (me, k) => me.assoc(k, updateWith[k])),
        'bulkDelete': (PersistentMap me, List keys) =>
            keys.fold(me, (me, k) =>  me.delete(k, missingOk: true)),
        'bulkAdjust': (PersistentMap me, List keys, adjust) =>
            keys.fold(me, (me, k) => me.update(k, adjust)),
        'deepCopy': (PersistentMap me) => me
      },
      // always transient
      'transient': {
        'create': () => new TransientMap(),
        'bulkInsert': (TransientMap me, Map updateWith) =>
            updateWith.keys.fold(me, (me, k) => me.doAssoc(k, updateWith[k])),
        'bulkDelete': (TransientMap me, List keys) =>
            keys.fold(me, (me, k) =>  me.doDelete(k, missingOk: true)),
        'bulkAdjust': (TransientMap me, List keys, adjust) =>
            keys.fold(me, (me, k) => me.doUpdate(k, adjust)),
        'deepCopy': (TransientMap me) {
          TransientMap res = new TransientMap();
          me.forEachKeyValue((k, v) => res.doAssoc(k, v));
          return res;
        }
      },
      // uses transient impl for bulk insert, delete atomicaly
      'persistentWithTransient': {
        'create': () => new PersistentMap(),
        'bulkInsert': (PersistentMap me, Map updateWith) =>
            me.withTransient((TransientMap me) =>
              updateWith.keys.fold(me, (me, k) => me.doAssoc(k, updateWith[k]))),
        'bulkDelete': (PersistentMap me, List keys) =>
            me.withTransient((TransientMap me) =>
              keys.fold(me, (me, k) =>  me.doDelete(k, missingOk: true))),
        'bulkAdjust': (PersistentMap me, List keys, adjust) =>
            me.withTransient((TransientMap me) =>
              keys.fold(me, (me, k) => me.doUpdate(k, adjust))),
        'deepCopy': (PersistentMap me) => me
      },

      'randomlyChangingPersistentTransient': 'will be defined later',
  };

  // helper for randomlyChangingPersistentTransient implementation
  impl_for(map){
    if (map is PersistentMap) return impls['persistent'];
    if (map is TransientMap) return impls['transient'];
    throw new Exception('shouldnt get here');
  }

  randomlyChangeImpl(m){
    if(probability(0.1)){
      if (m is PersistentMap){
        return m.asTransient();
      } else {
        return m.asPersistent();
      }
    } else {
      return m;
    }
  }

  // from time to time randomly change the implementation from persistent to
  // transient and vice versa; may perform multiple bulk operations in one
  // transient state
  impls['randomlyChangingPersistentTransient'] =
      {
        'create': () => new PersistentMap(),
        'bulkInsert': (me, Map updateWith) => randomlyChangeImpl(impl_for(me)['bulkInsert'](me, updateWith)),
        'bulkDelete': (me, List keys) => randomlyChangeImpl(impl_for(me)['bulkDelete'](me, keys)),
        'bulkAdjust': (me, List keys, adjust) => randomlyChangeImpl(impl_for(me)['bulkAdjust'](me, keys, adjust)),
        'deepCopy': (me) => deepCopyMap(me)
      };


  int range = 10000;
  List all_keys = [];
  List all_values = [];

  for (int i=0; i<range; i++){
    all_keys.add('hello $i');
    all_values.add('world $i');
  }
  test('Random Map Test', () {

    Map oldImpls = {};

    //list of all implementations we are going to use
    impls.forEach((name, impl){
      oldImpls[name] = {};
      impl['instance'] = impl['create']();
    });


    for(int i=0;i<operationsCnt;i++){
      // flip a coin, whether you want to perform bulk insert, or bulk delete
      // generate a random collection of keys(&values) which you want to insert/delete
      // do perform operation on all instances

      PersistentMap pm = impls['persistent']['instance'];
      print_fn('$i/$operationsCnt: current length: ${pm.length}');

      if(probability(0.5)) {
        // bulkInsert
        // let's add up to count keys to the map
        int count = r.nextInt((range/10).floor());

        Map  map = {};
        for(int i=0; i < count; i++) {
          map[random_elem(all_keys)] = random_elem(all_values);
        }

        impls.forEach((name, impl){
          impls[name]['instance'] = impls[name]['bulkInsert'](impl['instance'], map);
        });
      }
      else if(probability(0.5)){
        //bulkDelete
        List keys;
        if (probability(0.05)){
          // from time to time, delete the whole map
          keys = new List.from(pm.keys);
        } else {
          int count = r.nextInt(range);
          keys = [];
          for(int i=0; i < count; i++) {
            // sometimes try to delete key which is not there
            keys.add(random_elem(all_keys));
          }
        }
        // perform deletion on each instance
        impls.forEach((name, impl){
          impls[name]['instance'] = impls[name]['bulkDelete'](impl['instance'], keys);
        });
      }
      else {
        // bulkAdjust
        List keys = [];
        List activeKeys = impls['map']['instance'].keys.toList();
        int count = r.nextInt(range);
        // get count from active keys
        if (activeKeys.isNotEmpty) {
          for(int i=0; i < count; i++) {
            keys.add(random_elem(activeKeys));
          }
        }
        impls.forEach((name, impl){
          impls[name]['instance'] = impls[name]['bulkAdjust'](impl['instance'],
              keys, fn_adjust);
        });
      }

      // from time to time, deep-copy all the instances to test immutability
      if(probability(0.01) || oldImpls['persistent'].isEmpty){
        print_fn('saving old instances');
        impls.forEach((name, impl){
          oldImpls[name]['instance'] = impl['deepCopy'](impl['instance']);
        });
      }

      assertInstancesAreSame(impls);
      assertInstancesAreSame(oldImpls);

      // test iterating, equality and hashCode
      PersistentMap copy = new PersistentMap();
      for(Pair p in pm){
        copy = copy.assoc(p.first, p.second);
      }
      expect(pm == copy, isTrue);
      expect(pm.hashCode == copy.hashCode, isTrue);
      PersistentMap not_copy = copy.assoc('something', 'completely different');
      expect(pm == not_copy, isFalse);
      // this may very rarely not be true
      expect(pm.hashCode == not_copy.hashCode, isFalse);

      // test 'containsKey'
      for(int j=0; j<100; j++){
        num sum = 0;
        var elem = random_elem(all_keys);
        for (var impl in impls.keys){
          sum += impls[impl]['instance'].containsKey(elem)?1:0;
        }
        // all impementations must add the same 0 or 1 value to the sum
        expect(sum % impls.length, equals(0));
      }

      // test 'empty'
      num sum = 0;
      for (var impl in impls.keys){
        sum += impls[impl]['instance'].isEmpty?0:1;
      }
      // all impementations must add the same 0 or 1 value to the sum
      expect(sum % impls.length, equals(0));

      // test 'lookup'
      for(int j=0; j<100; j++){
        var last_val = _getNone();
        var elem = random_elem(all_keys);
        for (var impl in impls.keys){
          var val;
          try {
            val = impls[impl]['instance'][elem];
          } catch (_){
            val = null;
          }
          if (!_isNone(last_val)){
            expect(last_val, equals(val));
          }
          last_val = val;
        }
      }
    }
  });
}


