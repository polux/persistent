library transient_set_test;

import 'package:unittest/unittest.dart';
import 'package:persistent/persistent.dart';
import 'dart:math';
import 'dart:core';

const int NUM_OPERATIONS = 10000;

main() {
  print('Running test with $NUM_OPERATIONS');
  doTest(NUM_OPERATIONS);
  print('Test successfully finished');

}

List toCheck = ['persistent', 'persistentWithTransient'];
List toCheckWithTransient = ['persistent', 'transient', 'persistentWithTransient'];

assertEqualsPersistent(Map impls, List toCheck) {
  var last;
  toCheck.forEach((name) {
    if(last != null) {
      assert(true);
      assert(last == impls[name]['instance']);
    }
    last = impls[name]['instance'];
  });
}

doTest(operations){
  Random r = new Random(47);

  assertInstancesAreSame(Map impls){
    Map prevInst;
    for(String name in impls.keys){
      Map inst = impls[name];
      if(prevInst != null) {
        assertEquals(prevInst['instance'], inst['instance']);
      }
      prevInst = inst;
    }
  }

  Map impls = {
      'set': {
        'create': () => new Set(),
        'bulkInsert': (Set me, Set updateWith) {
            me.addAll(updateWith);
            return me;
        },
        'bulkDelete': (Set me, List values) {
            values.forEach((v) =>  me.remove(v));
            return me;
        },
        'deepCopy': (Set me) => new Set.from(me)
      },
      'persistent': {
        'create': () => new PersistentSet(),
        'bulkInsert': (PersistentSet me, Set updateWith) =>
            updateWith.fold(me, (act, v) => act.insert(v)),
        'bulkDelete': (PersistentSet me, List values) =>
            values.fold(me, (act, v) => act.delete(v, safe:true)),
        'deepCopy': (PersistentSet me) => me
      },
      'transient': {
        'create': () => new PersistentSet().asTransient(),
        'bulkInsert': (TransientSet me, Set updateWith){
          updateWith.forEach((v) =>  me.doInsert(v));
          return me;
        },
        'bulkDelete': (TransientSet me, List values){
          values.forEach((v) =>  me.doDelete(v, safe:true));
          return me;
        },
        'deepCopy': (TransientSet me) => me.asPersistent()
      },
      'persistentWithTransient': {
        'create': () => new PersistentSet(),
        'bulkInsert': (PersistentSet me, Set updateWith){
          var me2 = me.asTransient();
          updateWith.forEach((v) =>  me2.doInsert(v));
          return me2.asPersistent();
        },
        'bulkDelete': (PersistentSet me, List values){
          var me2 = me.asTransient();
          values.forEach((v) =>  me2.doDelete(v, safe:true));
          return me2.asPersistent();
        },
        'deepCopy': (PersistentSet me) => me
      }
  };

  Map oldImpls = {};

  //list of all implementations we are going to use
  impls.forEach((name, impl){
    oldImpls[name] = {};
    impl['instance'] = impl['create']();
  });


  for(int i=0;i< operations;i++){
    // flip a coin, whether you want to perform bulk insert, or bulk delete
    // generate a random collection of keys(&values) which you want to insert/delete
    // if inserting, insert cca random(0, allValues.length) elements
    // if deleting, delete random(0, keys.length) elements
    // do perform operation on all instances

    if(r.nextBool()) {
      //bulkInsert
      int num = r.nextInt(impls[impls.keys.first]['instance'].length + 47);

      Set set = new Set();

      for(int i=0; i < num; i++) {
        int a = r.nextInt(1000);
        set.add('$a foo $a');
      }

      impls.forEach((name, impl){
        impls[name]['instance'] = impls[name]['bulkInsert'](impl['instance'], set);
      });
    }
    else {
      //bulkDelete
      int num = r.nextInt(impls[impls.keys.first]['instance'].length);

      List keys = [];
      List values = [];
      for(int i=0; i < num; i++) {
        int a = r.nextInt(1000);
        keys.add('$a foo $a');
      }

      impls.forEach((name, impl){
        impls[name]['instance'] = impls[name]['bulkDelete'](impl['instance'], keys);
      });
    }

    assertInstancesAreSame(impls);
    assertInstancesAreSame(oldImpls);

    assertEqualsPersistent(impls, toCheck);
    assertEqualsPersistent(oldImpls, toCheckWithTransient);

    if(r.nextDouble() > 0.5){
      impls.forEach((name, impl){
        oldImpls[name]['instance'] = impl['deepCopy'](impl['instance']);
        if(name == 'transient')
          impls[name]['instance'] = impl['deepCopy'](impl['instance']).asTransient();
      });
    }
  }
}

assertEquals(a, b) {
  if(a == b) return;

  expect(a.length, equals(b.length));
  if(!((a is Set || a is TransientSet || a is PersistentSet) &&
        b is Set || b is TransientSet || b is PersistentSet)) assert(false);

  a.forEach((v){
    assert(b.contains(v));
  });

}
