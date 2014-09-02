import 'package:persistent/persistent.dart';
import 'package:unittest/unittest.dart';
import 'dart:math';
import 'dart:core';

final NUM_OPERATIONS = 10000;

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
        assertDeeplyEquals(prevInst['instance'], inst['instance']);
      }
      prevInst = inst;
    }
  }

  Map deepCopyMap(Map map) => new Map.from(map);
  Map impls = {
      'map': {
        'create': () => {},
        'bulkInsert': (Map me, Map updateWith) {
            updateWith.keys.fold(me, (_, k) => me[k] = updateWith[k]);
            return me;
        },
        'bulkDelete': (Map me, List keys) {
            keys.fold(me, (_, k) =>  me.remove(k));
            return me;
        },
        'deepCopy': (Map me) => deepCopyMap(me)
      },
      'persistent': {
        'create': () => new PersistentMap(),
        'bulkInsert': (PersistentMap me, Map updateWith) =>
            updateWith.keys.fold(me, (me, k) => me.insert(k, updateWith[k])),
        'bulkDelete': (PersistentMap me, List keys) =>
            keys.fold(me, (me, k) =>  me.delete(k, safe: true)),
        'deepCopy': (PersistentMap me) => me
      },
      'transient': {
        'create': () => new PersistentMap().asTransient(),
        'bulkInsert': (TransientMap me, Map updateWith) =>
            updateWith.keys.fold(me, (me, k) => me.doInsert(k, updateWith[k])),
        'bulkDelete': (TransientMap me, List keys) =>
            keys.fold(me, (me, k) =>  me.doDelete(k, safe: true)),
        'deepCopy': (TransientMap me) => me.asPersistent()
      },
      'persistentWithTransient': {
        'create': () => new PersistentMap(),
        'bulkInsert': (PersistentMap me, Map updateWith) =>
            me.withTransient((TransientMap me) =>
              updateWith.keys.fold(me, (me, k) => me.doInsert(k, updateWith[k]))),
        'bulkDelete': (PersistentMap me, List keys) =>
            me.withTransient((TransientMap me) =>
              keys.fold(me, (me, k) =>  me.doDelete(k, safe: true))),
        'deepCopy': (PersistentMap me) => me
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

      Map  map = {};

      for(int i=0; i < num; i++) {
        int a = r.nextInt(1000);
        map['$a hello $a'] = '$a world $a';
      }

      impls.forEach((name, impl){
        impls[name]['instance'] = impls[name]['bulkInsert'](impl['instance'], map);
      });
    }
    else {
      //bulkDelete
      int num = r.nextInt(impls[impls.keys.first]['instance'].length);

      List keys = [];
      List values = [];
      for(int i=0; i < num; i++) {
        int a = r.nextInt(1000);
        keys.add('$a hello $a');
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
          impl['instance'] = impl['deepCopy'](impl['instance']).asTransient();
      });
    }
  }
}

assertDeeplyEquals(a, b) {
  if(a == b) return;

  expect(a.length, equals(b.length));
  if(!((a is Map || a is TransientMap || a is PersistentMap) &&
        b is Map || b is TransientMap || b is PersistentMap)) assert(false);

  a.keys.forEach((key) => assertDeeplyEquals(a[key], b[key]));
}
