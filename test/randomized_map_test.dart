import 'package:persistent/persistent.dart';
import 'package:unittest/unittest.dart';
import 'dart:math';
import 'dart:core';
import 'utils.dart';


main() {
  doTest(10000, (message) => print(message));
  print('Test successfully finished');
}

doTest(operationsCnt, print_fn){
  Random r = new Random(47);

  assertDeeplyEquals(a, b) {
    if(a is! Map && a is! TransientMap && a is! PersistentMap){
      return a==b;
    } else {
      expect(a.length, equals(b.length));
      a.keys.forEach((key) => assertDeeplyEquals(a[key], b[key]));
    }
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
      // always transient
      'transient': {
        'create': () => new PersistentMap().asTransient(),
        'bulkInsert': (TransientMap me, Map updateWith) =>
            updateWith.keys.fold(me, (me, k) => me.doInsert(k, updateWith[k])),
        'bulkDelete': (TransientMap me, List keys) =>
            keys.fold(me, (me, k) =>  me.doDelete(k, safe: true)),
        'deepCopy': (TransientMap me) {
          TransientMap res = new TransientMap();
          me.forEachKeyValue((k, v) => res.doInsert(k, v));
          return res;
        }
      },
      // uses transient impl for bulk insert, delete atomicaly
      'persistentWithTransient': {
        'create': () => new PersistentMap(),
        'bulkInsert': (PersistentMap me, Map updateWith) =>
            me.withTransient((TransientMap me) =>
              updateWith.keys.fold(me, (me, k) => me.doInsert(k, updateWith[k]))),
        'bulkDelete': (PersistentMap me, List keys) =>
            me.withTransient((TransientMap me) =>
              keys.fold(me, (me, k) =>  me.doDelete(k, safe: true))),
        'deepCopy': (PersistentMap me) => me
      },
  };

  // some helper helper fns for persistentSlashTransient
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
  impls.addAll({
      'randomlyChangingPersistentTransient': {
        'create': () => new PersistentMap(),
        'bulkInsert': (me, Map updateWith) => randomlyChangeImpl(impl_for(me)['bulkInsert'](me, updateWith)),
        'bulkDelete': (me, List keys) => randomlyChangeImpl(impl_for(me)['bulkDelete'](me, keys)),
        'deepCopy': (me) => deepCopyMap(me)
      },
  });


  int range = 10000;
  List all_keys = [];
  List all_values = [];

  for (int i=0; i<range; i++){
    all_keys.add('hello $i');
    all_values.add('world $i');
  }

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
    print_fn('current length: ${pm.length}');

    if(probability(0.7)) {
      // bulkInsert
      // let's add some fixed percentage of all keys to the map
      int num = r.nextInt((range/10).floor());

      Map  map = {};
      for(int i=0; i < num; i++) {
        map[random_elem(all_keys)] = random_elem(all_values);
      }

      impls.forEach((name, impl){
        impls[name]['instance'] = impls[name]['bulkInsert'](impl['instance'], map);
      });
    }
    else {
      //bulkDelete
      List keys;
      if (probability(0.05)){
        // from time to time, delete the whole map
        keys = new List.from(pm.keys);
      } else {
        int num = r.nextInt(range);
        keys = [];
        for(int i=0; i < num; i++) {
          // sometimes try to delete key which is not there
          keys.add(random_elem(all_keys));
        }
      }
      // perform deletion on each instance
      impls.forEach((name, impl){
        impls[name]['instance'] = impls[name]['bulkDelete'](impl['instance'], keys);
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
      copy = copy.insert(p.fst, p.snd);
    }
    expect(pm == copy, isTrue);
    expect(pm.hashCode == copy.hashCode, isTrue);
    PersistentMap not_copy = copy.insert('something', 'completely different');
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

    // test 'lookup'
    for(int j=0; j<100; j++){
      var last_val = getNone();
      var elem = random_elem(all_keys);
      for (var impl in impls.keys){
        var val;
        try {
          val = impls[impl]['instance'][elem];
        } catch (_){
          val = null;
        }
        if (!isNone(last_val)){
          expect(last_val, equals(val));
        }
        last_val = val;
      }
    }

  }
}


