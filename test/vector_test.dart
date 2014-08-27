library vector_test;

import 'package:unittest/unittest.dart';
import 'package:persistent/persistent.dart';
import 'src/test_util.dart';
import 'dart:math';
import 'dart:core';

final NUM_OPERATIONS = 10000;

main() {
  test('random_test', () {
    Random r = new Random(47);
    int next = 1;
    PersistentVector mut = new PersistentVector.empty();
    PersistentVector vector = new PersistentVector.empty();
    ModelVector model = new ModelVector.empty();
    List actions = [];

    List actionsPrep = [{
      'times': 10,
      'prep': prepInsert,
      'call': callInsert,
      'mut': mutInsert,
    }, {
      'times': 8,
      'prep': prepDelete,
      'call': callDelete,
      'mut': mutDelete,
    }, {
      'times': 7,
      'prep': prepSet,
      'call': callSet,
      'mut': mutSet,
    }];
    actionsPrep.forEach((val) => actions.addAll(new List.filled(val['times'], val)));

    for (int i = 0; i < NUM_OPERATIONS; i++) {
      int nextRand = r.nextInt(actions.length);
      Map action = actions[nextRand];
      var prep;
      prep = action['prep'](r, vector);
      vector = action['call'](vector, prep);
      model = action['call'](model, prep);
      mut = action['mut'](mut, prep);
      checker(vector, model);
      checker(vector, mut);
    }
    checker(vector, model);
    checker(mut, model);

    // test conversion
    var c = new PersistentVector.from(vector);
    var d = new PersistentVector.from(model);
    var e = new PersistentVector.from(mut);
    expect(c, orderedEquals(d));
    expect(e, orderedEquals(d));
  });
}

checker(vector, model) {
  var a = new List.from(vector);
  var b = new List.from(model);
  expect (a, orderedEquals(b));
}

prepInsert(Random r, PersistentVector vector) {
  return r.nextInt(47474);
}

prepSet(Random r, PersistentVector vector) {
  var a = r.nextInt(vector.length);
  var b = r.nextInt(47474);
  return {
    'key': a,
    'value': b,
  };
}

prepDelete(Random r, PersistentVector vector) {
  return vector.length != 0;
}

callInsert(PersistentVectorInterface vector, prep) => vector.push(prep);
callSet(PersistentVectorInterface vector, prep) => vector.set(prep['key'], prep['value']);
callDelete(PersistentVectorInterface vector, prep) => prep ? vector.pop() : vector;

mutInsert(PersistentVector vector, prep) => vector.withMutations((vect) => vect.push(prep));
mutSet(PersistentVector vector, prep) => vector.withMutations((vect) => vect.set(prep['key'], prep['value']));
mutDelete(PersistentVector vector, prep) => prep ? vector.withMutations((vect) => vect.pop()) : vector;