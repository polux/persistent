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
    PersistentVector vector = new PersistentVector.empty();
    ModelVector model = new ModelVector.empty();
    List actions = [];

    List actionsPrep = [{
      'times': 10,
      'prep': prepInsert,
      'call': callInsert,
    }, {
      'times': 8,
      'prep': prepDelete,
      'call': callDelete,
    }, {
      'times': 7,
      'prep': prepSet,
      'call': callSet,
    }];
    actionsPrep.forEach((val) => actions.addAll(new List.filled(val['times'], val)));

    for (int i = 0; i < NUM_OPERATIONS; i++) {
      //print(i);
      int nextRand = r.nextInt(actions.length);
      Map action = actions[nextRand];
      var prep;
      if (action.containsKey('prep')) {
        prep = action['prep'](r, vector);
      } else {
        prep = null;
      }
      vector = action['call'](vector, prep);
      model = action['call'](model, prep);
      checker(vector, model);
    }
    checker(vector, model);
    var c = new PersistentVector.from(vector);
    var d = new PersistentVector.from(model);
    expect(c, orderedEquals(d));
  });
}

checker(vector, model) {
  //print("checking");
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