import 'package:persistent/persistent.dart';
import 'package:unittest/unittest.dart';
import 'dart:math';
import 'dart:core';

final NUM_OPERATIONS = 5000;
main() {
  test('random_test', () {
    Random r = new Random(47);

    int nextTransient = 1;
    int nextPersistent = 1;
    List<TransientMap> transientMaps = [];
    List<PersistentMap> persistentMaps = [new PersistentMap()];
    Map map = {};
    List keys = [];

    List actionsPrep = [{
      'times': 10,
      'prep': prepInsert,
      'call': callInsert,
      'map': insertMap,
      'per': insertPer,
      'tra': insertTra
    },
    {
      'times': 8,
      'prep': prepDelete,
      'call': callDelete,
      'map': deleteMap,
      'per': deletePer,
      'tra': deleteTra
    },
    {
      'times': 7,
      'prep': prepAdjust,
      'call': callAdjust,
      'map': adjustMap,
      'per': adjustPer,
      'tra': adjustTra
    }];

    List actions = [];
    actionsPrep.forEach((val) => actions.addAll(new List.filled(val['times'], val)));

    for(int i = 0; i < NUM_OPERATIONS; i++) {
      //print(i);
      if(nextTransient == i) {
        nextTransient *= 2;
        transientMaps.add(persistentMaps.last.asTransient());
      }
      if(nextPersistent == i) {
        nextPersistent *= 2;
        persistentMaps.add(persistentMaps.last);
      }
      int nextRand = r.nextInt(actions.length);
      Map action = actions[nextRand];
      var prep = action['prep'](r, keys);

      map = action['call'](map, action['map'], prep);

      transientMaps =
          transientMaps.map((map) => action['call'](map, action['tra'], prep)).toList();

      persistentMaps =
          persistentMaps.map((map) => action['call'](map, action['per'], prep)).toList();

      checkAll(map, transientMaps, persistentMaps);
    }
  });
}

checkAll(Map map, List<TransientMap> tran, List<PersistentMap> per) {
  PersistentMap toCompare = new PersistentMap.fromMap(map);
  map.forEach((k,v) {
    expect(v, equals(toCompare[k]));
  });
  per.forEach((PersistentMap m) {
    expect(m.length, equals(toCompare.length));
    expect(m.hashCode, equals(toCompare.hashCode));
    expect(m == toCompare, isTrue);
  });

  tran.forEach((TransientMap mt) {
    PersistentMap m = mt.asPersistent();
    expect(m.length, equals(toCompare.length));
    expect(m.hashCode, equals(toCompare.hashCode));
    expect(m == toCompare, isTrue);
  });
}


prepInsert(r, List keys) {
  var a = r.nextInt(47474);
  while(keys.contains(a)) a =  r.nextInt(47474);
  keys.add(a);
  return {
    'key': keys.last,
    'val': r.nextInt(4747)
  };
}

callInsert(map, fun, prep) {
  return fun(map, prep['key'], prep['val']);
}
insertMap(map, key, value) {
  map[key] = value;
  return map;
}

insertPer(map, key, value) {
  return map.insert(key, value);
}

insertTra(map, key, value) {
  return map.doInsert(key, value);
}

prepDelete(Random r, List keys) {
  if(keys.length == 0) return null;

  var del =  keys.removeAt(r.nextInt(keys.length));
  return del;
}
callDelete(map, fun, prep) {
  if(prep != null)
    return fun(map, prep);
  else
    return map;
}
deleteMap(Map map, key) {
  map.remove(key);
  return map;
}

deletePer(map, key) {
  return map.delete(key);
}

deleteTra(map, key) {
  return map.doDelete(key);
}

prepAdjust(Random r, List keys) {
  if(keys.length == 0) return null;
  int toAdj = keys[r.nextInt(keys.length)];

  return {
    'key': toAdj,
    'adj': (int a) => a + 47
  };
}
callAdjust(map, fun, prep) {
  if(prep != null)
    return fun(map, prep['key'], prep['adj']);
  else
    return map;
}
adjustMap(Map map, key, adjust) {
  map[key] = adjust(map[key]);
  return map;
}

adjustPer(map, key, adjust) {
  return map.adjust(key, adjust);
}

adjustTra(map, key, adjust) {
  return map.doAdjust(key, adjust);
}