import 'package:persistent/persistent.dart';
import 'package:unittest/unittest.dart';
import 'dart:math';
import 'dart:core';

final NUM_OPERATIONS = 10000;

main() {
  test('random_test', () {
    Random r = new Random(4944);

    int next = 1;

    List<TransientMap> transientMaps = [];
    List<PersistentMap> persistentMaps = [new PersistentMap()];
    List<PersistentMap> persistentWithTransient = [new PersistentMap()];
    Map map = {};
    List keys = [];

    List actionsPrep = [{
      'times': 10,
      'prep': prepInsert,
      'call': callInsert,
      'map': insertMap,
      'per': insertPer,
      'tra': insertTra,
      'withTra': insertWithTra
    }, {
      'times': 2,
      'prep': prepDelete,
      'call': callDelete,
      'map': deleteMap,
      'per': deletePer,
      'tra': deleteTra,
      'withTra': deleteWithTra
    }, {
      'times': 7,
      'prep': prepAdjust,
      'call': callAdjust,
      'map': adjustMap,
      'per': adjustPer,
      'tra': adjustTra,
      'withTra': adjustWithTra
    }, {
      'times': 1,
      'prep': prepMap,
      'call': callMap,
      'map': mapMap,
      'per': mapPer,
      'tra': mapTra,
      'withTra': mapWithTra
    }];

    List actions = [];
    actionsPrep.forEach((val) => actions.addAll(new List.filled(val['times'], val)));

    for(int i = 0; i < NUM_OPERATIONS; i++) {
      //print(i);

      int nextRand = r.nextInt(actions.length);
      Map action = actions[nextRand];
      var prep = action['prep'](r, keys);

      map = action['call'](map, action['map'], prep);

      transientMaps =
          transientMaps.map((map) => action['call'](map, action['tra'], prep)).toList();

      persistentMaps =
          persistentMaps.map((map) => action['call'](map, action['per'], prep)).toList();

      persistentWithTransient =
             persistentWithTransient.map((map) => action['call'](map, action['withTra'], prep)).toList();

      if(next == i) {
        next *= 2;
        transientMaps.add(persistentMaps.last.asTransient());
        persistentMaps.add(persistentMaps.last);
        checkAll(map, transientMaps, persistentMaps, persistentWithTransient);
      }
    }
    checkAll(map, transientMaps, persistentMaps, persistentWithTransient);
  });
}

checkAll(Map map, List<TransientMap> tran, List<PersistentMap> per, List<PersistentMap> withTran) {
  /*print(map.length);
  print(map);
  tran.forEach((e) => print(e));
  per.forEach((e) => print(e));*/
  PersistentMap toCompare = new PersistentMap.fromMap(map);
  map.forEach((k,v) {
    expect(v, equals(toCompare[k]));
  });
  per.forEach((PersistentMap m) {
    expect(m.length, equals(toCompare.length));
    expect(m.hashCode, equals(toCompare.hashCode));
    expect(m == toCompare, isTrue);
  });

  tran.map((TransientMap mt) {
    PersistentMap m = mt.asPersistent();
    expect(m.length, equals(toCompare.length));
    expect(m.hashCode, equals(toCompare.hashCode));
    expect(m == toCompare, isTrue);
    return m.asTransient();
  });

  withTran.forEach((PersistentMap m) {
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

insertWithTra(PersistentMap map, key, value) {
  return map.withTransient((TransientMap map) => map.doInsert(key, value));
}

prepDelete(Random r, List keys) {
  if(keys.length == 0) throw 'OCH';

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

deleteWithTra(map, key) {
  return map.withTransient((TransientMap map) => map.doDelete(key));
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
adjustWithTra(map, key, adjust) {
  return map.withTransient((TransientMap map) => map.doAdjust(key, adjust));
}

prepMap(Random r, List keys) {
  return (a) => a - 1;
}
callMap(map, fun, prep) {
  return fun(map, prep);
}
mapMap(Map map, f) {
  var newMap = {};
  map.forEach((k,v) => newMap[k] = f(v));
  return  newMap;
}

mapPer(PersistentMap map, f) {
  return map.mapValues(f);
}

mapTra(TransientMap map, f) {
  return map.doMapValues(f);
}

mapWithTra(PersistentMap map, f) {
  return map.withTransient((TransientMap map) => map.doMapValues(f));
}