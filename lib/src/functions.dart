// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

part of persistent;

//---- Collection Operations ----
// just for testing purpouse
final PersistentMap em = new PersistentMap();
final PersistentVector ev = new PersistentVector();
final PersistentSet es = new PersistentSet();

final TransientMap emt = em.asTransient();
final TransientVector evt = ev.asTransient();
final TransientSet est = es.asTransient();

_dispatch(x, {op:"operation", map, vec, set}) {
  if (x is PersistentMap) {
    if (map != null) return map();
  } else if (x is PersistentVector) {
    if (vec != null) return vec();
  } else if (x is PersistentSet) {
    if (set != null) return set();
  }
  throw new Exception("${x.runtimeType} does not support $op operation");
}

_firstP(p) => p.first;
_secondP(p) => (p is Pair)? p.second : p.last;

/**
 * Returns a new collection which is the result of inserting elements to persistent
 * [coll] ([PersistentMap]/[PersistentSet]/[PersistentVector]).
 * Accepts up to 9 positional elements in one call. If you need to insert
 * more elements, call [into] with [List] of elements.
 * When conjing to a map as element use [List] of length 2 or [Pair].
 * Inserting element to [PersistentSet], which is already presented, does nothing.
 * Inserting element to [PersistentMap] with existing key will overwrite that key.
 *
 * Examples:
 *      PersistentVector pv = persist([1, 2])
 *      conj(pv, 3, 4, 5); // == persist([1, 2, 3, 4, 5])
 *
 *      PersistentMap pm = new PersistentMap();
 *      conj(pm, ['a', 8]); // == persist({'a': 8});
 *      conj(pm, new Pair(['a', 6]), ['b', 10]); // == persist({'a': 6, 'b': 10})
 */
Persistent conj(Persistent coll, arg0, [arg1 = _none, arg2 = _none, arg3 = _none, arg4 = _none, arg5 = _none, arg6 = _none, arg7 = _none, arg8 = _none, arg9 = _none]) {
  var varArgs = [arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9].where((x) => x != _none);
  return into(coll, varArgs);
}

/**
 * Returns a new collection which is the result of inserting all elements of [iter]
 * to persistent [coll] ([PersistentMap]/[PersistentSet]/[PersistentVector]).
 * Inserting element to [PersistentSet], which is already presented, does nothing.
 * Inserting element to [PersistentMap] with existing key will overwrite that key.
 *
 * Examples:
 *      PersistentVector pv = persist([]);
 *      insert(pv, [0, 2, 4]); // == persist([0, 2, 4])
 *
 *      PersistentMap pm1 = persist({'a': 10, 'b': 9});
 *      PersistentMap pm2 = persist({'b': 15, 'c': 7});
 *      insert(pm1, pm2); // persist({'a':10, 'b': 15, 'c': 7})
 *      insert(pm1, [['b', 15], new Pair(c, 7)]); // == persist({'a':10, 'b': 15, 'c': 7});
 *
 *      PersistentSet perSet = new PersistentSet();
 *      into(perSet, [1,2,1,3,2]); // == persist(new Set.from([1, 2, 3]))
 */
Persistent into(Persistent coll, Iterable iter) {
  return _dispatch(coll,
     op: 'into',
     map:()=>  (coll as PersistentMap).withTransient((TransientMap t) => iter.forEach((arg) => t.doAssoc(_firstP(arg), _secondP(arg)))),
     vec:()=>  (coll as PersistentVector).withTransient((t) => iter.forEach((arg) => t.doPush(arg))),
     set:()=>  (coll as PersistentSet).withTransient((t) => iter.forEach((arg) => t.doInsert(arg)))
  );
}
/**
 * Returns a new collection which is the result of inserting new keys and values
 * into indexed peristent [coll] ([PersistentMap]/[PersistentVector]).
 * Accepts up to 9 key:value positional arguments. If you need more arguments use [assocI] with [Iterable].
 *
 * Example:
 *      PersistentMap pm = persist({});
 *      assoc(pm, 'a', 5, 'b', 6); // == persist({'a': 5, 'b': 6})
 *
 *      PersistentVector p = persist([1, 2, 3]);
 *      assoc(pm, 0, 'a', 2, 'b', 0, 'c'); // == persist(['c', 2, 'b'])
 */
Persistent assoc(Persistent coll, key0, val0, [
                                  key1 = _none, val1 = _none,
                                  key2 = _none, val2 = _none,
                                  key3 = _none, val3 = _none,
                                  key4 = _none, val4 = _none,
                                  key5 = _none, val5 = _none,
                                  key6 = _none, val6 = _none,
                                  key7 = _none, val7 = _none,
                                  key8 = _none, val8 = _none,
                                  key9 = _none, val9 = _none
                                 ]) {
  var argsAll = [[key0,val0],
                 [key1,val1],
                 [key2,val2],
                 [key3,val3],
                 [key4,val4],
                 [key5,val5],
                 [key6,val6],
                 [key7,val7],
                 [key8,val8],
                 [key9,val9]];
  argsAll.forEach((a) => (a[0] != _none && a[1] ==_none)?
      throw new ArgumentError("Even number of keys and values is required") : null);
  var varArgs = argsAll.where((x) => x[0]!= _none && x[1] != _none);
  return assocI(coll, varArgs);
}

/**
 * Returns a new collection which is the result of adding all elements of [iter]
 * into indexed peristent [coll] ([PersistentMap]/[PersistentVector]).
 * Elements of [iter] should be [Pair] or [List] with 2 arguments.
 *
 * Example:
 *      PersistentMap pm1 = persist({});
 *      assoc(pm1, [['a', 5], new Pair('b', 6)]); // == persist({'a': 5, 'b': 6})
 *
 *      PersistentVector pm2 = persist([1, 2, 3]);
 *      assoc(pm2, [[0, 'a'], [2, 'b'], [0, 'c']]); // == persist(['c', 2, 'b'])
 */
Persistent assocI(Persistent coll, Iterable iter) {
  return _dispatch(coll,
     op: 'assocI',
     map:()=> into(coll, iter),
     vec:()=> (coll as PersistentVector).withTransient((t) => iter.forEach((arg) => t[_firstP(arg)] = _secondP(arg)))
  );
}

/**
 * Returns a new [PersistentMap] which is the result of removing keys from persistent [coll] ([PersistentMap]).
 * Accepts up to 9 key:value positional arguments. If you need more arguments use [dissocI] with [Iterable].
 *
 * Example:
 *      PersistentMap p = persist({'a': 10, 'b':15, 'c': 17});
 *      dissoc(p, 'c', 'b'); // == persist({'a': 10})
 *      dissoc(p, 'a'); // == persist({'b': 15, 'c': 17})
 */
PersistentMap dissoc(PersistentMap coll, arg0, [arg1 = _none, arg2 = _none, arg3 = _none, arg4 = _none, arg5 = _none, arg6 = _none, arg7 = _none, arg8 = _none, arg9 = _none]) {
  var varArgs = [arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9].where((x) => x != _none);
  return dissocI(coll, varArgs);
}

/**
 * Returns a new [PersistentMap] which is the result of removing all keys in [iter] from persistent [coll] ([PersistentMap]).
 *
 * Example:
 *      PersistentMap p = persist({'a': 10, 'b':15, 'c': 17});
 *      dissocI(p, ['c', 'b']); // == persist({'a': 10})
 *      dissocI(p, ['a']); // == persist({'b': 15, 'c': 17})
 */
PersistentMap dissocI(PersistentMap coll, Iterable iter){
  return coll.withTransient((TransientMap t) => iter.forEach((arg) => t.doDelete(arg, allowMissing: true)));
}

/**
 * Returns a new [PersistentVector] which is the result of removing duplicate elements inside [iter].
 *
 * Example:
 *      PersistentVector p = persist([1, 2, 1, 3, 1, 2]);
 *      distinct(p); // == persist([1, 2, 3])
 */
PersistentVector distinct(Iterable iter) {
  var r = [];
  var s = new Set();
  iter.forEach((i) {
    if (!s.contains(i)) {
      r.add(i);
      s.add(i);
    }
  });
  return persist(r);
}

/**
 * Returns a new empty collection of the same type as [coll] ([PersistentMap]/[PersistentSet]/[PersistentVector]).
 *
 * Example:
 *      PersistentVector pv = persist([1, 2, 3]);
 *      empty(pv); // == persist([])
 *      PersistentMap pv = persist({'a': 10, 'b': 11});
 *      empty(pv); // == persist({})
 */
Persistent empty(Persistent coll) {
  return _dispatch(coll,
     op: 'empty',
     map:()=> new PersistentMap(),
     vec:()=> new PersistentVector(),
     set:()=> new PersistentSet()
  );
}

/**
 * Returns true if persistent [coll] ([PersistentMap]/[PersistentSet]/[PersistentVector]) contains [key].
 * As for [PersistentSet], true is returned if [key] is element of [coll].
 * As for [PersistentVector], true is returned if [key] is correct index in [coll].
 *
 * Example:
 *      PersistantVector pv = persist([1, 2, 5, 7]);
 *      hasKey(pv, 0); // == true
 *      hasKey(pv, 3); // == true
 *      hasKey(pv, 'a'); // == false
 *      hasKey(pv, -1); // == false
 *
 *      PersistantSet ps = persist(new Set.from(['a', 'b']));
 *      hasKey(ps, 'a'); // == true
 *      hasKey(ps, 0); // == false
 *
 *      PersistantMap pm = persist({'a' : 10, 'b': 18});
 *      hasKey(pm, 'a'); // == true
 *      hasKey(pm, 'c'); // == false
 */
bool hasKey(Persistent coll, key) {
  return _dispatch(coll,
    op: 'hasKey',
    map:()=> (coll as PersistentMap).containsKey(key),
    vec:()=> key >= 0 && key < (coll as PersistentVector).length,
    set:()=> (coll as PersistentSet).contains(key)
  );
}

//TODO Sadly, it doesn't throw any specific Exceptions
/**
 * Returns an element of persistent [coll] ([PersistentMap]/[PersistentSet]/[PersistentVector]) stored under
 * [key]. [PersistentVector] takes [key] as index. [PersistentSet] takes [key] as element of that set
 * and returns it.
 * Optional argument [notFound] is returned if [coll] doesn't have that key.
 * If you don't specify [notFound] and [key] is missing in [coll], then [Exception] is thrown.
 * [null] is a valid value for [notFound] and results in same behaviour as dart maps.
 *
 * Example:
 *      PersistantMap pm = persist({'a': 10});
 *      get(pm, 'a'); // == 10
 *      get(pm, 'b'); // throws ...
 *      get(pm, 'b', null); // null
 *
 *      PersistentVector = persist([11, 22, 33]);
 *      get(pv, 1); // == 22
 *      get(pv, -1); // throw ..
 *      get(pv, -1, 10); // == 10
 *
 *      PersistentSet ps = persist(new Set.from(['a', 'b']));
 *      get(ps, 'a'); // == 'a'
 *      get(ps, 'c'); // throw ..
 *      get(ps, 'c', 17); // 17
 */
dynamic get(Persistent coll, key, [notFound = _none]) {
  return (coll as dynamic).get(key, notFound);
}

//TODO Sadly, it doesn't throw any specific Exceptions
/**
 * Returns an element form recursive persistent [coll] under path of keys.
 * [coll] can contain nested [PersistentMap]s, [PersistentVector]s and [PersistentSet]s.
 * Optional argument [notFound] is returned if [coll] doesn't have that key path.
 * If you don't specify [notFound] and key path is missing in [coll], then [Exception] is thrown.
 *
 * Example:
 *      PersistentMap pm = persist({'a': {'b': 10}});
 *      getIn(pm, ['a', 'b']); // == 10
 *      getIn(pm, ['a', 'b', 'c']); // throw
 *      getIn(pm, ['a', 'b', 'c'], null); // null
 *
 *      PersistentVector pv = persist([{'a': 0}, 5]);
 *      getIn(pv, [0, 'a']); // == 0
 *      getIn(pv, [0, 'b']); // throws
 *      getIn(pv, [0, 'b'], 47); // 47
 */
getIn(Persistent coll, Iterable keys, [notFound = _none]) {
  try {
    return _getIn(coll, keys, notFound);
  } catch (e){
    throw new ArgumentError("Key path $keys doesn't exist in $coll, $e");
  }
}

_getIn(Persistent coll, Iterable keys, [notFound = _none]) {
  if (keys.length == 0) return coll;
  if (keys.length == 1) return get(coll, keys.first, notFound);
  return getIn(get(coll, keys.first, persist({})), keys.skip(1), notFound);
}

/**
 * Return [Pair] of [key] and result of [get] ([coll], [key], [notFound]).
 *
 * Example:
 *      PersistentMap pm = persist({'a' : 10});
 *      find(pm, 'a'); // == new Pair('a', 10);
 *      find(pm, 'b'); // throw
 *      find(pm, 'b', 15); // == new Pair('b', 15)
 */
Pair find(Persistent coll, key, [notFound = _none]) {
  return new Pair(key, get(coll, key, notFound));
}

/**
 * Returns a new persistent collection which is the result of [assoc] of [val] under [keys] path in [coll].
 * If key path is not present in collection, then [Exception] is thrown
 *
 *  Example:
 *      PersistentMap pm = persist({'a': [1, 2, 3]});
 *      assocIn(pm, ['a', 0], 17); // == persist({'a': [17, 2, 3]);
 *
 *      PersistenMap pm = persist({'a': {'b': 10}});
 *      assocIn(pm, ['a', 'c'], 17); // == persist({'a': {'b': 10, 'c': 17});
 *      assocIn(pm, ['a', 'c', 'd'], 17); // throws
 */
dynamic assocIn(Persistent coll, Iterable keys, val) {
  try{
    return _assocIn(coll, keys, val);
  } catch (e) {
    throw new Exception("Key path $keys doesn't exist in coll, $e");
  }
}

dynamic _assocIn(Persistent coll, keys, val) {
  if (keys.length == 0) return val;
  if (keys.length == 1) {
    return assoc(coll, keys.first, persist(val));
  } else {
    return assoc(coll, keys.first, _assocIn(get(coll, keys.first), keys.skip(1), val));
  }
}


/**
 * Returns a new persistent collection which is the result of [assoc] of apllied [f] to value under [keys] path in [coll].
 * If only the last key from path is not present, [f] is called without arguments and result is added to return value.
 * If other part of key path is not present in collection, then exception is thrown.
 *
 * Example:
 *      PersistentMap pm = persist({'a': {'b': 10}});
 *      inc(x) => x+1;
 *      maybeInc([x]) => (x != null)? x+1 : 0;
 *      updateIn(pm, ['a', 'b'], inc) // == persist({'a': {'b': 11}})
 *      updateIn(pm, ['a', 'c'], inc) // throws
 *      updateIn(pm, ['a', 'c'], maybeInc) // == persist({'a': {'b': 10, 'c': 0}})
 */
dynamic updateIn(Persistent coll, Iterable keys, Function f) {
  try{
    return _updateIn(coll, keys, f);
  } catch (e) {
    throw new Exception("Key path $keys doesn't exist in coll, $e");
  }
}

dynamic _updateIn(Persistent coll, Iterable keys, f) {
  if (keys.length == 0) return f(coll);
  if (keys.length == 1) {
    return assoc(coll, keys.first, hasKey(coll, keys.first)? f(get(coll,keys.first)) : f());
  } else {
    return assoc(coll, keys.first, _updateIn(get(coll, keys.first), keys.skip(1), f));
  }
}

/**
 * Take iterable [s0] as keys and [s1] as values and return [PersistentMap] constructed from key/value pairs.
 * If they have different length, the longer is trimmed to shorter one.
 *
 * Example:
 *      zipmap(['a', 'b'], [1, 2]); // == persist({'a': 1, 'b': 2})
 *      zipmap(['a', 'b', 'c'], [1, 2, 3, 4, 5]); // == persist({'a': 1, 'b': 2, 'c': 3})
 *      zipmap(['a', 'b', 'c', 'd', 'e'], [1, 2, 3]); // == persist({'a': 1, 'b': 2, 'c': 3})
 */
PersistentMap zipmap(Iterable s0, Iterable s1) {
  var m = {};
  while (s0.isNotEmpty && s1.isNotEmpty) {
    m[s0.first] = s1.first;
    s0 = s0.skip(1);
    s1 = s1.skip(1);
  }
  return persist(m);
}

/**
 * Returns a new [PersistentVector] as subvector from [vector], starting at index [start] inclusive and
 * ending at index [end] excusive. If [end] is not set, the length of [vector] is taken as [end] index.
 * If they overlap, empty vector is returned.
 *
 * Example:
 *      PersistentVector pv = persist([1, 2, 3, 4]);
 *      subvec(pv, 0); // == persist([1, 2, 3, 4])
 *      subvec(pv, 2); // == persist([3, 4])
 *      subvec(pv, 1, 3); // == persist([2, 3])
 *      subvec(pv, -1, 1); // == persist([1])
 *      subvec(pv, 10, -5); // == persist([])
 */
PersistentVector subvec(PersistentVector vector, start, [end]) {
  if (end == null) end = vector.length;
  var numberOfElem = end-start;
  if (numberOfElem < 0) numberOfElem = 0;
  return persist(vector.skip(start).take(numberOfElem));
}

/**
 * Returns the number of elements in [coll] ([PersistentMap]/[PersistentSet]/[PersistentVector]).
 *
 * Example:
 *      PersistentVector pv = persist([1, 2, 3]);
 *      count(pv); // == 3
 *
 *      PersistentMap pm = persist({'a': 10, 'b': 17});
 *      count(pm); // == 2
 *
 *      PersistentSet ps = persist(new Set.from('a'));
 *      count(ps); // == 1
 */
num count(Persistent coll) => (coll as Iterable).length;

/**
 *  Returns whether [coll] ([PersistentMap]/[PersistentSet]/[PersistentVector]) is empty.
 *
 *  Example:
 *      PersistentVector pv = persist([1, 2, 3]);
 *      isEmpty(pv); // == false
 *
 *      PersistentMap pm = persist({});
 *      isEmpty(pm); // == true
 *
 *      PersistentSet ps = persist(new Set.from('a'));
 *      isEmpty(ps); // == false
 */
bool isEmpty(Persistent coll) => (coll as Iterable).isEmpty;

/**
 * Reverse order of iteration on [coll].
 *
 * Example:
 *      PersistentVector pv = persist([1, 2, 3]);
 *      reverse(pv); // == iterable(1, 2, 3)
 */
Iterable reverse(Persistent coll) => persist((coll as Iterable).toList().reversed);

/**
 * Get iterable from keys of [map] ([PersistentMap]).
 *
 * Example:
 *        PersistentMap pm = persist({'a' : 10, 'c': 11});
 *        keys(pm); // == iterable('a', 'b')
 */
Iterable keys(PersistentMap map) => map.keys;

/**
 * Get iterable from values of [map] ([PersistentMap]).
 *
 * Example:
 *        PersistentMap pm = persist({'a' : 10, 'c': 11});
 *        values(pm); // == iterable(10, 11)
 */
Iterable values(PersistentMap map) => map.values;

/**
 * Returns a new [PersistentSet] as result of removing [elem] from [coll].
 * It is an inverted operation to [conj].
 * If element is not in [coll], original [coll] is returned.
 *
 * Example:
 *      PersistentSet ps = persist(new Set.from(['a', 'b']));
 *      disj(ps, 'a'); // ==  persist(new Set.from(['b']));
 */
PersistentSet disj(PersistentSet coll, elem) => coll.delete(elem, allowMissing: true);

/**
 * Returns a new [PersistentSet] as union of [s1] and [s2].
 *
 * Example:
 *      PersistentSet ps1 = persist(new Set.from(['a', 'b']));
 *      PersistentSet ps1 = persist(new Set.from(['b', 'c']));
 *      union(ps1, ps2); // ==  persist(new Set.from(['a', 'b', 'c']));
 */
PersistentSet union(PersistentSet s1, PersistentSet s2) => s1.union(s2);

/**
 * Returns a new [PersistentSet] as intersection of [s1] and [s2].
 *
 * Example:
 *      PersistentSet ps1 = persist(new Set.from(['a', 'b']));
 *      PersistentSet ps1 = persist(new Set.from(['b', 'c']));
 *      intersection(ps1, ps2); // ==  persist(new Set.from(['b']));
 */
PersistentSet intersection(PersistentSet s1, PersistentSet s2) => s1.intersection(s2);

/**
 * Returns a new [PersistentSet] as difference of [s1] and [s2].
 *
 * Example:
 *      PersistentSet ps1 = persist(new Set.from(['a', 'b']));
 *      PersistentSet ps1 = persist(new Set.from(['b', 'c']));
 *      difference(ps1, ps2); // ==  persist(new Set.from(['a']));
 */
PersistentSet difference(PersistentSet s1, PersistentSet s2) => s1.difference(s2);

/**
 * Returns true if [PersistentSet] [s1] is a subset of [s2].
 * Inverse operation is [isSuperset].
 *
 * Example:
 *      PersistentSet ps1 = persist(new Set.from(['a', 'b', 'c']));
 *      PersistentSet ps1 = persist(new Set.from(['b', 'c']));
 *      isSubset(ps2, ps1); // == true
 *      isSubset(ps1, ps2); // == false
 */
bool isSubset(PersistentSet s1, PersistentSet s2) => intersection(s1,s2) == s1;


/**
 * Returns true if [PersistentSet] [s1] is a superset of [s2].
 * Inverse operation is [isSubset].
 *
 * Example:
 *      PersistentSet ps1 = persist(new Set.from(['a', 'b', 'c']));
 *      PersistentSet ps1 = persist(new Set.from(['b', 'c']));
 *      isSubset(ps2, ps1); // == false
 *      isSubset(ps1, ps2); // == true
 */
bool isSuperset(PersistentSet s1, PersistentSet s2) => isSubset(s2, s1);

// ------------------- SEQUENCES ---------------------

first(Iterable i) => i.isEmpty ? null: i.first;

Iterable rest(Iterable i) => i.skip(1);

Iterable seq(dynamic c) {
 if (c is Iterable) {
   return c;
 } else if (c is String){
   return c.split('');
 } else if (c is Map) {
  return persist(c);
 }
 throw new ArgumentError("Can't convert ${c.runtimeType} to Iterable");
}

cons(dynamic val, dynamic coll) => [[val], seq(coll)].expand((x) => x);

concatI(Iterable<dynamic> a) => a.map((e) => seq(e)).expand((x) => x);

concat(arg0, arg1, [arg2 = _none, arg3 = _none, arg4 = _none, arg5 = _none, arg6 = _none, arg7 = _none, arg8 = _none, arg9 = _none])
  => concatI([arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9].where((x) => x != _none));

//flatten ?? what to do on maps? sets?

void forEach(Iterable i, f) => i.forEach(f);

Iterable map(f, Iterable i) => i.map(f);

Iterable mapcatI(f, Iterable<Iterable> ii) => concatI(ii.map((i) => map(f, i)));

mapcat(f, arg0, arg1, [arg2 = _none, arg3 = _none, arg4 = _none, arg5 = _none, arg6 = _none, arg7 = _none, arg8 = _none, arg9 = _none])
  => mapcatI(f, [arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9].where((x) => x != _none));

Iterable filter(pred, Iterable coll) => coll.where(pred);

Iterable remove(pred, Iterable coll) => filter((x) => !x, coll);

/* TODO do we want it as it's in clojure???
  [CLOJURE] f should be a function of 2 arguments. If val is not supplied,
returns the result of applying f to the first 2 items in coll, then
applying f to that result and the 3rd item, etc. If coll contains no
items, f must accept no arguments as well, and reduce returns the
result of calling f with no arguments.  If coll has only 1 item, it
is returned and f is not called.  If val is supplied, returns the
result of applying f to val and the first item in coll, then
applying f to that result and the 2nd item, etc. If coll contains no
items, returns val and f is not called. */
reduce(f, second, [third = _none]) {
  Iterable seq;
  var initialVal = _none;
  if (third == _none) {
    seq = second;
  } else {
    seq = third;
    initialVal = second;
  }
  if (seq.isEmpty) return f();
  if (rest(seq).isEmpty) return first(seq);
  return (initialVal == _none)? rest(seq).fold(first(seq), f) : seq.fold(second, f);
}

//reduce_kv do we want it ???

Iterable take(n, Iterable i) => i.take(n);
Iterable takeWhile(pred, Iterable i) => i.takeWhile(pred);
Iterable drop(n, Iterable i) => i.skip(n);
Iterable dropWhile(n, Iterable i) => i.skipWhile(n);
dynamic some(pred, Iterable i) => i.firstWhere(pred);
bool every(pred, Iterable i) => i.every(pred);

//sort clash with usefull ??
//sort_by clash with usefull ??

Iterable interpose(val, Iterable i) => rest(i.expand((x) => [val, x]));
//interleave(coll1, coll2, ...)
Iterable repeat(Iterable i) => quiver.cycle(i);
