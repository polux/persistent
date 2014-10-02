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


// up to 4

_dispach(x, {op:"operation", map, vec, set}) {
  if (x is PersistentMap) {
    if (map != null) return map();
  } else if (x is PersistentVector) {
    if (vec != null) return vec();
  } else if (x is PersistentSet) {
    if (set != null) return set();
  }
  throw new Exception("${x.runtimeType} don't support $op operation");
}

_undefArg(){}

Persistent conj(Persistent coll, arg0, [arg1 = _undefArg, arg2 = _undefArg, arg3 = _undefArg, arg4 = _undefArg, arg5 = _undefArg, arg6 = _undefArg, arg7 = _undefArg, arg8 = _undefArg, arg9 = _undefArg]) {
  var varArgs = [arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9].where((x) => x != _undefArg);
  return into(coll, varArgs);
}

Persistent into(Persistent coll, Iterable elements) {
  return _dispach(coll,
     op: 'conj',
     map:()=>  (coll as PersistentMap).withTransient((TransientMap t) => elements.forEach((arg) => t.doInsert((arg is Pair)? arg.fst : arg.first, (arg is Pair)? arg.snd : arg.last))),
     vec:()=>  (coll as PersistentVector).withTransient((t) => elements.forEach((arg) => t.doPush(arg))),
     set:()=>  (coll as PersistentSet).withTransient((t) => elements.forEach((arg) => t.doInsert(arg)))
  );
}

Persistent assoc(Persistent coll, key0, val0, [
                                  key1 = _undefArg, val1 = _undefArg,
                                  key2 = _undefArg, val2 = _undefArg,
                                  key3 = _undefArg, val3 = _undefArg,
                                  key4 = _undefArg, val4 = _undefArg,
                                  key5 = _undefArg, val5 = _undefArg,
                                  key6 = _undefArg, val6 = _undefArg,
                                  key7 = _undefArg, val7 = _undefArg,
                                  key8 = _undefArg, val8 = _undefArg,
                                  key9 = _undefArg, val9 = _undefArg
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
  argsAll.forEach((a) => (a[0] != _undefArg && a[1] ==_undefArg)? throw new ArgumentError("Key is specified but value is not") : null);
  var varArgs = argsAll.where((x) => x[0]!= _undefArg && x[1] != _undefArg);
  return _dispach(coll,
     op: 'assoc',
     map:()=> into(coll, varArgs),
     vec:()=> (coll as PersistentVector).withTransient((t) => varArgs.forEach((arg) => t[arg[0]] = arg[1]))
  );
}

PersistentMap dissoc(PersistentMap coll, arg0, [arg1 = _undefArg, arg2 = _undefArg, arg3 = _undefArg, arg4 = _undefArg, arg5 = _undefArg, arg6 = _undefArg, arg7 = _undefArg, arg8 = _undefArg, arg9 = _undefArg]) {
  var varArgs = [arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9].where((x) => x != _undefArg);
  return coll.withTransient((TransientMap t) => varArgs.forEach((arg) => t.doDelete(arg, safe: true)));
}


//Maybe should return iterator?? and should take more then Persistant Vector
PersistentVector distinct(PersistentVector coll) {
  var r = [];
  var s = new Set();
  coll.forEach((i) {
    if (!s.contains(i)) {
      r.add(i);
      s.add(i);
    }
  });
  return persist(r);
}

//return emty collection of that type
Persistent empty(Persistent coll) {
  return _dispach(coll,
     op: 'empty',
     map:()=> new PersistentMap(),
     vec:()=> new PersistentVector(),
     set:()=> new PersistentSet()
  );
}

// should map, vector set has this type of function??
bool hasKey(Persistent coll, key) {
  return _dispach(coll,
    op: 'hasKey',
    map:()=> (coll as PersistentMap).containsKey(key),
    vec:()=> key >= 0 && key < (coll as PersistentVector).length,
    set:()=> (coll as PersistentSet).contains(key)
  );
}

dynamic get(Persistent coll, key, [notFound]) {
  return _dispach(coll,
     op: 'get',
     map:()=> coll.lookup(key, orElse: (notFound == null)? null: () => notFound),
     vec:()=> coll.elementAt(key)
  );
  return r;
}

getIn(coll, Iterable keys, [notFound]) {
  var p = persist(coll);
  var r = null;
  keys.forEach((key) {
    if (p != null) {
      p = p[key];
    } else {
      //TODO hmm not found
    }
  });
  return r;
}


Pair find(coll, key, [notFound]) {
  var p = persist(coll);
  return new Pair(key, get(p, key));
}

// throw if dont exist
Persistent assoc_in(coll, keys, val) {
  var p = persist(coll);
  if (keys.length == 1) {
    return assoc(p, keys.first, persist(val));
  } else {
    return assoc(p, keys.first, assoc_in(get(p, keys.first), keys.skip(1), val));
  }
}

// throw if dont exist
Persistent update_in(coll, Iterable keys, f) {
  var p = persist(coll);
  if (keys.length == 1) {
    return assoc(p, keys.first, f(get(p,keys.first)));
  } else {
    return assoc(p, keys.first, update_in(p, keys.skip(1), f));
  }
}

num count(coll) {
  var p = persist(coll);
  var r;
  _dispach(p,
      map:()=> r = p.length,
      vec:()=> r = p.length,
      set:()=> r = p.length
  );
  return r;
}

bool isEmpty(coll) {
  var p = persist(coll);
  var r;
  _dispach(p,
    map:()=> r = p.isEmpty,
    vec:()=> r = p.isEmpty,
    set:()=> r = p.isEmpty
  );
  return r;
}

PersistentMap zipmap(Iterable s0, Iterable s1) {
  var m = {};
  while (s0.isNotEmpty || s1.isNotEmpty) {
    m[s0.first] = s1.first;
    s0 = s0.skip(1);
    s1 = s1.skip(1);
  }
  return persist(m);
}

Iterable reverse(coll) {
  return persist(persist(coll).toList().reversed);
}

//---- Vector Operations ----

PersistentVector subvec(vector, start, [end]) {
  PersistentVector p = persist(vector);
  if (end == null) end = p.length;
  return persist(p.skip(start).take(end-start));
}

//---- Hash Map Operations ----

Iterable keys(map) {
  PersistentMap p = persist(map);
  return p.keys;
}

Iterable values(map) {
  PersistentMap p = persist(map);
  return p.values;
}
