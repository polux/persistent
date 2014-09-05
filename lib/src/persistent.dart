// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

part of persistent;

class Persistent {}
class Owner {}

deepPersistent(from) {
  if(from is Persistent) return from;
  if(from is Map) {
    var map = new PersistentMap();
    return map.withTransient((TransientMap map) {
      from.forEach((key,value) => map.doInsert(key, deepPersistent(value)));
    });
  }
  else if(from is List) {
    from = from.map((e) => deepPersistent(e));
    return new PersistentVector.from(from);
  }
  else {
    return from;
  }
}

final _none = new Object();
final getNone = () => _none;
bool isNone(val) => val == _none;

/**
 * Calls [lookup] recursively using [path] elemenets as keys.
 */
lookupIn(Persistent structure, List path, {orElse()}) =>
    _lookupIn(structure, path.iterator, orElse: orElse);

_lookupIn(dynamic s, Iterator path, {orElse()}) {
  if(!path.moveNext()) return s;
  if(s is PersistentMap) {
    return _lookupIn(s.lookup(path.current, orElse: orElse), path, orElse: orElse);
  }
  else if(s is PersistentVector) {
    return _lookupIn(s.get(path.current, orElse: orElse), path, orElse: orElse);
  }
  else if(s is TransientMap) {
    return _lookupIn(s.lookup(path.current, orElse: orElse), path, orElse: orElse);
  }
  else if(s is TransientVector) {
    return _lookupIn(s.get(path.current, orElse: orElse), path, orElse: orElse);
  }
  else {
    throw new Exception('This should not happen');
  }
}

/**
  * Calls [insert] recursively using [path] elemenets as keys.
  */
Persistent insertIn(Persistent structure, Iterable path, dynamic value) =>
    _insertIn(structure, path.iterator..moveNext(), value);

Persistent _insertIn(s, Iterator path, dynamic value) {
  var current = path.current;
  if(path.moveNext()) { //path continues
    if(s is PersistentMap) {
      return s.insert(current, _insertIn(s.lookup(current), path, value));
    }
    else if(s is PersistentVector) {
      return s.set(current, _insertIn(s.get(current), path, value));
    }
    else if(s is TransientMap) {
      return s.doInsert(current, _insertIn(s.lookup(current), path, value));
    }
    else if(s is TransientVector) {
      return s.doSet(current, _insertIn(s.get(current), path, value));
    }
    else {
      throw new Exception('This should not happen');
    }
  }
  else {
    if(s is PersistentMap) {
      return s.insert(current, value);
    }
    else if(s is PersistentVector) {
      if(current == s.length) {
        return s.push(value);
      }
      return s.set(current, value);
    }
    else if(s is TransientMap) {
      return s.doInsert(current, value);
    }
    else if(s is TransientVector) {
      if(current == s.length) {
        return s.doPush(value);
      }
      return s.doSet(current, value);
    }
    else {
      throw new Exception('This should not happen');
    }
  }
}

/**
 * Calls [delete] recursively using [path] elemenets as keys.
 */
Persistent deleteIn(Persistent structure, List path, {bool safe: false}) =>
    _deleteIn(structure, path.iterator..moveNext(), safe: safe);

Persistent _deleteIn(s, Iterator path, {bool safe: false}) {
  var current = path.current;
  if(path.moveNext()) { //path continues
    if(s is PersistentMap) {
      var deleted = _deleteIn(s.lookup(current), path, safe: safe);
      return s.insert(current, deleted);
    }
    else if(s is PersistentVector) {
      var deleted = _deleteIn(s.get(current), path, safe: safe);
      return s.set(current, deleted);
    }
    else if(s is TransientMap) {
      var deleted = _deleteIn(s.lookup(current), path, safe: safe);
      return s.doInsert(current, deleted);
    }
    else if(s is TransientVector) {
      var deleted = _deleteIn(s.get(current), path, safe: safe);
      return s.doSet(current, deleted);  }
    else {
      throw new Exception('This should not happen');
    }
  }
  else {
    if(s is PersistentMap) {
      return s.delete(current);
    }
    else if(s is PersistentVector) {
      if(s.length - 1 == current) return s.pop();
      else throw new Exception('Cannot delete non last element in PersistentVector');
    }
    else if(s is TransientMap) {
      return s.doDelete(current);
    }
    else if(s is TransientVector) {
      if(s.length - 1 == current) return s.doPop();
      else throw new Exception('Cannot delete non last element in TransientVector');
    }
    else {
      throw new Exception('This should not happen');
    }
  }
  return throw 'It cant get here...';
}
