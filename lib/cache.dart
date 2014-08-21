library cache;

import 'dart:collection';
import 'dart:async';

import 'package:persistent/persistent.dart';

Cache get cache => Zone.current[#cache] == null ? defaultCache : Zone.current[#cache];
Cache defaultCache = new DummyCache();

abstract class Cache implements Function {
  factory Cache(int size) =>
      size == 0 ? new DummyCache() : new _CacheImpl(size);

  call(List<dynamic> args, Map<Symbol, dynamic> kvargs, {id, fn});
  get free;
  get size;
}

class _CacheImpl implements Cache {
  // Must be greater than 0
  final int size;

  _CacheImpl(this.size);

  Queue _cacheQueue = new Queue();
  Map _cacheMap = {};


  int get free => size - _cacheQueue.length;

  call(List<dynamic> args, Map<Symbol, dynamic> kvargs, {id, fn}) {
    Function function = fn;
    PersistentSet argsImmutable = new PersistentSet.from(args);
    PersistentMap map = new PersistentMap.fromMap(kvargs);
    map = map.insert('args', argsImmutable);
    map = map.insert('id', id);

    if(!_cacheMap.containsKey(map)) {
      if(_cacheQueue.length == size) {
        _cacheMap.remove(_cacheQueue.removeFirst());
      }
      _cacheMap[map] = Function.apply(function, args, kvargs);
      _cacheQueue.addLast(map);
    }
    return _cacheMap[map];
  }
}

class DummyCache implements Cache {
  DummyCache();

  call(List<dynamic> args, Map<Symbol, dynamic> kvargs, {id, fn}) {
    return Function.apply(fn, args, kvargs);
  }

  int get size => 0;
  int get free => 0;
}

do_with_cache(Cache cache, Function f) {
  var a = new ZoneSpecification();
  return runZoned(() {
    return f();
  }, zoneValues: {#cache: cache});
}