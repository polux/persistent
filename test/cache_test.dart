library cache_example;

import 'package:persistent/cache.dart';
import 'package:unittest/unittest.dart';
import "package:mock/mock.dart";
import 'dart:async';

class MockCache extends Mock implements Cache {
  noSuchMethod(Invocation invocation) => super.noSuchMethod(invocation);
}

main() {
  group('Cache', () {
    Cache dummy, oneCache, twoCache;
    MockCache mockCache;
    setUp(() {
      dummy = new DummyCache();
      oneCache = new Cache(1);
      twoCache = new Cache(2);
      mockCache = new MockCache();
    });

    test('function is called', () {
      var foo = expectAsync0(() => 'val');
      var cacheFoo = () => cache(id: 'foo', fn: foo);
      expect(cacheFoo(), equals('val'));
    });

    test('function is called with correct cache', () {
      var foo = () => null;
      var cacheFoo = () => cache(id: 'foo', fn: foo);
      mockCache.when(callsTo('call')).alwaysReturn('val');

      expect(do_with_cache(mockCache, cacheFoo), equals('val'));

      mockCache.getLogs(callsTo('call')).verify(happenedOnce);
    });

    test('function is called only once when using same cache', () {
      var foo = expectAsync0(() => 'val');
      var cacheFoo = () => cache(id: 'foo', fn: foo);
      expect(do_with_cache(oneCache, cacheFoo), equals('val'));
      expect(do_with_cache(oneCache, cacheFoo), equals('val'));
    });

    test('function is called twice, when has overflowed', () {
      var foo = expectAsync0(() => 'val', count: 2);
      var bar = expectAsync0(() => 'val');
      var cacheFoo = () => cache(id: 'foo', fn: foo);
      var cacheBar = () => cache(id: 'bar', fn: bar);

      expect(do_with_cache(oneCache, cacheFoo), equals('val'));
      expect(do_with_cache(oneCache, cacheBar), equals('val'));
      expect(do_with_cache(oneCache, cacheFoo), equals('val'));
    });

    test('calling cache during cache call is called with same cache.', () {
      var foo = expectAsync0(() => 'val', count: 1);
      var cacheFoo = () => cache(id: 'foo', fn: foo);

      var bar = expectAsync0(() {
        expect(cache, equals(twoCache));
        return cacheFoo();
      });
      var cacheBar = () => cache(id: 'bar', fn: bar);

      expect(do_with_cache(twoCache, cacheFoo), equals('val')); //puts in cache
      expect(do_with_cache(twoCache, cacheBar), equals('val')); //uses cache for foo
    });

    group('async', () {
      test('calling cache during cache call is called with same cache.', () {
        var foo = expectAsync0(() => 'val', count: 2);
        var cacheFoo = () => cache(id: 'foo', fn: foo);

        var bar = expectAsync0(() => new Future.microtask(() {
          expect(cache, equals(twoCache));
          return cacheFoo();
        }));
        var cacheBar = () => cache(id: 'bar', fn: bar);

        var checkEqualsVal = expectAsync1((val) => expect(val, equals('val')));

        expect(do_with_cache(twoCache, cacheFoo), equals('val')); //puts in cache
        do_with_cache(twoCache, cacheBar).then(checkEqualsVal); //uses cache for foo

        //replaces cache for test, before executing microtask
        expect(do_with_cache(oneCache, cacheFoo), equals('val'));
      });
    });
  });
}
