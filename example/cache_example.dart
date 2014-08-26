library cache_example;

import 'package:persistent/cache.dart';
import 'dart:async';

main() {
  Cache cache1 = new Cache(2);
  Cache cache2 = new Cache(5);

  var bar = (bar, {foo}) => cache(args: [bar], kvargs: {#foo: foo}, id: 'fn', fn: f);

  //Calling with defaultCache
  print(bar('bar', foo: 'foo'));

  //Calling with cache cache1
  print(do_with_cache(cache1, () => bar('bar', foo: 'foo')));

  //Calling with cache cache2
  print(do_with_cache(cache2, () => bar('bar', foo: 'foo')));

  //Calling with cache cache1, using cached
  print(do_with_cache(cache1, () => bar('bar', foo: 'foo')));
  print(do_with_cache(cache1, () => bar('bar', foo: 'foo')));

  //Calling with cache cache1
  print(do_with_cache(cache1, () => bar('bar', foo: 'foo2')));
  //Calling with cache cache1, removes old cached values
  print(do_with_cache(cache1, () => bar('bar', foo: 'foo3')));

  //Calling with cache cache1, old was already removed
  print(do_with_cache(cache1, () => bar('bar', foo: 'foo')));

  try {
    var a = do_with_cache(cache1, () => cache(id: '', fn: () => throw new Exception()));
  }
  catch(e,s) {
    print('Catched error');
  }

  Future a = do_with_cache(cache1, () => cache(id: '', fn: () => new Future.microtask(() => throw 5)));
  a.catchError((e) => print('Catched Future error'));
}

int counter = 0;
f(bar, {foo: 'foo'}) {
  return '$foo $bar ${counter++}';
}