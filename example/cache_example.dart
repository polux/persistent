library cache_example;

import 'package:persistent/cache.dart';

main() {
  Cache cache1 = new Cache(2);
  Cache cache2 = new Cache(5);

  var bar = (bar, {foo}) => cache([bar], {#foo: foo}, id: 'fn', fn: f);

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
}

int counter = 0;
f(bar, {foo: 'foo'}) {
  return '$foo $bar ${counter++}';
}