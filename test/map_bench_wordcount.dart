import 'dart:io';
import 'dart:async';
import 'package:http/http.dart' as http;
import 'package:persistent/persistent.dart';

add(n, m) => n + m;
insertWord(map, word) => map.insert(word, 1, add);
count(words) => words.fold(new PersistentMap(), insertWord);

final AUSTEEN = 'http://www.gutenberg.org/files/1342/1342.txt';
final DOYLE = 'http://www.gutenberg.org/files/1661/1661.txt';

Future<List<String>> getWords(String url) {
  return http.get(url).then((response) => response.body.split(' '));
}

void run(List<String> doyle, List<String> austeen) {
  final map1 = count(austeen);
  final map2 = count(doyle);
  final result = map1.union(map2, add);
  if (result.length != 36028) {
    throw new StateError("something's wrong");
  }
}

void bench(List<String> doyle, List<String> austeen) {
  // warmup
  run(doyle, austeen);

  final watch = new Stopwatch();
  watch.start();
  for (int i = 0; i < 10; i++) {
    run(doyle, austeen);
  }
  watch.stop();
  print(watch.elapsedMilliseconds / 10);
}

main() {
  Future
      .wait([getWords(AUSTEEN), getWords(DOYLE)])
      .then((result) => bench(result[0], result[1]));
}
