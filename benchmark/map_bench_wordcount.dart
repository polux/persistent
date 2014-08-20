import 'dart:async';
import 'package:http/http.dart' as http;
import 'package:persistent/persistent.dart';

add(n, m) => n + m;
insertWord(map, word) => map.insert(word, 1, add);
count(words) => words.fold(new PersistentMap(), insertWord);

insertWordT(map, word) => map.doInsert(word, 1, add);
countT(words) => words.fold(new PersistentMap().asTransient(), insertWordT);

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

void runT(List<String> doyle, List<String> austeen) {
  final map1 = countT(austeen);
  final map2 = countT(doyle);
  final result = map1.doUnion(map2, add);
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
  print('Persistent ${watch.elapsedMilliseconds / 10}');

  watch.reset();
  watch.start();
  for (int i = 0; i < 10; i++) {
    runT(doyle, austeen);
  }
  watch.stop();
  print('Transient ${watch.elapsedMilliseconds / 10}');
}

main() {
  Future
      .wait([getWords(AUSTEEN), getWords(DOYLE)])
      .then((result) => bench(result[0], result[1]));
}
