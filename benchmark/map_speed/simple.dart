import 'package:vacuum_persistent/persistent.dart';

main(){
  Stopwatch timer = new Stopwatch()..start();
  var map;
  for(var j=0; j<100;j++){
      map = new PMap();
      for(var i=0; i<3000; i++){
          map = map.assoc(i*i, 'val');
      }
  }
  print(timer.elapsedMilliseconds);
}
