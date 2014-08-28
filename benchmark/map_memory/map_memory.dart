library map_memory;

import 'package:persistent/persistent.dart';
import 'dart:convert';

Map template = {};
List data = new List.filled(100000, null);

var creators = {

  "persistent": () => new PersistentMap.fromMap(template),

  "transient": (){
    var res = new TransientMap();
    template.forEach((k, v) => res.doInsert(k, v));
    return res;
  },
  
  "json": () => JSON.encode(template), 
  
  "map": () => new Map.from(template), 
};

void run(int template_size, String mode) {
  
  for (int i = 0; i < template_size; i++) {
    template["$i".padLeft(8)] = "$i".padRight(8);    
  }
  
  int allocated = 0;
  for(bool go = true; go; allocated++){
    try{
      print(allocated);
      go = false;
      var a = creators[mode]();
      data[allocated] = a;
      go = true;
    } catch(e) {
      data = null;
    }
  }
}

main(List<String> args){
  
  run(int.parse(args[0]), args[1]);
}
