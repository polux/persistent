library vector_memory;

import 'package:persistent/persistent.dart';
import 'dart:convert';

List template = [];
List data = new List.filled(200000, null);

var creators = {

  "persistent": () => new PersistentVector.from(template),

  "json": () => JSON.encode(template), 
  
  "list": () => new List.from(template), 
};

void run(int template_size, String mode) {
  
  template.addAll(new List.generate(template_size, (i)=>"$i".padLeft(8)));
  
  int allocated = 0;
  for(bool go = true; go; allocated++){
    try{
      go = false;
      var a = creators[mode]();
      data[allocated] = a;
      go = true;
      print(1024000000.0 / allocated / template_size);
    } catch(e) {
      data = null;
    }
  }
}

main(List<String> args){
  
  run(int.parse(args[0]), args[1]);
}
