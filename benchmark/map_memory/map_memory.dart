// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

library map_memory;

import 'package:vacuum_persistent/persistent.dart';
import 'dart:collection';

Map template = {};

// here we will store the data to prevent it from garbage collection
List data = [];

var creators = {

  "persistent": () => new PersistentMap.fromMap(template),

  "transient": (){
    var res = new TransientMap();
    template.forEach((k, v) => res.doAssoc(k, v));
    return res;
  },

  "map": () => new Map.from(template),
  "hashmap": () => new HashMap.from(template)
};

void run(int template_size, String mode) {

  for (int i = 0; i < template_size; i++) {
    template["$i".padLeft(8)] = "$i".padRight(8);
  }

  int allocated = 0;
  for(bool go = true; go; allocated++){
    try{
      go = false;
      var a = creators[mode]();
      data.add(a);
      go = true;
      print(1073741824.0 / allocated / template_size);
    } catch(e) {
      data = null;
    }
  }
}

main(List<String> args){

  run(int.parse(args[0]), args[1]);
}
