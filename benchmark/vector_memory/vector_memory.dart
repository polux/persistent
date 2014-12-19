// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

library vector_memory;

import 'package:vacuum_persistent/persistent.dart';
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
      print(1073741824.0 / allocated / template_size);
    } catch(e) {
      data = null;
    }
  }
}

main(List<String> args){

  run(int.parse(args[0]), args[1]);
}
