// Copyright (c) 2014, VaccumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

import 'map_test.dart' as map_test;
import 'set_test.dart' as set_test;
import 'vector_test.dart' as vector_test;
import 'randomized_map_test.dart' as randomized_map_test;
import 'randomized_vector_test.dart' as randomized_vector_test;

main() {
  map_test.run();
  set_test.run();
  vector_test.run();
  randomized_map_test.run();
  randomized_vector_test.run();
}