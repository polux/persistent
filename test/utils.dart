// Copyright (c) 2014, VaccumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

import 'dart:math';

Random r = new Random();
bool probability(num what) => r.nextDouble() < what;
random_elem(List list) => list[r.nextInt(list.length)];