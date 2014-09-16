// Copyright (c) 2014, VaccumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

library linked_list_test;

import 'package:persistent/persistent.dart';
import 'package:unittest/unittest.dart';

main() {
  run();
}

N() => new Nil();
C(e,t) => new Cons(e,t);

run() {

  group("LinkedList", (){
    test("isNil", (){
      expect( N().isNil, isTrue);
      expect( C(0,N()).isNil, isFalse);
    });
    test("isCons", (){
      expect( N().isCons, isFalse);
      expect( C(0,N()).isCons, isTrue);
    });
    test("asNil", (){
      expect( N().asNil, equals(N()));
      expect( C(0,N()).asNil, equals(null));
    });
    test("asCons", (){
      expect( N().asCons, equals(null));
      expect( C(0,N()).asCons, equals(C(0,N())));
    });
    test("length", (){
      expect( N().length, equals(0));
      expect( C(0,N()).length, equals(1));
      expect( C(1,C(0,N())).length, equals(2));
      expect( C(2,C(1,C(0,N()))).length, equals(3));
      expect( C(3,C(2,C(1,C(0,N())))).length, equals(4));

    });
    test("iterator", (){
      // Indirectly using `toList`.
      expect( N().toList(), equals([]));
      expect( C(0,N()).toList(), equals([0]));
      expect( C(1,C(0,N())).toList(), equals([1,0]));
      expect( C(2,C(1,C(0,N()))).toList(), equals([2,1,0]));
      expect( C(3,C(2,C(1,C(0,N())))).toList(), equals([3,2,1,0]));
    });
    test("==", (){
      expect( N() == N(), isTrue);
      expect( C(0,N()) == C(0,N()), isTrue);
      expect( C(1,C(0,N())) == C(1,C(0,N())), isTrue);

      expect( N() == C(0,N()), isFalse);
      expect( C(0,N()) == N(), isFalse);
      expect( C(0,N()) == C(0,C(1,N())), isFalse);
      expect( C(1,N()) == C(0,N()), isFalse);
    });
  });

  group("Cons", (){
    test("elem", (){
      expect( C(0,N()).elem, equals(0));
    });
    test("tail", (){
      expect( C(0,N()).tail, equals(N()));
    });
  });

  group("LinkedListBuilder", (){
      test("basic", (){
        LinkedListBuilder b = new LinkedListBuilder();
        b.add(0);
        b.addAll([1,2]);
        b.add(3);
        b.addAll([]);
        expect(b.build(), equals(C(0,C(1,C(2,C(3,N()))))));
        expect(b.build(C(4,N())), equals(C(0,C(1,C(2,C(3,C(4,N())))))));
      });
    });
}
