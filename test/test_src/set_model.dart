// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Author: Paul Brauner (polux@google.com)

part of test_util;

/**
 * Naive implementation of PersistentSet using dart:core [Set]s.
 */
class ModelSet<E> extends PersistentSetBase<E> {
  Set<E> zet;

  ModelSet(this.zet);

  PersistentSet<E> insert(E element) {
    Set<E> newset = new Set<E>.from(zet);
    newset.add(element);
    return new ModelSet(newset);
  }

  PersistentSet<E> delete(E element) {
    Set<E> newset = new Set<E>.from(zet);
    newset.remove(element);
    return new ModelSet(newset);
  }

  PersistentSet<E> union(ModelSet<E> other) {
    Set<E> newset = new Set<E>.from(zet);
    newset.addAll(other.zet);
    return new ModelSet(newset);
  }

  PersistentSet<E> difference(ModelSet<E> other) {
    Set<E> newset = new Set<E>.from(zet);
    newset.removeAll(other.zet);
    return new ModelSet(newset);
  }

  PersistentSet<Pair> cartesianProduct(ModelSet other) {
    Set<Pair> newset = new Set<Pair>();
    for (E e1 in zet) {
      for (final e2 in other.zet) {
        newset.add(new Pair(e1, e2));
      }
    }
    return new ModelSet(newset);
  }

  PersistentSet<E> intersection(PersistentSet<E> other) {
    Set<E> newset = new Set<E>.from(zet);
    return new ModelSet(newset.where((E e) => other.contains(e)).toSet());
  }

  Iterator<E> get iterator => zet.iterator;
}
