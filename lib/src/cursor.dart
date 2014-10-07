// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

part of persistent;

class Cursor {
  final Reference _ref;
  final PersistentVector _path;

  Cursor(Reference this._ref, Iterable path) :
    _path = (path is PersistentVector)? path: new PersistentVector.from(path);

  Cursor operator[](val) => new Cursor(_ref, conj(_path, val) as PersistentVector);

  deref([notFound = _none]) => getIn(_ref.value, _path, notFound);

  update(f) => _ref.value = updateIn(_ref.value, _path, f);
}