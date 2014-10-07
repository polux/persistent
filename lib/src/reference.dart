// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

part of persistent;

class Reference<V> {
  V _value;
  StreamController<dynamic> _onChangeController, _onChangeSyncController;

  Reference([defVal = null]) : _value = defVal;

  Stream get onChange {
    if (_onChangeController == null) _onChangeController = new StreamController.broadcast(sync: false);
    return _onChangeController.stream;
  }

  Stream get onChangeSync {
    if (_onChangeSyncController == null) _onChangeSyncController = new StreamController.broadcast(sync: true);
    return _onChangeSyncController.stream;
  }

  get value => _value;

  set value(val) {
     var change = {'oldVal': _value, 'newVal': val};
     _value = val;
     if (_onChangeSyncController != null) _onChangeSyncController.add(change);
     if (_onChangeController != null) _onChangeController.add(change);
  }

  Cursor get cursor => new Cursor(this, []);

  dispose() {
    if (_onChangeSyncController != null) _onChangeSyncController.close();
    if (_onChangeController != null) _onChangeSyncController.close();
  }

  String toString() => 'Ref(${_value.toString()})';
}
