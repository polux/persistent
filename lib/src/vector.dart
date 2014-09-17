// Copyright (c) 2014, VacuumLabs.
// Copyright (c) 2012, Google Inc. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// Authors are listed in the AUTHORS file

part of persistent;

const int _SHIFT = 5;
const int _SIZE = 1 << _SHIFT;
const int _MASK = _SIZE - 1;
var _NOT_SET;

_getNotSet() {
  if (_NOT_SET == null)
    _NOT_SET = {};
  return _NOT_SET;
}

// Wrapper for referencing bool values
class _Bool {
  bool value = false;
}


/**
 * A read-only vector, ordered collection of elements of type [K].
 *
 * There is no default implementation of [ReadVector], since it just
 * specifies the common interface of [PersistentVector] and [TransientVector].
 */
abstract class ReadVector<E> implements Iterable<E>, Persistent {

  /**
   * Returns element at given [index].
   *
   * If the [index] is outside the array, [orElse] is called
   * to obtain the return value. Default [orElse] throws
   * [RangeError]
   *
   *     var v = new PersistentVector.from(["Hello","world"]);
   *     v.get(0); // returns "Hello"
   *     v.get(2, ()=>null); // returns null
   *     v.get(2); // throws RangeError
   */
  E get(int index, {Function orElse: null});

  /**
   * Returns element at given [index].
   *
   * Throws [RangeError] if the [index] is outside the array.
   *
   *     var v = new PersistentVector.from(["Hello","world"]);
   *     print(v[0]); // prints "Hello"
   *     print(v[2]); // throws RangeError
   */
  E operator[](int index);

  /// The first element of `this`
  E get first;

  /// The last element of `this`
  E get last;
}


/**
 * A persistent vector, resizable ordered collection of elements of type [K].
 *
 * Persistent data structure is an immutable structure, that provides effective
 * creation of slightly mutated copies.
 */
abstract class PersistentVector<E> implements ReadVector<E> {

  /**
   * Returns a new vector identical to `this` except that
   * element at [index] is [value].
   *
   * Throws [RangeError] if the [index] is outside the array.
   *
   *     var v = new PersistentVector.from(["A","B"]);
   *     v.set(1,":)"); // returns ["A",":)"]
   *     v.set(0,":("); // returns [":(","B"]
   *     v.set(2,":D"); // throws RangeError
   */
  PersistentVector<E> set(int index, E value);

  /**
   * Returns a new vector identical to `this` except that
   * the [value] is appended to its end.
   *
   *     var v = new PersistentVector.from(["one","two"]);
   *     v.push("three"); // returns ["one","two","three"]
   *     v.push("four"); // returns ["one","two","four"]
   */
  PersistentVector<E> push(E value);

  /**
   * Returns a new vector identical to `this` except that
   * the last element is removed.
   *
   * Throws [RangeError] if  `this` is empty
   *
   *     var v = new PersistentVector.from(["one","two"]);
   *     v.pop(); // returns ["one"]
   *     v.pop(); // still returns ["one"]
   *     new PersistentVector.from([]).pop(); // throws RangeError
   */
  PersistentVector<E> pop();

  /**
   * Returns a transient copy of `this`.
   *
   * This is ussualy called to do some changes and
   * then create a new [PersistentVector].
   *
   *     var persistent1 = new PersistentVector.from([1]);
   *     var transient = persistent1.asTransient();
   *     transient.doPush(2);
   *     var persistent2 = new transient.asPersistent();
   */
  TransientVector<E> asTransient();

  /**
   * Creates an empty [PersistentVector] using its default implementation.
   */
  factory PersistentVector() => new _PersistentVectorImpl.empty();

  /**
   * Creates an [PersistentVector] filled by [values]
   * using its default implementation.
   */
  factory PersistentVector.from(Iterable<E> values) => new _PersistentVectorImpl.from(values);

  /**
   * Creates transient copy of `this`, lets it to be modified by [change]
   * and returns persistent result.
   *
   *     var persistent1 = new PersistentVector.from([1,2]);
   *     var persistent2 = persistent1.withTransient((v){
   *       v.doPush(3);
   *     });
   */
  PersistentVector<E> withTransient(void change(TransientVector<E> vect));


  /**
   * The equality operator.
   *
   * Two persistent vectors are equal if and only if they have same lengths,
   * and for each index, the values at it are equal.
   */
  bool operator==(other);

  /*
   * The documentation is inherited from the Object
   */
  int get hashCode;
}


/**
 * A transient vector, resizable ordered collection of elements of type [K].
 *
 * Transient data structure is a mutable structure, that can be effectively
 * converted to the persistent data structure. It is ussualy created from
 * a persistent structure to apply some changes and obtain a new persistent
 * structure.
 */
abstract class TransientVector<E> implements ReadVector<E> {

  /**
   * Sets the element at [index] to be [value].
   *
   * Throws [RangeError] if the [index] is outside the array
   *
   *     var v = new PersistentVector.from(["A","B"]).asTransient();
   *     v.set[1] = ":)"; // v is now ["A",":)"]
   *     v.set[0] = ":("; // v is now [":(",":)"]
   *     v.set[2] = ":D"; // throws RangeError
   *
   */
  void operator []=(int index, E value);

  /**
   * Sets the element at [index] to be [value].
   *
   * Throws [RangeError] if the [index] is outside the array
   *
   *     var v = new PersistentVector.from(["A","B"]).asTransient();
   *     v.doSet(1,":)"); // v is now ["A",":)"]
   *     v.doSet(0,":("); // v is now [":(",":)"]
   *     v.doSet(2,":D"); // throws RangeError
   */
  void doSet(int index, E value);

  /**
   * Appends [value] to the end of `this`.
   *
   *     var v = new PersistentVector.from(["one","two"]).asTransient();
   *     v.doPush("three"); // v is now ["one","two","three"]
   *     v.doPush("four"); // v is now ["one","two","three","four"]
   */
  void doPush(E value);

  /**
   * Removes the last element of `this`.
   *
   * Throws [RangeError] if  `this` is empty
   *
   *     var v = new PersistentVector.from(["one","two"]).asTransient();
   *     v.doPop(); // v is now ["one"]
   *     v.doPop(); // v is now []
   *     v.doPop(); // throws RangeError
   */
  void doPop();

  /**
   * Returns a persistent copy of `this`.
   *
   * This is ussualy called when changes to `this`
   * are finished
   *
   *     var persistent1 = new PersistentVector.from([1]);
   *     var transient = persistent1.asTransient();
   *     transient.doPush(2);
   *     var persistent2 = new transient.asPersistent();
   */
  PersistentVector<E> asPersistent();
}

abstract class _PersistentVectorBase<E> extends IterableBase<E> {
  int _size;

  E _get(int index, {Function orElse: null});

  E get first => _get(0);
  E get last => _get(this.length > 0 ? this.length - 1 : 0);
  int get length => _size;
  Iterator<E> get iterator;
}

abstract class _BaseVectorImpl<E> extends _PersistentVectorBase<E> {
  _Owner _owner;
  _VNode _root;
  _VNode _tail;
  int _level;
  bool __altered = false;

  _BaseVectorImpl._prototype() {
    this._owner = null;
    this._root = new _VNode([], _owner);
    this._tail = _root;
    this._level = _SHIFT;
    this._size = 0;
  }

  Iterator<E> get iterator => new _VectorIterator([this._root, this._tail]);

  E _get(int index, {Function orElse: null}) {
    try {
      index = _checkIndex(index);
    } catch(e) {
      if (orElse == null) {
        throw(e);
      } else {
        return orElse();
      }
    }

    var node = _vectorNodeFor(index);
    var maskedIndex = index & _MASK;
    // if resize ever gets publicly exposed, we need to check if node != null
    return node._get(maskedIndex);
  }

  _BaseVectorImpl<E> _set(int index, E value) {
    index = _checkIndex(index);

    var vector = this;
    var newTail = vector._tail;
    var newRoot = vector._root;
    var didAlter = new _Bool();
    if (index >= _getTailOffset(vector._size)) {
      newTail = newTail._update(vector._owner, 0, index, value, didAlter);
    } else {
      newRoot = newRoot._update(vector._owner, vector._level, index, value, didAlter);
    }
    if (!didAlter.value) {
      return vector;
    }
    if (vector._owner != null) {
      vector._root = newRoot;
      vector._tail = newTail;
      vector.__altered = true;
      return vector;
    }
    return new _PersistentVectorImpl._make(vector._size, vector._level, newRoot, newTail);
  }

  _BaseVectorImpl<E> _push(E value) {
    var len = this.length;
    return this._withTransient((vect) => vect._resize(len+1)._set(len, value));
  }

  _BaseVectorImpl<E> _pop() {
    return this._resize(this.length-1);
  }

  int _getTailOffset(int size) {
    return size < _SIZE ? 0 : (((size - 1) >> _SHIFT) << _SHIFT);
  }

  int _checkIndex(int index) {
    if (index < 0 || index >= _size) {
      throw new RangeError.value(index);
    }
    return index;
  }

  _VNode _vectorNodeFor(int index) {
    if (index >= _getTailOffset(this._size)) {
      return this._tail;
    }
    if (index < 1 << (this._level + _SHIFT)) {
      var node = this._root;
      var level = this._level;
      while (node != null && level > 0) {
        node = node._get((index >> level) & _MASK);
        level -= _SHIFT;
      }
      return node;
    }
    return null;
  }

  _BaseVectorImpl<E> _resize(int end) {
    if (end < 0) {
      throw new RangeError.value(end);
    }
    var owner;
    if (_owner == null) {
      owner = _owner;
    } else {
      owner = new _Owner();
    }
    var oldSize = _size;
    var newSize = end;

    if (oldSize == newSize) return this;
    var newLevel = _level;
    var newRoot = _root;

    var oldTailOffset = _getTailOffset(oldSize);
    var newTailOffset = _getTailOffset(newSize);
    while (newTailOffset >= 1 << (newLevel + _SHIFT)) {
      newRoot = new _VNode(newRoot != null && newRoot.length > 0 ? [newRoot] : [], owner);
      newLevel += _SHIFT;
    }

    var oldTail = _tail;
    var newTail = newTailOffset < oldTailOffset ?
      _vectorNodeFor(newSize - 1) :
        newTailOffset > oldTailOffset ? new _VNode([], owner) : oldTail;

    if (newTailOffset > oldTailOffset && oldSize > 0 && oldTail.length > 0) {
      newRoot = _transientVNode(newRoot, owner);
      var node = newRoot;
      for (var level = newLevel; level > _SHIFT; level -= _SHIFT) {
        var idx = (oldTailOffset >> level) & _MASK;
        node._set(idx , _transientVNode(node._get(idx), owner));
        node = node._get(idx);
      }
      node._set((oldTailOffset >> _SHIFT) & _MASK, oldTail);
    }

    if (newTailOffset < oldTailOffset) {
      newRoot = _transientVNode(newRoot, owner);
      var node = newRoot;
      var parent = null;
      var idx = null;
      for (var level = newLevel; level > _SHIFT; level -= _SHIFT) {
        parent = node;
        idx = (newTailOffset >> level) & _MASK;
        node._set(idx, _transientVNode(node._get(idx), owner));
        node = node._get(idx);
      }
      var newNode = node._removeAfter(owner, node.length - 1);
      if (parent == null) {
        newRoot = newNode;
      } else {
        parent._set(idx, newNode);
      }
    }

    if (newSize < oldSize) {
      newTail = newTail._removeAfter(owner, newSize);
    }

    if (_owner != null) {
      _size = newSize;
      _level = newLevel;
      _root = newRoot;
      _tail = newTail;
      __altered = true;
      return this;
    }
    return new _PersistentVectorImpl._make(newSize, newLevel, newRoot, newTail);

  }

  // TODO: debug funkcia, umazat
  void printInfo() {
    print("Size: $_size");
    print("Level: $_level");
    print("Root: $_root");
    print("Tail: $_tail");
  }

  _BaseVectorImpl _ensureOwner(_Owner ownerID) {
    if (ownerID == this._owner) {
      return this;
    }
    if (ownerID == null) {
      this._owner = ownerID;
      return new _PersistentVectorImpl._make(this._size, this._level, this._root, this._tail);
    }
    return new _TransientVectorImpl._make(this._size, this._level, this._root, this._tail, ownerID);
  }

  _TransientVectorImpl _asTransient() {
    return this._owner != null ? this : this._ensureOwner(new _Owner());
  }

  _PersistentVectorImpl _asPersistent() {
    return this._ensureOwner(null);
  }

  _BaseVectorImpl _withTransient(fn) {
    var transient = this._asTransient();
    fn(transient);
    return transient.wasAltered() ? transient._ensureOwner(this._owner) : this;
  }

}

class _VectorIterator<E> implements Iterator<E> {
  List _array;
  int _index = 0;
  Iterator _current = null;

  _VectorIterator(this._array);
  E get current => (_current != null) ? _current.current : null;

  bool moveNext() {
    while(_index < _array.length) {
      if (_current == null) {
        _current = _array[_index].iterator;
      }
      if (_current.moveNext()) {
        return true;
      } else {
        _current = null;
        _index++;
      }
    }
    return false;
  }
}

class _VNode {
  List _array;
  _Owner _ownerID;

  int get length => _array.length;

  Iterator get iterator {
    if (_array.length == 0) return _array.iterator;
    if (_array[0] is _VNode) return new _VectorIterator(_array);
    return _array.iterator;
  }

  String toString() {
    return "VNode: " + _array.toString();
  }

  _VNode(this._array, this._ownerID);

  void _set(int index, value) {
    if (_array.length > index) {
      _array[index] = value;
    } else if (_array.length == index) {
      _array.add(value);
    } else {
      throw new Exception("Should not happen; ${_array.length} ${index}");
    }
  }

  _get(int index) => (index >= 0 && index < this.length) ? this._array[index] : null;

  _VNode _removeAfter(_Owner ownerID, int newSize) {
    var sizeIndex = (newSize - 1) & _MASK;
    if (newSize != 0 && sizeIndex >= this.length - 1) {
      return this;
    }
    var editable = _transientVNode(this, ownerID);
    if (newSize == 0) {
      editable._array = [];
    } else {
      editable._array.removeRange(sizeIndex + 1, editable.length);
    }
    return editable;

  }

  _VNode _update(ownerID, level, index, value, _Bool didAlter) {
    var deleted = value == _getNotSet();
    var node = this;
    var newNode;
    var idx = (index >> level) & _MASK;
    var nodeHas = node != null && idx < node._array.length;
    if (deleted && !nodeHas) {
      return node;
    }
    if (level > 0) {
      var lowerNode;
      if (node != null && node._array.length > idx) {
        lowerNode = node._array[idx];
      } else {
        lowerNode = null;
      }
      var newLowerNode = lowerNode._update(ownerID, level - _SHIFT, index, value, didAlter);
      if (newLowerNode == lowerNode) {
        return node;
      }
      var newNode = _transientVNode(node, ownerID);
      newNode._set(idx, newLowerNode);
      return newNode;
    }

    if (!deleted && nodeHas && node._array[idx] == value) {
      return node;
    }

    didAlter.value = true;

    newNode = _transientVNode(node, ownerID);
    if (deleted) {
      newNode._set(idx, null);
    } else {
      newNode._set(idx, value);
    }
    return newNode;
  }
}

_VNode _transientVNode(_VNode node, _Owner ownerID) {
  if (ownerID != null && node != null && ownerID == node._ownerID) {
    return node;
  }
  return new _VNode(node != null ? node._array.sublist(0) : [], ownerID);
}

class _PersistentVectorImpl<E> extends _BaseVectorImpl<E> implements PersistentVector<E> {
  // cached hashCode.
  int _hashCode = null;


  factory _PersistentVectorImpl.from(Iterable<E> values) {
    if (values.length == 0) {
      return new _PersistentVectorImpl.empty();
    }
    _PersistentVectorImpl<E> result = new _PersistentVectorImpl.empty();
    result = result.withTransient((vector) {
      values.forEach((E value) {
        vector.doPush(value);
      });
      return vector;
    });
    return result;
  }

  factory _PersistentVectorImpl.empty() => new _PersistentVectorImpl._prototype();
  _PersistentVectorImpl._prototype() : super._prototype();

  factory _PersistentVectorImpl._make(int size, int level, _VNode root, _VNode tail) {
    var x = new _PersistentVectorImpl._prototype();
    x._size = size;
    x._level = level;
    x._root = root;
    x._tail = tail;
    x._owner = null;
    return x;
  }

  int get hashCode {
    if (this._hashCode == null) {
      return hashObjects(this);
    }
    return this._hashCode;
  }

  bool operator==(other) {
    if (other is! _PersistentVectorImpl) return false;
    _PersistentVectorImpl otherVector = other;
    if (this.hashCode != otherVector.hashCode) return false;
    if (this.length != otherVector.length) return false;
    for (int i = 0; i < this.length; i++) {
      if (this._get(i) != otherVector.get(i)) return false;
    }
    return true;
  }

  _PersistentVectorImpl _clear() {
    if (this.length == 0) {
      return this;
    }
    return new _PersistentVectorImpl.empty();
  }

  _TransientVectorImpl asTransient() => _asTransient();
  _PersistentVectorImpl withTransient(fn) => _withTransient(fn);
  _PersistentVectorImpl push(E value) => _push(value);
  _PersistentVectorImpl pop() => _pop();
  _PersistentVectorImpl set(int index, E value) => _set(index, value);
  E get(int index, {Function orElse: null}) => _get(index, orElse: orElse);
  E operator[](int index) => _get(index);
}

class _TransientVectorImpl<E> extends _BaseVectorImpl<E> implements TransientVector<E> {
  _TransientVectorImpl._prototype() : super._prototype();

  factory _TransientVectorImpl._make(int size, int level, _VNode root, _VNode tail, _Owner ownerID) {
    var x = new _TransientVectorImpl._prototype();
    x._size = size;
    x._level = level;
    x._root = root;
    x._tail = tail;
    x._owner = ownerID;
    return x;
  }

  bool wasAltered() {
    return this.__altered;
  }

  _TransientVectorImpl _clear() {
    this._size = 0;
    this._level = _SHIFT;
    this._root = this._tail = null;
    this.__altered = true;
    return this;
  }

  _PersistentVectorImpl asPersistent() => _asPersistent();
  void doPush(E value) {
    _push(value);
  }
  void doPop() {
    _pop();
  }
  E get(int index, {Function orElse: null}) => _get(index, orElse: orElse);
  E operator[](int index) => _get(index);
  void doSet(int index, E value) {
    _set(index, value);
  }
  void operator []=(int index, E value) {
    _set(index, value);
  }
}