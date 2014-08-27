part of persistent;

const int _SHIFT = 5;
const int _SIZE = 1 << _SHIFT;
const int _MASK = _SIZE - 1;
var _NOT_SET;

getNotSet() {
  if (_NOT_SET == null)
    _NOT_SET = {};
  return _NOT_SET;
}

// Wrapper for referencing bool values
class Bool {
  bool value = false;
}

class Owner {}

abstract class PersistentVectorInterface<E> {
  E get(int index, [E notSetValue = null]);
  PersistentVectorInterface<E> set(int index, E value);
  E get first;
  E get last;

  PersistentVectorInterface<E> delete(int index);
  String toString();
  PersistentVectorInterface<E> push(E value);
  PersistentVectorInterface<E> pop();
  PersistentVectorInterface<E> from(Iterable<E> values);
}

abstract class PersistentVectorBase<E> extends IterableBase<E> implements PersistentVectorInterface<E> {
  int _size;

  E get first => get(0);
  E get last => get(this.length ? this.length - 1 : 0);
  int get length => _size;
  Iterator<E> get iterator => new PersistentVectorIterator<E>(this);
}

class PersistentVectorIterator<E> extends Iterator<E> {
  PersistentVectorBase<E> _parentVector;
  int _position = -1;
  int _length;

  PersistentVectorIterator(this._parentVector) {
    _length = _parentVector.length;
  }

  bool moveNext() {
    if (_length == _position + 1) return false;
    _position++;
    return true;
  }

  E get current => _parentVector.get(_position);
}

class PersistentVector<E> extends PersistentVectorBase<E> {
  int _origin;
  Owner __ownerID;
  VNode _root;
  VNode _tail;
  int _level;
  // cached hashCode.
  int _hashCode = null;
  bool __altered = false;

  factory PersistentVector.from(Iterable<E> values) {
    if (values.length == 0) {
      return new PersistentVector.empty();
    }
    PersistentVector<E> result = new PersistentVector.empty();
    values.forEach((E value) {
      result = result.push(value);
    });
    return result;
  }

  factory PersistentVector.empty() {
    var x = new PersistentVector._prototype();
    return x;
  }

  PersistentVector._prototype() {
    this._origin = 0;
    this.__ownerID = null;
    this._root = new VNode([], __ownerID);
    this._tail = _root;
    this._level = _SHIFT;
    this._size = 0;
  }

  factory PersistentVector._make(int origin, int size, int level, VNode root, VNode tail, [Owner ownerID = null, int hashCode = null]) {
    var x = new PersistentVector._prototype();
    x._origin = origin;
    x._size = size;
    x._level = level;
    x._root = root;
    x._tail = tail;
    x.__ownerID = ownerID;
    return x;
  }

  E get(int index, [E notSetValue = null]) {
    index = _checkIndex(index);
    if (index >= this._size) {
      return notSetValue;
    }
    var node = _vectorNodeFor(index);
    var maskedIndex = index & _MASK;
    return (node != null && (notSetValue == null || node._array.length > maskedIndex)) ?
      node._array[maskedIndex] : notSetValue;
  }

  PersistentVector<E> set(int index, E value) {
    if (index >= this.length) {
      if (value == getNotSet())
        return this;
      return this.withMutations((vect) => vect._resize(index+1).set(index,value));
    }

    var vector = this;
    var newTail = vector._tail;
    var newRoot = vector._root;
    var didAlter = new Bool();
    if (index >= _getTailOffset(vector._size)) {
      newTail = _updateVNode(newTail, vector.__ownerID, 0, index, value, didAlter);
    } else {
      newRoot = _updateVNode(newRoot, vector.__ownerID, vector._level, index, value, didAlter);
    }
    if (!didAlter.value) {
      return vector;
    }
    if (vector.__ownerID != null) {
      vector._root = newRoot;
      vector._tail = newTail;
      vector.__altered = true;
      return vector;
    }
    return new PersistentVector._make(vector._origin, vector._size, vector._level, newRoot, newTail);
  }

  PersistentVector<E> push(E value) {
    var len = this.length;
    return this.withMutations((vect) => vect._resize(len+1).set(len, value));
  }

  PersistentVector<E> pushAll(List<E> values) {
    var t = this._resize(this.length + values.length);
    for (int i = 0; i < values.length; i++) {
      t = t.set(this.length+i, values[i]);
    }
    return t;
  }

  PersistentVector<E> pop() {
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

  VNode _vectorNodeFor(int index) {
    if (index >= _getTailOffset(this._size)) {
      return this._tail;
    }
    if (index < 1 << (this._level + _SHIFT)) {
      var node = this._root;
      var level = this._level;
      while (node != null && level > 0) {
        node = node._array[(index >> level) & _MASK];
        level -= _SHIFT;
      }
      return node;
    }
  }

  PersistentVector<E> _resize(int end) {
    var owner;
    if (__ownerID == null) {
      owner = __ownerID;
    } else {
      owner = new Owner();
    }
    //var owner = __ownerID || new Owner();
    var oldSize = _size;
    var newSize = end;

    if (oldSize == newSize) return this;
    var newLevel = _level;
    var newRoot = _root;

    var oldTailOffset = _getTailOffset(oldSize);
    var newTailOffset = _getTailOffset(newSize);
    while (newTailOffset >= 1 << (newLevel + _SHIFT)) {
      newRoot = new VNode(newRoot != null && newRoot._array.length > 0 ? [newRoot] : [], owner);
      newLevel += _SHIFT;
    }

    var oldTail = _tail;
    var newTail = newTailOffset < oldTailOffset ?
      _vectorNodeFor(newSize - 1) :
        newTailOffset > oldTailOffset ? new VNode([], owner) : oldTail;

    if (newTailOffset > oldTailOffset && oldSize > 0 && oldTail.length > 0) {
      newRoot = newRoot._ensureOwner(owner);
      var node = newRoot;
      for (var level = newLevel; level > _SHIFT; level -= _SHIFT) {
        var idx = (oldTailOffset >> level) & _MASK;
        node._array[idx] = node._array[idx] == null ? node._array[idx]._ensureOwner(owner) : new VNode([], owner);
        node = node._array[idx];
      }
      node._set((oldTailOffset >> _SHIFT) & _MASK, oldTail);
    }

    if (newSize < oldSize) {
      newTail = newTail._removeAfter(owner, 0, newSize);
    }

    if (__ownerID != null) {
      _size = newSize;
      _origin = 0;
      _level = newLevel;
      _root = newRoot;
      _tail = newTail;
      __altered = true;
      return this;
    }
    return new PersistentVector._make(0, newSize, newLevel, newRoot, newTail, null);

  }

  // TODO: debug funkcia, umazat
  void printInfo() {
    print("Size: $_size");
    print("Origin: $_origin");
    print("Level: $_level");
    print("Root: $_root");
    print("Tail: $_tail");
  }

  int get hashCode {
    if (this._hashCode == null) {
      int result = 17;
      for (E value in this) {
        result = 37 * result + value.hashCode % 1000000009;
      }
      this._hashCode = result;
    }
    return this._hashCode;
  }

  bool operator==(other) {
    if (other is! PersistentVector) return false;
    PersistentVector otherVector = other;
    if (this.hashCode != otherVector.hashCode) return false;
    if (this.length != otherVector.length) return false;
    for (int i = 0; i < this.length; i++) {
      if (this.get(i) != otherVector.get(i)) return false;
    }
    return true;
  }

  PersistentVector clear() {
    if (this.length == 0) {
      return this;
    }
    if (this.__ownerID != null) {
      this._size = 0;
      this._origin = 0;
      this._level = _SHIFT;
      this._root = this._tail = null;
      this._hashCode = null;
      this.__altered = true;
      return this;
    }
    return new PersistentVector.empty();
  }

  PersistentVector _ensureOwner(Owner ownerID) {
    if (ownerID == this.__ownerID) {
      return this;
    }
    if (ownerID == null) {
      this.__ownerID = ownerID;
      return this;
    }
    return new PersistentVector._make(this._origin, this._size, this._level, this._root, this._tail, ownerID, this._hashCode);
  }

  PersistentVector asMutable() {
    return this.__ownerID != null ? this : this._ensureOwner(new Owner());
  }

  PersistentVector asImmutable() {
    return this._ensureOwner(null);
  }

  bool wasAltered() {
    return this.__altered;
  }

  PersistentVector withMutations(fn) {
    var mutable = this.asMutable();
    fn(mutable);
    return mutable.wasAltered() ? mutable._ensureOwner(this.__ownerID) : this;
  }

}

class VNode {
  List _array;
  int _ownerID;

  int get length => _array.length;

  String toString() {
    return "VNode: " + _array.toString();
  }

  VNode(this._array, this._ownerID);

  void _set(int index, value) {
    if (_array.length > index) {
      _array[index] = value;
    } else {
      for (int i = _array.length; i < index; i++) {
        _array.add(null);
      }
      _array.add(value);
    }
  }

  VNode _removeAfter(Owner ownerID, int level, int index) {
    if (index == level ? 1 << level : 0 || this.length == 0) {
      return this;
    }
    var sizeIndex = ((index - 1) >> level) & _MASK;
    if (sizeIndex >= this.length) {
      return this;
    }
    var removingLast = sizeIndex == this.length - 1;
    var newChild;
    if (level > 0) {
      var oldChild = this._array[sizeIndex];
      if (oldChild == null) newChild = null;
      else {
        newChild = oldChild.removeAfter(ownerID, level - SHIFT, index);
      }
      if (newChild == oldChild && removingLast) {
        return this;
      }
    }
    if (removingLast && newChild == null) {
      return this;
    }
    var editable = _mutableVNode(this, ownerID);
    if (!removingLast) {
      editable._array.removeRange(sizeIndex + 1, editable._array.length);
    }
    if (newChild != null) {
      editable._array[sizeIndex] = newChild;
    }
    return editable;

  }

  _ensureOwner(ownerID) {
    if (ownerID && ownerID == _ownerID)
      return this;
    return new VNode(_array.sublist(0), ownerID);
  }

  VNode _update(ownerID, level, index, value, Bool didAlter) {
    var deleted = value == _NOT_SET;
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
      var newLowerNode = _updateVNode(lowerNode, ownerID, level - _SHIFT, index, value, didAlter);
      if (newLowerNode == lowerNode) {
        return node;
      }
      var newNode = _mutableVNode(node, ownerID);
      newNode._set(idx, newLowerNode);
      return newNode;
    }

    if (!deleted && nodeHas && node._array[idx] == value) {
      return node;
    }

    didAlter.value = true;

    newNode = _mutableVNode(node, ownerID);
    if (deleted) {
      newNode._set(idx, null);
    } else {
      newNode._set(idx, value);
    }
    return newNode;
  }
}

_updateVNode(VNode node, Owner ownerID, int level, int index, value, Bool didAlter) {
  if (node == null) {
    var t = new VNode([], new Owner());
    return t._update(ownerID, level, index, value, didAlter);
  }
  return node._update(ownerID, level, index, value, didAlter);
}

VNode _mutableVNode(VNode node, Owner ownerID) {
  if (ownerID != null && node != null && ownerID == node._ownerID) {
    return node;
  }
  return new VNode(node != null ? node._array.sublist(0) : [], ownerID);
}
