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

/**
 * A read-only vector, ordered collection of elements of type [K].
 *
 * There is no default implementation of [ReadVector], since it just
 * specifies the common interface of [PersistentVector] and [TransientVector].
 */
abstract class ReadVector<E> implements Iterable<E> {
  
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
  factory PersistentVector() => new PersistentVectorImpl.empty();
  
  /**
   * Creates an [PersistentVector] filled by [values]
   * using its default implementation.
   */
  factory PersistentVector.from(Iterable<E> values) => new PersistentVectorImpl.from(values);
  
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

abstract class PersistentVectorBase<E> extends IterableBase<E> {
  int _size;

  E _get(int index, {Function orElse: null});

  E get first => _get(0);
  E get last => _get(this.length > 0 ? this.length - 1 : 0);
  int get length => _size;
  Iterator<E> get iterator => new VectorIterator<E>(this);
}

class VectorIterator<E> extends Iterator<E> {
  PersistentVectorBase<E> _parentVector;
  int _position = -1;
  int _length;

  VectorIterator(this._parentVector) {
    _length = _parentVector.length;
  }

  bool moveNext() {
    if (_length == _position + 1) return false;
    _position++;
    return true;
  }

  E get current => _parentVector._get(_position);
}

abstract class BaseVectorImpl<E> extends PersistentVectorBase<E> {
  Owner _owner;
  _VNode _root;
  _VNode _tail;
  int _level;
  bool __altered = false;

  BaseVectorImpl._prototype() {
    this._owner = null;
    this._root = new _VNode([], _owner);
    this._tail = _root;
    this._level = _SHIFT;
    this._size = 0;
  }

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

  BaseVectorImpl<E> _set(int index, E value) {
    index = _checkIndex(index);

    var vector = this;
    var newTail = vector._tail;
    var newRoot = vector._root;
    var didAlter = new Bool();
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
    return new PersistentVectorImpl._make(vector._size, vector._level, newRoot, newTail);
  }

  BaseVectorImpl<E> _push(E value) {
    var len = this.length;
    return this._withTransient((vect) => vect._resize(len+1)._set(len, value));
  }

  BaseVectorImpl<E> _pop() {
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

  BaseVectorImpl<E> _resize(int end) {
    if (end < 0) {
      throw new RangeError.value(end);
    }
    var owner;
    if (_owner == null) {
      owner = _owner;
    } else {
      owner = new Owner();
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
    return new PersistentVectorImpl._make(newSize, newLevel, newRoot, newTail);

  }

  // TODO: debug funkcia, umazat
  void printInfo() {
    print("Size: $_size");
    print("Level: $_level");
    print("Root: $_root");
    print("Tail: $_tail");
  }

  BaseVectorImpl _ensureOwner(Owner ownerID) {
    if (ownerID == this._owner) {
      return this;
    }
    if (ownerID == null) {
      this._owner = ownerID;
      return new PersistentVectorImpl._make(this._size, this._level, this._root, this._tail);
    }
    return new TransientVectorImpl._make(this._size, this._level, this._root, this._tail, ownerID);
  }

  TransientVectorImpl _asTransient() {
    return this._owner != null ? this : this._ensureOwner(new Owner());
  }

  PersistentVectorImpl _asPersistent() {
    return this._ensureOwner(null);
  }

  BaseVectorImpl _withTransient(fn) {
    var transient = this._asTransient();
    fn(transient);
    return transient.wasAltered() ? transient._ensureOwner(this._owner) : this;
  }

}

class _VNode {
  List _array;
  Owner _ownerID;

  int get length => _array.length;

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

  _VNode _removeAfter(Owner ownerID, int newSize) {
    var sizeIndex = (newSize - 1) & _MASK;
    if (sizeIndex >= this.length - 1) {
      return this;
    }
    var editable = _transientVNode(this, ownerID);
    editable._array.removeRange(sizeIndex + 1, editable.length);
    return editable;

  }

  _VNode _update(ownerID, level, index, value, Bool didAlter) {
    var deleted = value == getNotSet();
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

_VNode _transientVNode(_VNode node, Owner ownerID) {
  if (ownerID != null && node != null && ownerID == node._ownerID) {
    return node;
  }
  return new _VNode(node != null ? node._array.sublist(0) : [], ownerID);
}

class PersistentVectorImpl<E> extends BaseVectorImpl<E> implements PersistentVector<E> {
  // cached hashCode.
  int _hashCode = null;


  factory PersistentVectorImpl.from(Iterable<E> values) {
    if (values.length == 0) {
      return new PersistentVectorImpl.empty();
    }
    PersistentVectorImpl<E> result = new PersistentVectorImpl.empty();
    result = result.withTransient((vector) {
      values.forEach((E value) {
        vector.doPush(value);
      });
      return vector;
    });
    return result;
  }

  factory PersistentVectorImpl.empty() => new PersistentVectorImpl._prototype();
  PersistentVectorImpl._prototype() : super._prototype();

  factory PersistentVectorImpl._make(int size, int level, _VNode root, _VNode tail) {
    var x = new PersistentVectorImpl._prototype();
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
    if (other is! PersistentVectorImpl) return false;
    PersistentVectorImpl otherVector = other;
    if (this.hashCode != otherVector.hashCode) return false;
    if (this.length != otherVector.length) return false;
    for (int i = 0; i < this.length; i++) {
      if (this._get(i) != otherVector.get(i)) return false;
    }
    return true;
  }

  PersistentVectorImpl _clear() {
    if (this.length == 0) {
      return this;
    }
    return new PersistentVectorImpl.empty();
  }

  TransientVectorImpl asTransient() => _asTransient();
  PersistentVectorImpl withTransient(fn) => _withTransient(fn);
  PersistentVectorImpl push(E value) => _push(value);
  PersistentVectorImpl pop() => _pop();
  PersistentVectorImpl set(int index, E value) => _set(index, value);
  E get(int index, {Function orElse: null}) => _get(index, orElse: orElse);
  E operator[](int index) => _get(index);
}

class TransientVectorImpl<E> extends BaseVectorImpl<E> implements TransientVector<E> {
  TransientVectorImpl._prototype() : super._prototype();

  factory TransientVectorImpl._make(int size, int level, _VNode root, _VNode tail, Owner ownerID) {
    var x = new TransientVectorImpl._prototype();
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

  TransientVectorImpl _clear() {
    this._size = 0;
    this._level = _SHIFT;
    this._root = this._tail = null;
    this.__altered = true;
    return this;
  }

  PersistentVectorImpl asPersistent() => _asPersistent();
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