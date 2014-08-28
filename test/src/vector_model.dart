part of test_util;

class ModelVector<E> extends IterableBase implements PersistentVectorInterface<V> {
  final List<E> _vector;
  int get length => _vector.length;

  ModelVector(this._vector);

  bool get isEmpty => _vector.isEmpty;

  ModelVector<E> push(E value) {
    var result = new List.from(_vector);
    result.add(value);
    return new ModelVector(result);
  }

  ModelVector<E> pop() {
    var result = new List.from(_vector);
    result.removeLast();
    return new ModelVector(result);
  }

  ModelVector<E> set(int key, E value) {
    var result = new List.from(_vector);
    result[key] = value;
    return new ModelVector(result);
  }

  factory ModelVector.empty() {
    return new ModelVector([]);
  }

  String toString() => _vector.toString();

  Iterator<E> get iterator => _vector.iterator;
}