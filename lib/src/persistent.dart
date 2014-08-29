part of persistent;

class Persistent {}

deepPersistent(from) {
  if(from is Persistent) return from;
  if(from is Map) {
    var map = new PersistentMap();
    return map.withTransient((TransientMap map) {
      from.forEach((key,value) => map.doInsert(key, deepPersistent(value)));
    });
  }
  else {
    return from;
  }
}