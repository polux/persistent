part of persistent;

class Persistent {}
class Owner {}

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
final _none = new Object();
final getNone = () => _none;
bool isNone(val) => val == _none;
