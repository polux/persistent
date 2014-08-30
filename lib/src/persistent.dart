part of persistent;

class Persistent {}
class Owner {}

final _none = new Object();
final none = () => _none;
bool isNone(val) => val == _none;