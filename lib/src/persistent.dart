part of persistent;

class Persistent {}

final _none = new Object();
final none = () => _none;
bool isNone(val) => val == _none;