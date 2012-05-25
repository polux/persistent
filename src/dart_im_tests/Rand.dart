// Copyright 2012 Google Inc. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Author: Paul Brauner (polux@google.com)

/**
 * Immutable random number generator.
 */
class Rand {
  final int _s1;
  final int _s2;

  Rand._internal(this._s1, this._s2);

  factory Rand(int s) {
    if (s < 0) {
      return new Rand(-s);
    } else {
      int n = 2147483562;
      int q = s ~/ n;
      int s1 = (s % n) + 1;
      int s2 = (q % 2147483398) + 1;
      return new Rand._internal(s1, s2);
    }
  }

  Pair<int, Rand> next() {
    int k = _s1 ~/ 53668;
    int s1p = 40014 * (_s1 - k * 53668) - k * 12211;
    int s1pp = (s1p < 0) ? s1p + 2147483563 : s1p;
    int kp = _s2 ~/ 52774;
    int s2p = 40692 * (_s2 - kp * 52774) - kp * 3791;
    int s2pp = (s2p < 0) ? s2p + 2147483399 : s2p;
    int z = s1pp - s2pp;
    int zp = (z < 1) ? z + 2147483562 : z;
    return new Pair<int, Rand>(zp, new Rand._internal(s1pp, s2pp));
  }

  Pair<Rand, Rand> split() {
    int new_s1 = (_s1 == 2147483562) ? 1 : _s1 + 1;
    int new_s2 = (_s2 == 1) ? 2147483398 : _s2 -1;
    Rand nextG = next().snd;
    return new Pair<Rand, Rand>(
        new Rand._internal(new_s1, nextG._s2),
        new Rand._internal(nextG._s1, new_s2));
  }

  Pair<int, Rand> randomInt(int low, int high) {
    if (low > high) {
      return randomInt(high, low);
    } else {
      int k = high - low  + 1;
      int b = 2147483561;
      int n = _iLogBase(b, k);

      Pair<int, Rand> f(int i, int acc, Rand g) {
        if (i == 0) {
          return new Pair<int, Rand>(acc, g);
        } else {
          Pair<int, Rand> p = g.next();
          return f(i - 1, p.fst + acc * b, p.snd);
        }
      }

      Pair<int, Rand> p = f(n, 1, this);
      return new Pair<int, Rand>(low + (p.fst % k), p.snd);
    }
  }

  Pair<bool, Rand> randomBool()  {
    Pair<int, Rand> p = randomInt(0, 1);
    return new Pair<bool, Rand>((p.fst == 0) ? false : true, p.snd);
  }

  static int _iLogBase(int b, int i) => (i < b) ? 1 : 1 + _iLogBase(b, i ~/ b);

  toString() => "Rand($_s1, $_s2)";
}

/**
 * Mutable random number generator.
 */
class MRand {
  Rand _rand;

  MRand(int seed) : _rand = new Rand(seed);

  bool randomBool()  {
    Pair<bool, Rand> p = _rand.randomBool();
    _rand = p.snd;
    return p.fst;
  }

  int randomInt(int low, int high) {
    Pair<int, Rand> p = _rand.randomInt(low, high);
    _rand = p.snd;
    return p.fst;
  }

  toString() => "MRand($_rand)";
}
