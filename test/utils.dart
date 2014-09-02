import 'dart:math';

Random r = new Random();
bool probability(num what) => r.nextDouble() < what;
random_elem(List list) => list[r.nextInt(list.length)];