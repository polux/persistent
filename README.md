# Efficient Persistent Data Structures

[![Build Status](https://drone.io/github.com/vacuumlabs/persistent/status.png)](https://drone.io/github.com/vacuumlabs/persistent/latest)

The project is forked from
[polux/persistent](https://github.com/polux/persistent).

## What are persistent data structures
*Persistent* data structure is an immutable structure; the main difference with standard data structures is how you 'write' to them: instead of mutating
the old structure, you create the new, independent, (slightly) modified copy of it. Typical examples of commonly used Persistent structures are String (in Java, Javascript, Python, Ruby) or Python's Tuple or Java's BigDecimal. [(Not only)](http://www.infoq.com/presentations/Value-Identity-State-Rich-Hickey) we believe such concept could be beneficial also for other data structures such as Maps, Lists/Vectors, Sets.

     var couple = new PMap.fromMap({'father': 'Homer', 'mother': 'Marge'});
     // do not (and can not) modify couple anymore
     var withChild = couple.assoc('boy', 'Bart');
     print(couple); // {mother: Marge, father: Homer}
     print(withChild); // {boy: Bart, mother: Marge, father: Homer}

## Got it. And it is cool because...?

### It disallows unwanted side effects
You know the story. Late in the evening, exhausted and frustrated you find out that some guy that implemented

     int ComputeResponseLength(Map responseMap) 

got a 'great' idea, that instead of just computing response length he also mutates responseMap in some tricky way (say, he does some kind of sanitization of responseMap). Even if this was mentioned in the documentation and even if the methods name was different: this is a spaghetti code.

### Equality and hashCode done right
Finally, they work as you'd expect. How cool is this:

    // deeply persist the structure of Maps and Lists
    PMap a = persist({[1,2]: 'tower', [1,3]: 'water'});
    PMap b = persist({[1,2]: 'tower', [1,3]: 'water'});
    assert(a==b); 
    // kids, don't try this with standard List, it ain't going to work
    print(a[persist([1, 2])]); // prints 'tower'

### Instant 'deep copy'
Just copy/pass the reference. It's like doing deep copy in O(1).

### Caching made easier
Caching can speed things up significantly. But how do you cache results of a function

    List findSuspiciousEntries(List<Map> entries)

One possible workaround would be to JSONize entries to string and use such string as a hashing key. However, it's much more elegant, safe (what about ordering of keys within maps?), performant and memory-wise with Persistent structures. Also, until you restrict to well-behaving functions, there's no need to invalidate cache; you can cache anything, for as long as you want.
    
### Simplicity matters
Fast copying or equality done right are nice features, but this is not the only selling point here. Having different ways how to copy (shallow, deep) objects or how to compare them (== vs. equals in Java) introduces new complexity. Even if you get used to it, it still takes some part of your mental capabilities and can lead to errors.

### Structure sharing 
    PMap map1 = persist({'a': 'something', 'b': bigMap});
    PMap map2 = a.assoc('a', 'something completely different');
Suppose you are interested, whether map1['b'] == map2['b']. Thanks to structure sharing, this is O(1) operation, which means it is amazingly fast - no need to traverse through the bigMap. Although it may sound unimportant at the first glance, it is what really enables fast caching of complex functions. Also, this is the reason, why [Om](https://github.com/swannodette/om/) framework is MUCH faster than Facebooks [React](http://facebook.github.io/react/).

## And what is the prize for this all
In the 2.0 release, we optimized memory consumption such that the only penalty for using Persistent
comes at lower speed. Although structure sharing makes the whole thing much more effective than naive
copy-it-all approach, Persistents are still slower than their mutable counterparts (note however, that on
the other hand, some operations runs significantly faster, so its hard to say something conclusive
here). Following numbers illustrate, how much slow are Persistent data structures when benchmarking either on DartVM
or Dart2JS on Node (the numbers are quite independent of the structure size):

* DartVM read speed: 2
* DartVM write speed: 12 (5 by using Transients)
* Dart2JS read speed: 3
* Dart2JS write speed: 14 (6 by using Transients)

Although the factors are quite big, the whole operation is still very fast and it probably won't be THE bottleneck which would slow down your app. 

Some [advanced topics](https://github.com/vacuumlabs/persistent/wiki/Advanced-topics).

