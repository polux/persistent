# Technical overview

The implementation of Persistent Vector is very similar to the one found in
Facebook's [immutable.js] (https://github.com/facebook/immutable-js). We show almost no invention
here. The rest of the document describes our design of Persistent Map which is more unusual and needs
more explanation.


## PMap technical overview

The implementation is a version of HAMT, be sure you understand the basic concepts before reading
further. Good places to start are [wikipedia] (http://en.wikipedia.org/wiki/Hash_array_mapped_trie)
or this [blog post]
(http://blog.higher-order.net/2009/09/08/understanding-clojures-persistenthashmap-deftwice.html).
The following text explains issues, that are specific for our implementation.

The whole HAMT consists of two types of nodes: Node and Leaf (they are called _Node and _Leaf in the
code). 

Node is typical HAMT inner node. It's branching factor is set to 16 (this may change) currently this
gets us best results in the benchmarks. Note that Node implements PMap interface.

Leaf can hold several key-value pairs. These are stored in a simple List such as:
[hash1, ke1, value1, hash2, key2, value2, etc..] 
if the leaf grows big (currently, > 48 such h,k,v triplets), it is split up to several Nodes. Similarly,
if Node stores only few k,v pairs (in all its nodes) it is compacted to one single Leaf (threshold
for this is currently set to < 32 triplets)

Few things to note here:

- In the tree, h,k,v triplets are stored in a way to guarantee the following property: if iterating
  through one Node by inorder (i.e. you are recursively visiting its children from the 0th to the
  15-th), you enumerate h,k,v triplets sorted by hash value. This may look unimportant on the first
  glance, but it simplifies several things; for example comparing Leaf with Node on equality, or doing intersection
  with Leaf and Node gets easier. For this purpose, we do the following:

    - In a single Leaf, h,k,v triplets are sorted by the hash. This allows us to binsearch for the
      correct value, when doing lookup.

    - In the put / lookup process, we consume the key hash from the first digits (not from the last,
      as usual). Note that hashes of small objects (especially, small ints) tend to have just zeros
      in the leading places. To overcome this problem, we work with mangled hash, which has enough
      entropy also in the first digits (check out _mangeHash function).

- In the Node implementation we're not compacting the array of children. Typically, to save memory, HAMT
  implementation stores only not-null children. Such implementations then use bitmask to correctly
  determine, what the proper indexes of individual (not-null) children would be (if the nulls were
  there). Such trick is neat, but it costs time, and moreover, we don't need it. Why? Because we
  store up to 48 values in a single Leaf. This means, when the Leaf gets expanded to a proper Node,
  most of its children will be not null. (Exercise: you randomly pick 48 numbers from
  interval 0,15 inclusive. What is the expectation for the count of numbers not picked at least once?)

- Node is a strange class. It serves for two purposes (which is probably not the cleanest design):
  it implements all PMap methods (in fact, when you construct new PMap, what you got is Node) and it
  implements low-level method for HAMT manipulation. Moreover, PMap methods (such as assoc) can be
  called only on the root Node - on every other Node, such call will lead to inconsistent result.
  Why such bad design?
  
  - The main purpose is to save time and memory by creating an additional object that would encapsulate the
    root Node (yes, it matters).

  - All "bad things" happen only internally and there is no possibility for the end-user to get the
    structure to the inconsistent state. So, it's not such a bad design after all.

