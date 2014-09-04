# Efficient Persistent Data Structures

Transient and persistent sets, maps and vectors with utilities

## Terminology

  * *Persistent* data structure is an immutable structure, that provides effective
    creation of slightly mutated copies.
  * *Transient* data structure is a mutable structure, that can be effectively
    converted to the persistent data structure. It is ussualy created from
    a persistent structure to apply some changes and then obtain a new persistent
    structure.

## Desing

In the following part, `Structure` stands for any of `Map`, `Set`, `Vector`.

### Interface

There are several interfaces for each `Structure`:
`PersistentStructure` and `TransientStructure` that both
implement `ReadStructure`.

#### `ReadStructure`

Declares the common, read-only interface.

#### `PersistentStructure`

Declares methods for creating mutated copies that has form
of `PersistentStructure someMethod(...)`.

Moreover, some mass operations are provided.
(For example, `union`, `intersection`)

#### `TransientStructure`

Declares methods for mutating itself that has form
of `void doSomeMethod(...)`. Each `doXxx` method coresponds
to the `xxx` method of `PersistentStructure` having the same
arguments and behavior.

It's only purpose is to apply bigger amount of small changes to
the persistent structure effectively. Therefore, no mass
operation interface is provided.

### Lifecycle

In the almost all cases `PersistentStructure` should be used.
However, when a bigger amount of changes should be done,
`TransientStructure` comes to the scene. It is created
by `PersistentStructure.asTransient()`, the changes are
aplied and then `TransientStructure.asPersistent()` is
called to obtain the result.

** WARNING! ** After `asPersistent()` is called, the
`TransientStructure` may (in the default implementation will)
become outdated and do not allow more changes
(throwing an exception).

The other possibility is to write a `change(TransientMap m)`,
function, that aplies the changes to `m` and pass it
to the `PersistentStructure.withTransient()`.

### Equality and hash

Two persistent structures are equal if they carry the equal data.
This allows them to be used as map keys - the key is the data in the context,
not the object itself.

Two transient structures are equal if they are the same object.

The hash code is consistent with the equality operator.

