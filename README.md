# Efficient Persistent Data Structures

Transient and persistent sets, maps and vectors with utilities

## Terminology

  - *Persistent* data structure is an immutable structure, that provides effective
    creation of slightly mutated copies.
  - *Transient* data structure is a mutable structure, that can be effectively
    converted to the persistent data structure. It is ussualy created from
    a persistent structure to apply some changes and then obtain a new persistent
    structure.

## Desing

In the following part, `Structure` stands for any of `Map`, `Set`, `Vector`.

### Classes

There are several interfaces for each `Structure`:
`PersistentStructure` and `TransientStructure` that both
implements `ReadStructure`.

`ReadStructure` declares the common, read-only interface.

`PersistentStructure` declares interface for creating mutated copies that has form
of `PersistentStructure someMethod(...)`. 

`TransientStructure` declares interface for mutating itself that has form
of `void doSomeMethod(...)`. It's main purpose is to apply bigger amount of changes to
the persistent structure effectively. Therefore, after obtaining modified persistent structure,
it may be outdated - all mutating methods throw.

`TransientStructure.doSomeMethod(...)` has the same arguments and behavior
as `PersistentStructure.someMethod(...)` except what they modify.

