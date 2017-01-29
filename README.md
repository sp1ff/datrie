# datrie
Efficient trie implementation

## Introduction

A trie, sometimes also called a prefix tree, is a search tree used to
store associative arrays where the keys are typically strings. Unlike
the more familiar binary search tree, keys do not correspond directly
to nodes; rather, the key is encoded by position in the tree in such a
way that all descendents of a given node share the same prefix (this
makes it handy for prefix-matching).

Straightforward trie implementations tend to be memory-inefficient;
this project provides an extremely compact implementation in C due to
[Aoe](#ref_1).

## Discussion

I wrote this several years ago when I was writing Windows device
drivers (I've only recently gotten around to cleaning it up &
publishing it now). The runtime facilities available in kernel mode on
Windows were much simpler, and in particular, there was no associative
array. My keys were going to be strings, and memory was at a premium,
so I wanted a very compact representation, rather than a
general-purpose datastructure. [Aoe](#ref_1)'s implementation was
certainly compact, the algorithm appealed to me, and I'd always wanted
to learn more about tries, so I coded it up.

The interface & implementation are fully documented in `datrie.h`, but
briefly, I tried to make this code usable on any platform, in both
kernel & user modes.  As such, I've tried to factor out any system
dependecies, even C runtime library calls (which generally aren't
available in Kernel mode on Windows, for instance).  Client code is
expected to provide implementations of certain services (string
copying, e.g.) through `#define`s at compile-time. 

The project contains a little driver program, so it can be compiled &
run "out of the box".


## References

1. <a name="ref_1"> Aoe, Jun-Ichi "An Efficient Digital Search Algorithm by Using a Double-Array Structure", IEEE Transactions On Software Engineering. Vol. 15, No. 9. September 1989.

2. <a name="ref_2"> Knuth, Donald "The Art of Computer Programming, Volume 3 Searching and Sorting", Second Edition, Chapter 6.3 Digital Searching, pp492-496.
