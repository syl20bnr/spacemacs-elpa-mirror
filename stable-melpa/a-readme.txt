Library for dealing with associative data structures: alists, hash-maps, and
vectors (for vectors, the indices are treated as keys).

This library is largely inspired by Clojure, it has many of the functions
found in clojure.core, prefixed with `a-'. All functions treat their
arguments as immutable, so e.g. `a-assoc' will clone the hash-table or alist
it is given. Keep this in mind when writing performance sensitive code.
