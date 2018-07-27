Tries are an efficient data structure for the storage of strings.
Addition, deletion, and query functions all have O(m) performance,
where m is the length of the string to be added/deleted/queried.

They are a natural choice for completing partial strings according to some
dictionary.

This implementation also supports key-value storage, which means that a trie
can act as a substitute for a dictionary / hash-table.

See http://en.wikipedia.org/wiki/Trie for more details.
