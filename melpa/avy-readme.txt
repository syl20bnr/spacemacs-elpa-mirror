This package provides a generic completion method based on building
a balanced decision tree with each candidate being a leaf.  To
traverse the tree from the root to a desired leaf, typically a
sequence of `read-key' can be used.

In order for `read-key' to make sense, the tree needs to be
visualized appropriately, with a character at each branch node.  So
this completion method works only for things that you can see on
your screen, all at once:

* character positions
* word or subword start positions
* line beginning positions
* link positions
* window positions

If you're familiar with the popular `ace-jump-mode' package, this
package does all that and more, without the implementation
headache.
