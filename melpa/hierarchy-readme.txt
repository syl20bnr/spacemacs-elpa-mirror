Library to create, query, navigate and display hierarchy structures.

Creation: After having created a hierarchy with `hierarchy-new', populate it by
calling `hierarchy-add-tree' or `hierarchy-add-trees'.  You can then optionally sort its
element with `hierarchy-sort'.

Querying: You can learn more about your hierarchy by using functions such as
`hierarchy-roots', `hierarchy-has-item', `hierarchy-length', `hierarchy-parent', `hierarchy-descendant-p'.

Navigation: When your hierarchy is ready, you can use `hierarchy-map-item', `hierarchy-map',
and `map-tree' to apply functions to elements of the hierarchy.

Display: You can display a hierarchy as a tabulated list using
`hierarchy-tabulated-display' and as an expandable/foldable tree
using `hierarchy-convert-to-tree-widget'.  The
`hierarchy-labelfn-*' functions will help you display each item of
the hierarchy the way you want it.
