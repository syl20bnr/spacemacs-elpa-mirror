`gvpr` is the "graph pattern recognizer", a graph processing and
transformation tool that is part of the Graphviz open-source graph
visualization suite (<http://www.graphviz.org/>).

`gvpr` is an `awk`-like processor for graphs defined in Graphviz's
DOT language.  `gvpr` can apply a user-specified *action* to every
graph, node, or edge that meets the conditions of the corresponding
*predicate*.

`gvpr` defines a C-like language for this.

This file defines a very basic emacs mode for syntax coloring
of `gvpr` scripts.

To use it simply load` or `require` this file.
You can then enable this mode by executing `M-x gvpr-mode`.
The `.gvpr` file extension is associated with this mode
by default.
