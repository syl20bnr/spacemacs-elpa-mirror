This package takes an org-mode tree and converts it into a
file that can be read into graphviz in order to visually show the
tree as a directed graph.  Mail to <theodore.wiles@gmail.com> to discuss
features and additions.  All suggestions are more than welcome.

The headings of the org-mode file are treated as node text in the resulting tree.
Org-mode heading tags are included in the resulting tree as additional cells
within the node.

The tags are color-coded to be consistent across the tree.

Tree interleaving is also possible by naming multiple org-mode headings
with the same heading.

NOTE: this requires the GRAPHVIZ software.  This is installable on
windows using cygwin.

To install, add this code to your .emacs:
(load "org-mind-map.el")

If on linux, customize the values of org-mind-map-unflatten-command
and org-mind-map-dot-command to have the values corresponding to
the executables in your system.

Then, run "M-x org-mind-map-write"

The latest version is available at:

https://github.com/theodorewiles/org-mind-map
