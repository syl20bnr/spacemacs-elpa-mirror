org-brain implements a variant of concept mapping with org-mode, it is
inspired by The Brain software (http://thebrain.com). An org-brain is a
network of org-mode files, and you can get a visual overview of the
relationships between the files (kind of like a mind map): a file's parents
(other files which link to the file) and its children (files linked to from
the file). Files can also be browsed between via this overview.

All org files put into your `org-brain-path' directory will be considered as
"entries" (nodes, thoughts, pages, whatever you like to call them) in your
org-brain. An entry can link to other entries via an `org-mode' brain link,
for instance [[brain:index]]. You can also include search patterns in the
link, just like the `org-mode' file-link: [[brain:index::*Programming]].

You can use `org-brain-visualize' to see the relationships between entries,
quickly add parents/children/pins to an entry, and open them for editing. In
this view you may also have pinned entries, which will be shown at all times.
To pin an entry, add #+BRAIN_PIN: on a line in the beginning of the entry
file (or use bindings in `org-brain-visualize-mode' directly).
