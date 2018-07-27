org-brain implements a variant of concept mapping with org-mode, it is
inspired by The Brain software (http://thebrain.com).  An org-brain is a
network of org-mode entries, where each entry is a file or a headline, and
you can get a visual overview of the relationships between the entries:
parents, children, siblings and friends.  This visual overview can also be
used to browse your entries.  You can think of entries as nodes in a mind map,
or pages in a wiki.

All org files put into your `org-brain-path' directory will be considered
entries in your org-brain.  Headlines with an ID property in your entry file(s)
are also considered as entries.

Use `org-brain-visualize' to see the relationships between entries, quickly
add parents/children/friends/pins to an entry, and open them for editing.
