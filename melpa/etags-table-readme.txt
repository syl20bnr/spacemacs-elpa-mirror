This extension sets the tags table(s) based on the current file.
`etags-table-alist' is a list of lists, where the car of each sublist is
compared to the current filename.  If it matches, all the rest of the list
elements are put on `tags-table-list'.  If `etags-table-search-up-depth' is
an integer, the file path will be searched upwards for a tags file.  If one
is found, it will be added to the tags table list; this is actually done
first so the local TAGS file is at the head of the list.

When you switch files and do something tag-related, the tags table list is
automatically recomputed.
