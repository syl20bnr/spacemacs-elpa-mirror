Load this file and run M-x darcsum-whatsnew.  This will display a
pcl-cvs like buffer showing modified files.  RET on a file reveals
changes; RET on a directory reveals changes to its files.

Displayed changes may be recorded with "c", which offers a buffer
for inputting the change name (first line) and long description
(subsequent lines).  C-c C-c records the patch.

If you only want to record a part of your changes, you need to mark
those.  If a change is "marked" in the summary buffer with "m"
(done on the change, the file (all changes) or the directory (all
changes in all files)), only marked changes are recorded,
regardless of point.

Alternatively, if no changes are marked, then only visible changes
are recorded.

Move changes between buffers with "M", which prompts for a darcsum
buffer to move to (creating one if the buffer doesn't exist).

"g" forgets everything and resubmits the "whatsnew" command.
Collapsing a file forgets all marks for that file.  Only displayed
changes are ever recorded!

"n" and "p" move among files/changes.  "q" kills the buffer.
