   Require confirmation for large region deletion.
 Replacements for `kill-region' and `clipboard-kill-region'.

 Original code by Bard Bloom, bard@theory.lcs.mit.edu.
 Modifications by Drew Adams.

 This provides `kill-region-wimpy', a replacement for
 `kill-region'.  If the region is larger than `wimpy-delete-size'
 characters, then `kill-region-wimpy' asks you if you really want
 to delete it.  The prompt tells you how big the region is, and
 indicates the region's text.  This can thus also be used as an
 alternative to `C-x C-x' to determine where the region is.

 Similarly, `clipboard-kill-region-wimpy' is provided as a
 replacement for `clipboard-kill-region'.

 New functions defined here:

   `clipboard-kill-region-wimpy', `kill-region-wimpy'.

 New user options (variables) defined here:

   `wimpy-delete-dopey-message', `wimpy-delete-size'.
