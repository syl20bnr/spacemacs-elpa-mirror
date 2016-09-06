This package makes use of the `org-element' API to extract clock
entries from org files and convert them into CSV format. It is
intended to facilitate clocked time analysis in external programs.

In interactive mode, calling `org-clock-csv' will open a buffer
with the parsed entries from the files in `org-agenda-files'. The
function can also be called from Lisp code with a file list
argument, and there is an `org-clock-csv-batch' version that will
output the CSV content to standard output (for use in batch mode).
