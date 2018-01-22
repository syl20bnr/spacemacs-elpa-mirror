Bibliothek.el is a management tool for a library of PDFs.  Or it
aspires to be so.  It's quite fresh ATM, it'll grow as it gets
used more.

Presently, bibliothek.el displays PDF files from directories in
‘bibliothek-path’ in a tabulated list [1], where the user can view the
file related to row under point (RET on the button or ‘f’ anywhere
on the row by default) or bring up a buffer detailed with PDF
metadata of that file (bound to ‘i’ by default).  Also,
‘tabulated-list-mode’ provides an interactive buffer header, where
by clicking the column headers, the table can be sorted.

Some more documentation may be present in the docstring for the
‘bibliothek’ command.



Installation:

Bibliothek.el depends on ‘pdf-tools’.

After putting a copy of bibliothek.el in a directory in the
‘load-path’, bibliothek.el can be configured as such:

(require 'bibliothek)
(setq bibliothek-path (list "~/Documents"))

Then, the Bibliothek interface can be brought up via M-x bibliothek RET.

[1] (see (info "(elisp) Tabulated List Mode"))
