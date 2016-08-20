Opens MS Windows standard Open file dialog box.

The executable `getfile.exe' should be in the system path somewhere
or var `dlgopen-executable-path' should have executable path+name.

Command `dlgopen-open-files' opens the Windows Open dialog box.

If function `w32-browser' is defined, then it is used to open files
selected in the Windows Open dialog box.  Otherwise, standard Emacs
`find-file' functions are used.

If `w32-browser' is not defined, then `dlgopen-other-window'
controls how selected files are opened:
Non-nil => display chosen file(s) in separate windows.
Nil => display a single chosen file in the current window; don't
display multiple chosen files.  (The effect is reversed if
you supply a prefix arg to `dlgopen-open-files'.)

NOTE: This is a minor tweak of file `dlgopen.el', by Binu Jose
      Philip.
