`pmdm.el' is a simple alternative to desktop-mode for storing
and opening loaded files.

Run `pmdm-write-opened-files' manually (or in a hook) before quitting
emacs to save the files you want to restore later and `pmdm-load-files'
to open the stored files again.

Customizable variable `pmdm-file-name' contains the name of the file
used to store the files list.
