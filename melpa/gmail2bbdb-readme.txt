To setup, insert below code into ~/.emacs:
  (add-to-list 'load-path "~/.emacs.d/lisp/")
  (autoload 'gmail2bbdb-import-file "gmail2bbdb" nil t nil)

Usage:
  - At https://www.google.com/contacts, click "More->Export->vCard format->Export".
    The file "contacts.vcf" will be downloaded.
  - Run command "M-x gmail2bbdb-import-file" and select contacts.vcf.
    ~/.bbdb will be created.
