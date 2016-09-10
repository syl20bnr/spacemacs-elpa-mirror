Usage in Emacs,
`M-x elpamr-create-mirror-for-installed`
 If you use native Windows Emacs, install Cygwin or MSYS2.

Usage in Shell,
  emacs --batch -l ~/.emacs.d/init.el
        -l ~/any-directory-you-prefer/elpa-mirror.el \
        --eval='(setq elpamr-default-output-directory "~/myelpa")' \
        --eval='(elpamr-create-mirror-for-installed)

Make Emacs use the repository created by elpa-mirror,
  - Insert `(setq package-archives '(("myelpa" . "~/myelpa")))` into ~/.emacs
  - Restart Emacs

You can also setup repositories on Dropbox and Github.
See https://github.com/redguardtoo/elpa-mirror for HOW.
