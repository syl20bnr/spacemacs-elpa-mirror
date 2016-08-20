In Emacs,
`M-x elpamr-create-mirror-for-installed`

In Shell,
  emacs --batch -l ~/.emacs.d/init.el
        -l ~/any-directory-you-prefer/elpa-mirror.el \
        --eval='(setq elpamr-default-output-directory "~/myelpa")' \
        --eval='(elpamr-create-mirror-for-installed)

Usage,
  - Insert `(setq package-archives '(("myelpa" . "~/myelpa")))` into ~/.emacs
  - Restart Emacs

You can also setup repositories on Dropbox and Github.
See https://github.com/redguardtoo/elpa-mirror for HOW.
