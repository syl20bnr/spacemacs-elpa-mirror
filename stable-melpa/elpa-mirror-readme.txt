- `M-x elpamr-create-mirror-for-installed` to create local repository at "~/myelpa"
- Insert `(setq package-archives '(("myelpa" . "~/myelpa")))` into ~/.emacs
   to use that local repository

You can run below command in shell instead:

  emacs --batch -l ~/.emacs.d/init.el
        -l ~/projs/elpa-mirror/elpa-mirror.el \
        --eval='(setq elpamr-default-output-directory "~/myelpa")' \
        --eval='(elpamr-create-mirror-for-installed)

You can also setup repositories on Dropbox and Github.
See https://github.com/redguardtoo/elpa-mirror for HOW.
