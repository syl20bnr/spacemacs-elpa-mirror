Mismatches of html tags are highlighted with the face html-check-frag-error-face.
You can go to the next mismatch with html-check-frag-next.

Installation:
Put html-check-frag.el into your load-path and add the following line into
your emacs start-up file (e.g. "~/.emacs"):

(require 'html-check-frag)

If you want to start html-check-frag-mode together with html-mode then also add:

(add-hook 'html-mode-hook (lambda () (html-check-frag-mode 1)))
