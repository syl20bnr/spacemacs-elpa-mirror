A company-mode Emacs completion backend for
SLY, a Common Lisp IDE.

SLY lives at https://github.com/capitaomorte/sly. company-mode
lives at http://company-mode.github.io.  slime-company, whereupon
sly-company is based, lives at https://github.com/anwyn/slime-company.

Installation:

Install via MELPA, or alternatively do the following in your
`~/.emacs' or `~/.emacs.d/init/el' init file.

    (add-to-list 'load-path "/path/to/sly-company")
    (require 'sly-company)

In either case add this to the init file as well:

    (add-hook 'sly-mode-hook 'sly-company-mode)
    (add-to-list 'company-backends 'sly-company)

The following bindings for `company-active-map' may be useful:

  (define-key company-active-map (kbd "\C-n") 'company-select-next)
  (define-key company-active-map (kbd "\C-p") 'company-select-previous)
  (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-.") 'company-show-location)
