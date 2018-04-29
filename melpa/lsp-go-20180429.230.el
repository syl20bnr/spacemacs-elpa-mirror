;;; lsp-go.el --- Go support for lsp-mode

;; Copyright (C) 2017 Vibhav Pant <vibhavp@gmail.com>

;; Author: Vibhav Pant <vibhavp@gmail.com>
;; Version: 1.0
;; Package-Version: 20180429.230
;; Package-Requires: ((lsp-mode "3.0"))
;; Keywords: go, golang
;; URL: https://github.com/emacs-lsp/lsp-go

(require 'lsp-mode)

(lsp-define-stdio-client lsp-go "go" #'(lambda () default-directory)
			 '("go-langserver" "-mode=stdio" "-gocodecompletion")
			 :ignore-regexps
			 '("^langserver-go: reading on stdin, writing on stdout$"))

(provide 'lsp-go)
;;; lsp-go.el ends here
