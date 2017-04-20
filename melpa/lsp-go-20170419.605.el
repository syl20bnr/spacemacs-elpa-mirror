;;; lsp-go.el --- Go support for lsp-mode

;; Copyright (C) 2017 Vibhav Pant <vibhavp@gmail.com>

;; Author: Vibhav Pant <vibhavp@gmail.com>
;; Version: 1.0
;; Package-Version: 20170419.605
;; Package-Requires: ((lsp-mode "2.0") (go-mode "1.0.0"))
;; Keywords: go, golang
;; URL: https://github.com/emacs-lsp/lsp-go

(require 'lsp-mode)
(require 'go-mode)

;;;###autoload
(lsp-define-client 'go-mode "go" 'stdio #'(lambda () default-directory)
  :command '("go-langserver" "-mode=stdio")
  :name "Go Language Server"
  :ignore-regexps '("^langserver-go: reading on stdin, writing on stdout$"))

(provide 'lsp-go)
;;; lsp-go.el ends here
