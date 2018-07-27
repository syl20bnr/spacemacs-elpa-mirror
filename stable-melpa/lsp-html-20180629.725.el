;;; lsp-html.el --- HTML support for lsp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Vibhav Pant <vibhavp@gmail.com>

;; Author: Vibhav Pant <vibhavp@gmail.com>
;; Version: 1.0
;; Package-Version: 20180629.725
;; Package-Requires: ((lsp-mode "4.2"))
;; Keywords: languages, html, lsp
;; URL: https://github.com/emacs-lsp/lsp-html

;;; Commentary:

;; HTML support for lsp-mode using VSCode's vscode-html-langserver server.
;; Enable using (add-hook 'html-mode-hook #'lsp-html-enable)

;;; Code:

(require 'lsp-mode)
(require 'sgml-mode)

(lsp-define-stdio-client lsp-html "html"
			 (lambda () default-directory)
			 '("html-languageserver" "--stdio"))

(provide 'lsp-html)
;;; lsp-html.el ends here
