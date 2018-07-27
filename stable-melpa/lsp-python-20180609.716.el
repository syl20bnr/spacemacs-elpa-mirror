;;; lsp-python.el --- Python support for lsp-mode -*- lexical-binding: t -*-

;; Copyright (C) 2017 Vibhav Pant <vibhavp@gmail.com>

;; Author: Vibhav Pant <vibhavp@gmail.com>
;; Version: 1.0
;; Package-Version: 20180609.716
;; Package-Requires: ((lsp-mode "3.0"))
;; Keywords: python
;; URL: https://github.com/emacs-lsp/lsp-python

;;; Code:
(require 'lsp-mode)
(require 'lsp-common)

(lsp-define-stdio-client lsp-python "python"
			 (lsp-make-traverser #'(lambda (dir)
						 (directory-files
						  dir
						  nil
              "setup.py\\|Pipfile\\|setup.cfg\\|tox.ini")))
			 '("pyls"))

(provide 'lsp-python)
;;; lsp-python.el ends here
