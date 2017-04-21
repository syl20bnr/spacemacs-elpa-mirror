;;; lsp-python.el --- Python support for lsp-mode

;; Copyright (C) 2017 Vibhav Pant <vibhavp@gmail.com>

;; Author: Vibhav Pant <vibhavp@gmail.com>
;; Version: 1.0
;; Package-Version: 20170421.322
;; Package-Requires: ((lsp-mode "2.0"))
;; Keywords: python
;; URL: https://github.com/emacs-lsp/lsp-python

(require 'lsp-mode)
(require 'lsp-common)
(require 'python)

;;;###autoload
(lsp-define-stdio-client 'python-mode "python" 'stdio
			 (lsp-make-traverser #'(lambda (dir)
						 (directory-files
						  dir
						  nil
						  "\\(__init__\\|setup\\)\\.py")))
			 "Python Language Server"
			 '("pyls"))

(provide 'lsp-python)
;;; lsp-python.el ends here
