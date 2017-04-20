;;; lsp-python.el --- Python support for lsp-mode

;; Copyright (C) 2017 Vibhav Pant <vibhavp@gmail.com>

;; Author: Vibhav Pant <vibhavp@gmail.com>
;; Version: 1.0
;; Package-Version: 20170419.604
;; Package-Requires: ((lsp-mode "2.0"))
;; Keywords: python
;; URL: https://github.com/emacs-lsp/lsp-python

(require 'lsp-mode)
(require 'python)

;;;###autoload
(lsp-define-client 'python-mode "python" 'stdio #'(lambda () default-directory)
  :command '("pyls")
  :name "Python Language Server")

(provide 'lsp-python)
;;; lsp-python.el ends here
