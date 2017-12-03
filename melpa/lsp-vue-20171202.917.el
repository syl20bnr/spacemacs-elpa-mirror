;;; lsp-vue.el --- Vue support for lsp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Nikita Sivakov <cryptomaniac.512@gmail.com>

;; Author: Nikita Sivakov <cryptomaniac.512@gmail.com>
;; Version: 1.0
;; Package-Version: 20171202.917
;; Package-Requires: ((emacs "25.1") (lsp-mode "3.0"))
;; URL: https://github.com/emacs-lsp/lsp-vue

;;; The MIT License:

;; http://en.wikipedia.org/wiki/MIT_License
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Vue support for lsp-mode using official vue-language-server.

;; Enable `lsp-vue` for any needed major-mode provided by `vue-mode` with
;; `lsp-vue-mmm-enable', also you can use `lsp-vue-enable` function to enable it
;; in needed major-mode without `lsp-vue`

;;; Code:

(require 'lsp-mode)

(defconst lsp-vue--get-root (lsp-make-traverser #'(lambda (dir)
							   (directory-files dir nil "package.json"))))

(lsp-define-stdio-client lsp-vue "vue"
			 lsp-vue--get-root '("vls"))

(defun lsp-vue-mmm-enable ()
  "Enable lsp-vue for all major-modes supported by ‘vue-mode’."
  (interactive)
  (lsp-vue-enable)
  (when (and lsp-enable-flycheck (featurep 'lsp-flycheck) (featurep 'vue-mode))
    (require 'vue-mode)
    (dolist (mode-settings vue-modes)
      (lsp-flycheck-add-mode (plist-get mode-settings ':mode)))))

(provide 'lsp-vue)
;;; lsp-vue.el ends here
