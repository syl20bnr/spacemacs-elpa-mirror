;;; lsp-vue.el --- Vue support for lsp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Nikita Sivakov <cryptomaniac.512@gmail.com>

;; Author: Nikita Sivakov <cryptomaniac.512@gmail.com>
;; Version: 1.0
;; Package-Version: 20180628.715
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

(defgroup lsp-vue
  nil
  "Customization options for lsp-vue")

(defgroup html
  nil
  "Customization options for lsp-vue-html"
  :prefix "html."
  :group 'lsp-vue)

(defgroup vetur
  nil
  "Customization options for vetur"
  :prefix "vetur."
  :group 'lsp-vue)

(defcustom html.suggest.angular1
  t
  "Suggest for angular1"
  :type 'boolean
  :group 'html)

(defcustom html.suggest.html5
  t
  "Suggest for html5"
  :type 'boolean
  :group 'html)

(defcustom html.suggest.ionic
  t
  "Suggest for ionic"
  :type 'boolean
  :group 'html)

(defcustom vetur.colorDecorators.enable
  t
  "Use color decorators in vue"
  :type 'boolean
  :group 'vetur)

(defcustom vetur.completion.autoImport
  t
  "Include completion for module export and auto import them"
  :group 'vetur
  :type 'boolean)

(defcustom vetur.grammar.customBlocks
  '((doc . "md") (i18n . "json"))
  "Mapping from custom block tag name to language name. Used for generating grammar to support syntax highlighting for custom blocks"
  :group 'vetur
  :type 'sexp)

(defcustom vetur.validation.template
  t
  "Validate vue-html in <template> using eslint-plugin-vue"
  :group 'vetur
  :type 'boolean)

(defcustom vetur.validation.style
  t
  "Validate js/ts in <script>"
  :group 'vetur
  :type 'boolean)

(defcustom vetur.format.defaultFormatter.html
  "none"
  "Default formatter for <template> region"
  :group 'vetur
  :type '(radio
          (const "none" :tag "disable formatting")
          (const "js-beautify-html" :tag "html formatter of js-beautify")))

(defcustom vetur.format.defaultFormatter.css
  "pretter"
  "Default formatter for <style> region"
  :group 'vetur
  :type '(radio
          (const "none" :tag "disable formatting")
          (const "pretter" :tag "css formatter using css parser from prettier")))

(defcustom vetur.format.defaultFormatter.postcss
  "prettier"
  "Default formatter for <style lang='postcss'> region"
  :group 'vetur
  :type '(radio
          (const "none" :tag "disable formatting")
          (const "pretter" :tag "css formatter using css parser from prettier")) )

(defcustom vetur.format.defaultFormatter.scss
  "prettier"
  "Default formatter for <style lang='scss'> region"
  :group 'vetur
  :type '(radio
          (const "none" :tag "disable formatting")
          (const "pretter" :tag "css formatter using css parser from prettier")) )


(defcustom vetur.format.defaultFormatter.less
  "prettier"
  "Default formatter for <style lang='less'> region"
  :group 'vetur
  :type '(radio
          (const "none" :tag "disable formatting")
          (const "pretter" :tag "css formatter using css parser from prettier")) )

(defcustom vetur.format.defaultFormatter.stylus
  "stylus-supremacy"
  "Default formatter for <style lang='stylus'> region"
  :group 'vetur
  :type '(radio
          (const "none" :tag "disable formatting")
          (const "stylus-supremacy" :tag "stylus formatter from stylus-supremacy")))

(defcustom vetur.format.defaultFormatter.js
  "prettier"
  "Default formatter for <script> region"
  :group 'vetur
  :type '(radio
          (const :tag "disable formatting" "none")
          (const :tag "js formatter from prettier" "prettier" )
          (const "vscode-typescript" :tag "js formatter from TypeScript")))

(defcustom vetur.format.defaultFormatter.ts
  "prettier"
  "Default formatter for <script> region"
  :group 'vetur
  :type '(radio
          (const "none" :tag "disable formatting")
          (const "prettier" :tag "ts formatter using typescript parser from prettier")
          (const "vscode-typescript" :tag "ts formatter from TypeScript")))

(defcustom vetur.format.defaultFormatterOptions
  '(js-beautify-html . nil)
  "Options for all default formatters"
  :group 'vetur
  :type 'sexp)

(defcustom vetur.format.styleInitialIndent
  nil
  "Whether to have initial indent for <style> region"
  :group 'vetur
  :type 'boolean)

(defcustom vetur.format.scriptInitialIndent
  nil
  "Whether to have initial indent for <script> region"
  :group 'vetur
  :type 'boolean)


(defun lsp-vue--vetur-configuration (features)
  "Get all features configuration."
  (cl-labels ((dotted-p (x) (not (consp (cdr x))))
              (walklist
               (l table)
               (let (
                     (key (car l))
                     (value (cdr l))
                     (localtable (or table (make-hash-table :test 'equal))))

                 (if (listp key)
                     (dolist (sublist l)
                       (walklist sublist localtable))

                   (progn
                     (puthash key (or (gethash key localtable) (make-hash-table :test 'equal)) localtable)
                     (if (not (dotted-p l))
                         (puthash key (walklist value (gethash key localtable)) localtable)
                       (puthash key value localtable))))

                 localtable)))
    (let ((table (make-hash-table :test 'equal)))
      (dolist (feature features)
        (walklist
         (mapcar
          #'(lambda (form)
              (let* ((custom (first form))
                     (path (split-string (symbol-name custom) "\\.")))
                (append path (symbol-value custom))))
          (get feature 'custom-group))
         table))
      table)))


(defconst lsp-vue--get-root (lsp-make-traverser #'(lambda (dir)
                                                           (directory-files dir nil "package.json"))))

(lsp-define-stdio-client lsp-vue "vue"
                         lsp-vue--get-root '("vls"))


(defun lsp-vue--set-configuration ()
  "Send project config to lsp-server"
    (lsp--set-configuration (lsp-vue--vetur-configuration '(vetur html))))

(add-hook 'lsp-after-initialize-hook 'lsp-vue--set-configuration)

(defun lsp-vue-mmm-enable ()
  "Enable lsp-vue for all major-modes supported by ‘vue-mode’."
  (interactive)
  (lsp-vue-enable)
  (when (and lsp-ui-flycheck-enable (featurep 'lsp-ui-flycheck) (featurep 'vue-mode))
    (require 'vue-mode)
    (dolist (mode-settings vue-modes)
      (lsp-ui-flycheck-add-mode (plist-get mode-settings ':mode)))))

(provide 'lsp-vue)
;;; lsp-vue.el ends here
