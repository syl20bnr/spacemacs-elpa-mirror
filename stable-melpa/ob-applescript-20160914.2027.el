;;; ob-applescript.el --- org-babel functions for template evaluation

;; Copyright (C) Stig Brautaset

;; Author: Stig Brautaset
;; Keywords: literate programming, reproducible research, mac
;; Package-Version: 20160914.2027
;; Homepage: http://github.com/stig/ob-applescript.el
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Requirements:

;; Use this section to list the requirements of this language.  Most
;; languages will require that at least the language be installed on
;; the user's system, and the Emacs major mode relevant to the
;; language be installed as well.

 ;;; Code:
(require 'ob)
(require 'ob-core)
(require 'ob-ref)
(require 'ob-eval)

;; Define tangle extension.
(add-to-list 'org-babel-tangle-lang-exts '("applescript" . "scpt"))
(add-to-list 'org-babel-tangle-lang-exts '("apples" . "scpt"))

;; (defvar org-babel-default-header-args:applescript '(:var))


(defun org-babel-variable-assignments:applescript (params)
  "Return list of AppleScript statements assigning the block's variables."
  (mapcar
   (lambda (pair)
     (format "set %s to %s\n"
             (car pair)
             (org-babel-applescript-var-to-applescript (cdr pair))))
   (mapcar #'cdr (org-babel-get-header params :var))))

(defun org-babel-applescript-var-to-applescript (var)
  "Convert an elisp var into a string of AppleScript source code
 specifying a var of the same value."
  (format "%S" var))

(defun org-babel-expand-body:applescript (body params)
  (concat (apply #'concat
                 (org-babel-variable-assignments:applescript params))
          "\n"
          body
          "\n"))

(defun org-babel-execute:applescript (body params)
  "Execute a block of AppleScript code with org-babel.
 This function is called by `org-babel-execute-src-block'"
  (message "executing AppleScript source code block")
  (let* ((processed-params (org-babel-process-params params))
         (full-body (org-babel-expand-body:applescript body processed-params)))
    (org-babel-applescript-table-or-string
     (org-babel-eval "osascript" full-body)
     params)))

(defun org-babel-execute:apples (body params)
  "Execute a block of AppleScript with org-babel."
  (org-babel-execute:applescript body params))

(defun org-babel-applescript-table-or-string (results params)
  "If the results look like a table, then convert them into an
 Emacs-lisp table, otherwise return the results as a string."
  (org-babel-reassemble-table
   (org-babel-result-cond (cdr (assoc :result-params params))
     (org-babel-read results)
     (let ((tmp-file (org-babel-temp-file "c-")))
       (with-temp-file tmp-file (insert results))
       (org-babel-import-elisp-from-file tmp-file)))
   (org-babel-pick-name
    (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
   (org-babel-pick-name
    (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params)))))

(provide 'ob-applescript)
;;; ob-applescript.el ends here
