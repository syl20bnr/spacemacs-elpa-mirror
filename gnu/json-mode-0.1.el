;;; json-mode.el --- Major mode for editing JSON files  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016 Free Software Foundation, Inc.

;; Author: Simen Heggestøyl <simenheg@gmail.com>
;; Maintainer: Simen Heggestøyl <simenheg@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: data

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for editing JavaScript Object Notation (JSON) data files.
;; Read more about JSON at http://json.org/.

;; It provides support for indentation and syntax highlighting.

;; Thanks to Josh Johnston for writing the original JSON mode!

;;; Code:

(require 'json)
(require 'smie)

(defgroup json-mode nil
  "JavaScript Object Notation (JSON) editing mode."
  :tag "JSON Mode"
  :group 'data)

(defcustom json-mode-indent-level 2
  "Basic size of one indentation step."
  :type 'integer)

(defface json-mode-object-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face to use for JSON object names.")

(defvar json-mode-map
  (let ((map (make-sparse-keymap "JSON")))
    (define-key map "\C-c\C-f" #'json-mode-pretty-print-dwim)
    (define-key map "\C-c\C-p" #'json-mode-show-path)
    (easy-menu-define json-menu map "JSON mode menu"
      `("JSON"
        :help "JSON-specific features"
        ["Pretty-print region" json-mode-pretty-print-dwim
         :visible (region-active-p)]
        ["Pretty-print buffer" json-mode-pretty-print-dwim
         :visible (not (region-active-p))]
        ["Show path" json-mode-show-path]))
    map)
  "Keymap used in JSON mode.")

(defvar json-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Objects
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    ;; Arrays
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    ;; Comments
    (modify-syntax-entry ?/ ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    st))

(defvar json-mode-font-lock-keywords
  `(;; Constants
    (,(concat "\\<" (regexp-opt json-keywords) "\\>")
     (0 font-lock-constant-face))
    ;; Object names
    ("\\(\"[^\"]*\"\\)[[:blank:]]*:"
     (1 'json-mode-object-name-face))
    ;; Strings
    ("\"\\(\\\\.\\|[^\"]\\)*\""
     (0 font-lock-string-face))))

(defun json-font-lock-syntactic-face-function (state)
  "Highlight comments only.
Strings are handled by `json-mode-font-lock-keywords', since we
want to highlight object name strings differently from ordinary
strings."
  (when (nth 4 state) font-lock-comment-face))

(defconst json-mode--smie-grammar
  (smie-prec2->grammar
   (smie-precs->prec2 '((assoc ",") (left ":")))))

(defun json-mode--smie-rules (method arg)
  "Provide indentation rules for METHOD given ARG.
See the documentation of `smie-rules-function' for further
information."
  (pcase (cons method arg)
    (`(:elem . basic) json-mode-indent-level)))

(defun json-mode-pretty-print-dwim (&optional alphabetical)
  "Pretty print region if active, else pretty print the buffer.
`json-mode-indent-level' will be used as indentation offset.  If
ALPHABETICAL is non-nil (interactively, with a prefix argument),
JSON object members will be sorted alphabetically by their keys."
  (interactive "P")
  (let ((json-encoding-default-indentation
         (make-string json-mode-indent-level ?\s)))
    (if (use-region-p)
        (funcall
         (if alphabetical
             'json-pretty-print-ordered
           'json-pretty-print)
         (region-beginning) (region-end))
      (funcall
       (if alphabetical
           'json-pretty-print-buffer-ordered
         'json-pretty-print-buffer)))))

(defun json-mode-show-path ()
  "Show the path to the JSON value under point.
The value is also copied to the kill ring."
  (interactive)
  (let ((path (json-path-to-position (point))))
    (if path
        (let ((formatted-path
               (json-mode--format-path (plist-get path :path))))
          (when (fboundp 'pulse-momentary-highlight-region)
            (pulse-momentary-highlight-region
             (plist-get path :match-start)
             (plist-get path :match-end)))
          (message formatted-path)
          (kill-new formatted-path))
      (message "Not a JSON value"))))

(defun json-mode--format-path (path)
  "Return PATH formatted as a JSON data selector.
PATH should be a list of keys, which can be either strings or
integers."
  (mapconcat (lambda (key) (format "[%S]" key)) path ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

;;;###autoload
(define-derived-mode json-mode prog-mode "JSON"
  "Major mode for editing JavaScript Object Notation (JSON) data files."
  (setq-local
   font-lock-defaults
   '(json-mode-font-lock-keywords
     nil nil nil nil
     (font-lock-syntactic-face-function
      . json-font-lock-syntactic-face-function)))
  ;; JSON has no comment syntax, but we set this to keep SMIE happy.
  ;; Also, some JSON extensions allow comments.
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (smie-setup json-mode--smie-grammar #'json-mode--smie-rules))

;;;; ChangeLog:

;; 2016-12-11  Simen Heggestøyl  <simenheg@gmail.com>
;; 
;; 	New package: json-mode
;; 


(provide 'json-mode)

;;; json-mode.el ends here
