;;; jar-manifest-mode.el --- Major mode to edit JAR manifest files

;;; Copyright (C) 2014 Omair Majid

;; Author: Omair Majid <omair.majid@gmail.com>
;; URL: http://github.com/omajid/jar-manifest-mode
;; Package-Version: 20160430.1726
;; Version: 0.0.1
;; Keywords: convenience languages

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides support for working with jar file manifests
;; (normally named 'manifest.mf' or 'MANIFEST.MF').  It provides basic
;; syntax checking and keyword highlighting.

;; TODO
;; - Deal with x-Digest-y and x-Extension-* style attributes
;; - Highlight non-conforming entries as an error

;;; Code:

(defun jar-manifest-font-lock-extend-region ()
  "Extend the search region to contain an entire jar header."
  ;; font-lock-beg/end are dynamically bound, define them to remove warnings
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (let ((beg (or (re-search-backward "^[^ ]" nil t)
		   (point-min))))
      (goto-char font-lock-end)
      (when (re-search-forward "^[^ ]" nil t)
	(beginning-of-line)
	(setq font-lock-end (point)))
      (setq font-lock-beg beg))))

(defconst jar-manifest-header-name-regexp "^[a-zA-Z0-9]\\(?:[a-zA-Z0-9]\\|_\\|-\\)*"
  "Regexp matching a header name.")

(defconst jar-manifest-header-value-regexp " .*\n\\( .*\n\\)*"
  "Regexp matching a header value.")

(defconst jar-manifest-header-regexp
  (concat
   "\\(" jar-manifest-header-name-regexp "\\):"
   "\\(" jar-manifest-header-value-regexp "\\)"))

(defconst jar-manifest-well-known-header-names
  (list "Manifest-Version" "Created-By" "Signature-Version" "Class-Path"
        "Permissions" "Codebase" "Application-Name" "Application-Library-Allowable-Codebase" "Caller-Allowable-Codebase"
        "Trusted-Only" "Trusted-Library"
        "Main-Class"
        "Extension-List"
        "Extension-Name"
        "Implementation-Title" "Implementation-Version" "Implementation-Vendor" "Implementation-Vendor-Id" "Implementation-URL"
        "Specification-Title" "Specification-Version" "Specification-Vendor"
        "Sealed"
        "Content-Type"
        "Premain-Class" "Agent-Class" "Boot-Class-Path" "Can-Redefine-Classes" "Can-Retransform-Classes" "Can-Set-Native-Method-Prefix"
        "Java-Bean"
        "Magic"))

;; TODO make this more comprehensive
(defconst jar-manifest-known-header-names-regexp
  (concat
   "^"
   (regexp-opt jar-manifest-well-known-header-names 'symbols)))

(defconst jar-manifest-font-lock-keywords
  (list
   (cons jar-manifest-known-header-names-regexp 'font-lock-keyword-face)
   (list jar-manifest-header-regexp '(1 font-lock-type-face) '(2 font-lock-string-face)))
  "Expressions to highlight in jar-manifest-mode.")

(defalias 'jar-manifest-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode jar-manifest-mode jar-manifest-parent-mode "Manifest"
  "Major mode for editing JAR Manifest (Manifest.mf) files."

  ;; syntax highlighting
  (setq font-lock-defaults '(jar-manifest-font-lock-keywords))
  ;; syntax extends across lines, so work across lines
  (set (make-local-variable 'font-lock-multiline) t)
  (setq-local font-lock-multiline t)
  ;; buffer-local hook to extend regions used for syntax processing
  (add-hook 'font-lock-extend-region-functions 'jar-manifest-font-lock-extend-region nil t))


;;;###autoload
(add-to-list 'auto-mode-alist '("[Mm][Aa][Na][Ii][Ff][Ee][Ss][Tt]\\.[Mm][Ff]\\'" . jar-manifest-mode))

(provide 'jar-manifest-mode)
;;; jar-manifest-mode.el ends here
