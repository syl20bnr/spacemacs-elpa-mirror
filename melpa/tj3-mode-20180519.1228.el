;;; tj3-mode.el --- major mode for editing TaskJuggler 3 files

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Copyright (C) 2018 Christophe Rhodes <christophe@rhodes.io>

;; Author: Christophe Rhodes <christophe@rhodes.io>
;; URL: https://github.com/csrhodes/tj3-mode
;; Package-Version: 20180519.1228

;;; Commentary:

;; A major mode for editing TaskJuggler 3 source files.

;; TaskJuggler 3 is a project management tool taking descriptions of
;; resources, tasks and other project elements and attributes in plain
;; text.  A command-line application performs task scheduling,
;; resource allocation, and constructs reports.  For mor information
;; about TaskJuggler, visit <http://taskjuggler.org/>.

;; This major mode supports simple keyword-based font locking.

;;; Code:

(defgroup tj3 nil "Major mode for editing TaskJuggler 3 source files"
  :tag "TaskJuggler 3"
  :group 'languages)

(defvar tj3-iddef-properties
  (list "account"
        "resource"
        "shift"
        "task"))

(defvar tj3-properties
  (list "account"
        "auxdir"
        "balance"
        "copyright"
        "flags"
        "include"
        "leaves"
        "limits"
        "macro"
        "navigator"
        "projectid"
        "projectids"
        "rate"
        "resource"
        "shift"
        "statussheet"
        ;; supplement
        "task"
        "timesheet"
        "vacation"))

(defvar tj3-reports
  (list "accountreport"
        "export"
        "icalreport"
        "nikureport"
        "resourcereport"
        "statussheetreport"
        "tagfile"
        "taskreport"
        "textreport"
        "timesheetreport"
        "tracereport"))

(defvar tj3-font-lock-keywords
  `(
    (,(concat "^" (regexp-opt tj3-reports "\\(?1:") "\\(?:\\s-+\\(?2:\\w+\\)\\)?")
     (1 'font-lock-keyword-face) (2 'font-lock-variable-name-face))
    (,(concat "^\\s-*" (regexp-opt tj3-iddef-properties "\\(?1:") "\\(?:\\s-+\\(?2:\\w+\\)\\)?")
     (1 'font-lock-keyword-face) (2 'font-lock-function-name-face))))

(defvar tj3-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table))

(define-derived-mode tj3-mode prog-mode "tj3"
  "Major mode for editing TaskJuggler 3 source files.
\\{tj3-mode-map}."
  :group 'tj3
  :syntax-table tj3-mode-syntax-table
  (setq font-lock-defaults (list '(tj3-font-lock-keywords) nil)))

(add-to-list 'auto-mode-alist '("\\.tj[pi]\\'" . tj3-mode))

(provide 'tj3-mode)

;;; tj3-mode.el ends here
