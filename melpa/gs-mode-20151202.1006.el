;;; gs-mode.el --- Major mode for editing GrADS script files

;; Author: Joe Wielgosz <joew@cola.iges.org>
;; Created: 2 Oct 2003
;; Keywords: GrADS script major-mode
;; Package-Version: 20151202.1006
;; Version: 0.1

;; Copyright (C) Joe Wielgosz <joew@cola.iges.org>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;;
;; Based on wpdl-mode-el, a major mode for editing WPDL files
;; by Scott Andrew Borton <scott@pp.htv.fi>,
;; which is an example used in a tutorial about Emacs
;; mode creation. The tutorial can be found here:
;; http://two-wugs.net/emacs/mode-tutorial.html

;;; Code:
(defvar gs-mode-hook nil)
(defvar gs-mode-map nil
  "Keymap for GrADS script major mode.")

(if gs-mode-map nil
  (setq gs-mode-map (make-keymap)))

(setq auto-mode-alist
      (append
       '(("\\.gs\\'" . gs-mode))
       auto-mode-alist))

(defconst gs-font-lock-keywords-1
  (list
   '("\\b\\(e\\(?:lse\\|nd\\(?:if\\|while\\)\\)\\|function\\|if\\|while\\)\\b" . font-lock-keyword-face)
   '("\\b\\(close\\|p\\(?:rompt\\|ull\\)\\|read\\|s\\(?:ay\\|ub\\(?:lin\\|str\\|wrd\\)\\)\\|write\\)\\b" . font-lock-builtin-face)
   ;; unused faces:
   ;;   '("\\('\\w*'\\)" . font-lock-variable-name-face)
   ;;   '("\\<\\(TRUE\\|FALSE\\)\\>" . font-lock-constant-face)))
   "Highlighting expressions for GrADS script mode."))

(defvar gs-font-lock-keywords gs-font-lock-keywords-1
  "Default highlighting expressions for GrADS script mode.")

(defun gs-indent-line ()
  "Indent current line as GrADS script code."
  (interactive)
  (beginning-of-line)
  (if (or (bobp) (looking-at "\\s-*\\*") (looking-at "\\s-*function"))
      ;; First line is always non-indented
      ;; so are comments and functions
      (indent-line-to 0)

    (let ((not-indented t) cur-indent)
      (if (looking-at "\\s-*\\(endif\\|endwhile\\|else\\)")
          ;; If the line we are looking at
          ;; is the end of a block,
          ;; then decrease the indentation
          (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) 2)))
            (if (< cur-indent 0)        ; We can't indent past the left margin
                (setq cur-indent 0)))

        (save-excursion
          (while not-indented           ; Iterate backwards until
                                        ; we find an indentation hint
            (forward-line -1)
            (unless (looking-at "\\s-*\\*")
              (if (looking-at ".*\\b\\(endif\\|endwhile\\)")
                  ;; This hint indicates that we need to
                  ;; indent at the level of the endwhile/endif
                  (progn
                    (setq cur-indent (current-indentation))
                    (setq not-indented nil))
                (if (looking-at ".*\\b\\(if\\|while\\|else\\)")
                    ;; This hint indicates that we need to
                    ;; indent an extra level
                    (progn
                      (setq cur-indent
                            (+ (current-indentation) 2))
                      ;; Do the actual indenting
                      (setq not-indented nil)))))
            (if (bobp)
                (setq not-indented nil)))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))          ; If we didn't see an indentation hint,
                                        ; then allow no indentation

(defvar gs-mode-syntax-table nil
  "Syntax table for gs-mode.")

(defun gs-create-syntax-table ()
  (if gs-mode-syntax-table
      ()
    (setq gs-mode-syntax-table (make-syntax-table))

    ;; This is added so entity names with underscores and periods can be more easily parsed
    (modify-syntax-entry ?_ "w" gs-mode-syntax-table)

    ;; Comment syntax
    (modify-syntax-entry ?* "<" gs-mode-syntax-table)
    (modify-syntax-entry ?\n ">" gs-mode-syntax-table)

    ;; Quote syntax
    (modify-syntax-entry ?\' "\"" gs-mode-syntax-table)
    (modify-syntax-entry ?\" "\"" gs-mode-syntax-table))

  (set-syntax-table gs-mode-syntax-table))

;;;###autoload
(defun gs-mode ()
  "Major mode for editing GrADS script files."
  (interactive)
  (kill-all-local-variables)
  (gs-create-syntax-table)

  ;; Set up font-lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '(gs-font-lock-keywords))

  ;; Register our indentation function
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'gs-indent-line)

  (setq major-mode 'gs-mode)
  (setq mode-name "GS")
  (run-hooks 'gs-mode-hook))

(provide 'gs-mode)
;;; gs-mode.el ends here
