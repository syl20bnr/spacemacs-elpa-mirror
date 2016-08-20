;;; gildas-mode.el --- Major mode for Gildas

;; Copyright (C) 2014-2015 Sébastien Maret

;; Author: Sébastien Maret <sebastien.maret@icloud.com>
;; Package-Requires: ((polymode "0") (emacs "24.3"))
;; Package-Version: 20150919.1501
;; Keywords: languages, gildas
;; URL: https://github.com/smaret/gildas-mode
;; Version: 1

;;; Commentary:

;; This package provides a major mode to edit Gildas scripts.

;;; License:

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

;;; Code:

(require 'polymode)

(defvar gildas-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\! "<" st) ; begin comment
    (modify-syntax-entry ?\n ">" st) ; end comment
    (modify-syntax-entry ?_ "w")     ; variables, functions, etc.
    st)
  "Syntax table for `gildas-mode'.")

(defconst gildas-keywords
  (regexp-opt '("\@" "begin" "break" "continue" "else" "exit" "end" "endif"
		"for" "if" "next" "on" "pause" "python" "quit" "return"
		"accept" "compute" "define" "delete" "examine" "import" "let"
		"message" "mfit" "next" "parse" "say" "sic" "sort" "symbol"
		"system" "then" "to") 'words)
  "Keywords for `gildas-mode'.")

(defconst gildas-types
  (regexp-opt '("real" "integer" "double" "logical" "character" "table"
		"header" "image" "uvtable" "structure" "fits" "alias"
		"procedure" "help" "data") 'words)
  "Types for `gildas-mode'.")

(defconst gildas-constants
  (regexp-opt '("no" "pi" "sec" "yes") 'words)
  "Constants for `gildas-mode'.")

(defvar gildas-font-lock-keywords
  `((,gildas-keywords . font-lock-keyword-face)
    (,gildas-types . font-lock-type-face)
    (,gildas-constants . font-lock-constant-face)
    ("procedure \\(\\sw+\\)" (1 font-lock-function-name-face))
    ("let \\(\\sw+\\)" (1 font-lock-variable-name-face))
    ("define \\sw+ \\(\\sw+\\)" (1 font-lock-variable-name-face)))
  "Keyword highlighting specification for `gildas-mode'.")

(define-derived-mode gildas-mode prog-mode "Gildas"
  "Major mode for Gildas."
  :syntax-table gildas-mode-syntax-table
  (setq-local comment-start "!")
  (setq-local comment-start-skip "!+ *")
  (setq-local font-lock-defaults
	      '(gildas-font-lock-keywords))
  (setq-local indent-line-function
	      'gildas-indent-line))

(defvar gildas-indent-offset
  4
  "Indentation offset for Gildas.")

(defun gildas-indent-line ()
  "Indent current line as Gildas code"
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*\\(endif\\|next\\|end procedure\\)")
	  (progn
	    (save-excursion
	      (forward-line -1)
	      (setq cur-indent (- (current-indentation) gildas-indent-offset)))
	    (if (< cur-indent 0)
		(setq cur-indent 0)))
	      (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[ \t]*\\(endif\\|next\\|end procedure\\)")
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              (if (looking-at "^[ \t]*\\(if .+ then\\|for\\|begin procedure\\)")
                  (progn
                    (setq cur-indent (+ (current-indentation) gildas-indent-offset))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))))))
      (if cur-indent
	  (indent-line-to cur-indent)
	(indent-line-to 0)))))

(defcustom pm-host/gildas
  (pm-bchunkmode "gildas"
		 :mode 'gildas-mode)
  "Gildas host chunkmode"
  :group 'hostmodes
  :type 'object)

(defcustom  pm-inner/pygildas
  (pm-hbtchunkmode "python"
                   :head-reg "^begin .*\.py"
                   :tail-reg "^end .*\.py"
		   :mode 'python-mode)
  "Python chunk."
  :group 'innermodes
  :type 'object)

(defcustom pm-poly/gildas
  (pm-polymode-one "pygildas"
                   :hostmode 'pm-host/gildas
                   :innermode 'pm-inner/pygildas)
  "Polymode for Gildas."
  :group 'polymodes
  :type 'object)

(define-polymode poly-gildas-mode pm-poly/gildas)

;;;###autoload
(progn
  (autoload 'poly-gildas-mode "gildas-mode" "" t)
  (add-to-list 'auto-mode-alist '("\\.astro\\'" . poly-gildas-mode))
  (add-to-list 'auto-mode-alist '("\\.class\\'" . poly-gildas-mode))
  (add-to-list 'auto-mode-alist '("\\.greg\\'" . poly-gildas-mode))
  (add-to-list 'auto-mode-alist '("\\.map\\'" . poly-gildas-mode))
  (add-to-list 'auto-mode-alist '("\\.sic\\'" . poly-gildas-mode)))

(provide 'gildas-mode)
;;; gildas-mode.el ends here
