;;; gift-mode.el --- major mode for editing GIFT format quizzes

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

;; Copyright (C) 2017 Christophe Rhodes <csr21@cantab.net>

;; Author: Christophe Rhodes <christophe@rhodes.io>
;; URL: https://github.com/csrhodes/gift-mode
;; Package-Version: 20180204.1358
;; Version: 0.1

;;; Commentary:
;; 

;;; Code:

(require 'font-lock)
(require 'tex-mode)
(require 'outline)
(require 'newcomment)

(defconst gift-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)
    ;; maybe these should be in the font-lock-syntax-table instead?
    (modify-syntax-entry ?~ "_   " table)
    (modify-syntax-entry ?\\ "_   " table)
    table))

(defgroup gift nil "Mode for editing GIFT-format quiz questions"
  :tag "GIFT"
  :group 'wp)

(defface gift-keyword '((t (:inherit font-lock-keyword-face)))
  "keywords for GIFT quizzes")

(defface gift-category '((t (:inherit outline-1)))
  "category for GIFT quizzes")

(defface gift-question-name '((t (:inherit outline-2)))
  "question name in GIFT quizzes")

(defface gift-latex-math '((t (:inherit tex-math)))
  "math notation in GIFT quizzes")

(defface gift-wrong '((t (:inherit error)))
  "wrong answers in multiple-choice questions in GIFT quizzes")

(defface gift-right '((t (:inherit success)))
  "right answers in multiple-choice questions in GIFT quizzes")

(defface gift-wrong-credit '((t (:inherit gift-wrong :weight normal)))
  "credit for wrong answer in GIFT quizzes")

(defface gift-right-credit '((t (:inherit gift-right :weight normal)))
  "credit for right answer in GIFT quizzes")

(defface gift-feedback '((t (:inherit font-lock-string-face)))
  "feedback in GIFT quizzes")

;;; FIXME: I'd really like to highlight the background of right/wrong
;;; answers.  However, the GIFT grammar is more context-sensitive than
;;; simple font-lock search will let me deal with directly: it seems
;;; to be valid to use $$foo = bar$$ in the question body, but in the
;;; answers it needs to be $$foo \= bar$$.  (Only the second variant
;;; is context-free.)  Supporting both is probably possible with some
;;; multiline magic; supporting just the escaped version is
;;; straightforward but might be surprising to users.
(defvar gift-font-lock-keywords
  '(
    ("\\_<=\\(\\([^\\~=\n}#%]\\|\\\\[}~=#%]\\)*\\)\\(#\\(.*\\)\\)?" (1 'gift-right keep) (4 'gift-feedback keep t))
    ("{#\\(-?[0-9.:]+\\)" (1 'gift-right keep))
    ("\\_<~\\(%\\([0-9.]+\\)%\\)\\(\\([^\\~=%}\n#]\\|\\\\[A-Za-z0-9}~=#%]\\)*\\)\\(#\\(.*\\)\\)?" (2 'gift-right-credit) (3 'gift-right keep) (6 'gift-feedback keep t))
    ("\\_<~\\(%\\(-[0-9.]+\\)%\\)\\(\\([^\\~=%}\n#]\\|\\\\[A-Za-z0-9}~=#%]\\)*\\)\\(#\\(.*\\)\\)?" (2 'gift-wrong-credit) (3 'gift-wrong keep) (6 'gift-feedback keep t))
    ("\\_<~\\(\\([^\\~=%}\n#]\\|\\\\[}~=#%]\\)*\\)\\(#\\(.*\\)\\)?" (1 'gift-wrong keep) (4 'gift-feedback keep t))
    ("\\$\\$\\([^$]\\|\\\\$\\)*[^\\]\\$\\$" (0 'gift-latex-math t)) ; doesn't handle \$$$O(n)$$ correctly; think about font-lock-multiline
    ("\\(\\$CATEGORY\\):\s-*\\(\\$course\\$/?\\|\\)\\(.*?\\)\\(//\\|$\\)" (1 'gift-keyword) (2 'gift-keyword) (3 'gift-category))
    ("::\\([^:]\\|\\\\:\\)+::" . 'gift-question-name)))

(defvar gift-credit '("-100" "-50" "-33.3333" "-25" "-20" "-16.6667" "16.6667" "20" "25" "33.3333" "50" "100"))

(defun gift-decrease-credit ()
  (interactive)
  (save-excursion
    (save-match-data
      (let ((bol (save-excursion (beginning-of-line) (point)))
	    (eol (save-excursion (end-of-line) (point))))
	(if (search-forward-regexp "~%\\(-?[0-9.]+\\)%" eol t)
	    (let* ((current (match-string 1))
		   (cpos (position current gift-credit :test 'equal)))
	      (when (and cpos (> cpos 0))
		(replace-match (elt gift-credit (1- cpos)) t t nil 1)))
	  (beginning-of-line)
	  (when (search-forward-regexp "~%\\(-?[0-9.]+\\)%" eol t)
	    (let* ((current (match-string 1))
		   (cpos (position current gift-credit :test 'equal)))
	      (when (and cpos (> cpos 0))
		(replace-match (elt gift-credit (1- cpos)) t t nil 1)))))))))

(defun gift-increase-credit ()
  (interactive)
  (save-excursion
    (save-match-data
      (let ((bol (save-excursion (beginning-of-line) (point)))
	    (eol (save-excursion (end-of-line) (point))))
	(if (search-forward-regexp "~%\\(-?[0-9.]+\\)%" eol t)
	    (let* ((current (match-string 1))
		   (cpos (position current gift-credit :test 'equal)))
	      (when (and cpos (< cpos (1- (length gift-credit))))
		(replace-match (elt gift-credit (1+ cpos)) t t nil 1)))
	  (beginning-of-line)
	  (when (search-forward-regexp "~%\\(-?[0-9.]+\\)%" eol t)
	    (let* ((current (match-string 1))
		   (cpos (position current gift-credit :test 'equal)))
	      (when (and cpos (< cpos (1- (length gift-credit))))
		(replace-match (elt gift-credit (1+ cpos)) t t nil 1)))))))))

;;;###autoload
(define-derived-mode gift-mode text-mode "GIFT"
  "Major mode for editing GIFT format quizzes.
\\{gift-mode-map}"
  :group 'gift
  :syntax-table gift-mode-syntax-table
  (setq font-lock-defaults (list '(gift-font-lock-keywords) nil))
  (set (make-local-variable 'outline-regexp) "\\(\\$CATEGORY\\|::\\)")
  (set (make-local-variable 'outline-heading-end-regexp)
       "\\(\\$CATEGORY.*\n\\|::[^:]+::\\(.\\|\n\\)\\)")
  (set (make-local-variable 'outline-heading-alist)
       '(("$CATEGORY" . 1) ("::" . 2)))
  (set (make-local-variable 'comment-start) "//"))

(define-key gift-mode-map (kbd "<C-left>") 'gift-decrease-credit)
(define-key gift-mode-map (kbd "<C-right>") 'gift-increase-credit)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gift\\'" . gift-mode))

(provide 'gift-mode)
;;; gift-mode.el ends here
