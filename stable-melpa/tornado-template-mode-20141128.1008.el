;;; tornado-template-mode.el --- A major mode for editing tornado templates

;; Copyright (C) 2014 Florian Mounier aka paradoxxxzero

;; Author: Florian Mounier aka paradoxxxzero
;; Version: 0.2
;; Package-Version: 20141128.1008

;; This program is free software: you can redistribute it and/or modify
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

;;   This is an emacs major mode for tornado-template with:
;;        syntax highlighting
;;        sgml/html integration
;;        indentation (working with sgml)
;;        more to come

;; This file comes from http://github.com/paradoxxxzero/tornado-template-mode

;;; Code:

(require 'sgml-mode)

(defgroup tornado-template nil
  "Major mode for editing tornado-template code."
  :prefix "tornado-template-"
  :group 'languages)

(defcustom tornado-template-user-keywords nil
  "Custom keyword names"
  :type '(repeat string)
  :group 'tornado-template)

;; (defcustom tornado-template-debug nil
;;   "Log indentation logic"
;;   :type 'boolean
;;   :group 'tornado-template)

(defun tornado-template-closing-keywords ()
  (append
   tornado-template-user-keywords
   '("apply" "block" "for" "if" "try" "while")))

(defun tornado-template-closing-and-reopening-keywords ()
  '("else" "elif" "except" "finally" "end"))

(defun tornado-template-indenting-keywords ()
  (append
   (tornado-template-closing-keywords)
   (tornado-template-closing-and-reopening-keywords)
   ))

(defun tornado-template-builtin-keywords ()
  '("autoescape" "comment" "extends" "from"
    "import" "include" "module" "raw" "set"))


(defun tornado-template-find-open-tag ()
  (if (search-backward-regexp
       (rx-to-string
        `(and "{%"
              (* whitespace)
              (? (group
                  "end"))
              (group
               ,(append '(or)
                        (tornado-template-closing-keywords)
                        ))
              (group
               (*? anything))
              (* whitespace)
              "%}")) nil t)
      (if (match-string 1) ;; End tag, going on
          (let ((matches (tornado-template-find-open-tag)))
            (if (string= (car matches) (match-string 2))
                (tornado-template-find-open-tag)
              (list (match-string 2) (match-string 3))))
        (list (match-string 2) (match-string 3)))
    nil))

(defun tornado-template-close-tag ()
  "Close the previously opened template tag."
  (interactive)
  (let ((open-tag (save-excursion (tornado-template-find-open-tag))))
    (if open-tag
        (insert "{% end %}")
      (error "Nothing to close")))
  (save-excursion (tornado-template-indent-line)))

(defun tornado-template-insert-tag ()
  "Insert an empty tag"
  (interactive)
  (insert "{% ")
  (save-excursion
    (insert " %}")
    (tornado-template-indent-line)))

(defun tornado-template-insert-var ()
  "Insert an empty tag"
  (interactive)
  (insert "{{ ")
  (save-excursion
    (insert " }}")
    (tornado-template-indent-line)))

(defun tornado-template-insert-comment ()
  "Insert an empty tag"
  (interactive)
  (insert "{# ")
  (save-excursion
    (insert " #}")
    (tornado-template-indent-line)))

(defconst tornado-template-font-lock-comments
  `(
    (,(rx "{#"
          (* whitespace)
          (group
           (*? anything)
           )
          (* whitespace)
          "#}")
     . (1 font-lock-comment-face t))))

(defconst tornado-template-font-lock-keywords-1
  (append
   tornado-template-font-lock-comments
   sgml-font-lock-keywords-1))

(defconst tornado-template-font-lock-keywords-2
  (append
   tornado-template-font-lock-keywords-1
   sgml-font-lock-keywords-2))

(defconst tornado-template-font-lock-keywords-3
  (append
   tornado-template-font-lock-keywords-1
   tornado-template-font-lock-keywords-2
   `(
     (,(rx "{{"
           (* whitespace)
           (group
            (*? anything)
            )
           (*
            "|" (* whitespace) (*? anything))
           (* whitespace)
           "}}") (1 font-lock-variable-name-face t))
     (,(rx  (group "|" (* whitespace))
            (group (+ word))
            )
      (1 font-lock-keyword-face t)
      (2 font-lock-warning-face t))
     (,(rx-to-string `(and word-start
                           ,(append '(or)
                                    (tornado-template-indenting-keywords)
                                    )
                           word-end)) (0 font-lock-keyword-face))
     (,(rx-to-string `(and word-start "end"
                           word-end)) (0 font-lock-keyword-face))
     (,(rx-to-string `(and word-start
                           ,(append '(or)
                                    (tornado-template-builtin-keywords)
                                    )
                           word-end)) (0 font-lock-builtin-face))

     (,(rx (or "{%" "%}")) (0 font-lock-function-name-face t))
     (,(rx (or "{{" "}}")) (0 font-lock-type-face t))
     (,(rx "{#"
           (* whitespace)
           (group
            (*? anything)
            )
           (* whitespace)
           "#}")
      (1 font-lock-comment-face t))
     (,(rx (or "{#" "#}")) (0 font-lock-comment-delimiter-face t))
     )))

(defvar tornado-template-font-lock-keywords
  tornado-template-font-lock-keywords-1)

(defun sgml-indent-line-num ()
  "Indent the current line as SGML."
  (let* ((savep (point))
         (indent-col
          (save-excursion
            (back-to-indentation)
            (if (>= (point) savep) (setq savep nil))
            (sgml-calculate-indent))))
    (if (null indent-col)
        0
      (if savep
          (save-excursion indent-col)
        indent-col))))

(defun tornado-template-calculate-indent-backward (default)
  "Return indent column based on previous lines"
  (let ((indent-width sgml-basic-offset) (default (sgml-indent-line-num)))
    (forward-line -1)
    (if (looking-at "^[ \t]*{% *end") ; Don't indent after end
        (current-indentation)
      (if (looking-at "^[ \t]*{% *.*?{% *end *%}")
          (current-indentation)
        (if (looking-at (concat "^[ \t]*{% *" (regexp-opt (tornado-template-indenting-keywords)))) ; Check start tag
            (+ (current-indentation) indent-width)
          (if (looking-at "^[ \t]*<") ; Assume sgml block trust sgml
              default
            (if (bobp)
                0
              (tornado-template-calculate-indent-backward default))))))))


(defun tornado-template-calculate-indent ()
  "Return indent column"
  (if (bobp)  ; Check begining of buffer
      0
    (let ((indent-width sgml-basic-offset) (default (sgml-indent-line-num)))
      (if (looking-at (concat "^[ \t]*{% *" (regexp-opt (tornado-template-closing-and-reopening-keywords)))) ; Check close tag
          (save-excursion
            (forward-line -1)
            (if
                (and
                 (looking-at (concat "^[ \t]*{% *" (regexp-opt (tornado-template-indenting-keywords))))
                 (not (looking-at (concat "^[ \t]*{% *.*?{% *end *%}"))))
                (current-indentation)
              (- (current-indentation) indent-width)))
        (if (looking-at "^[ \t]*</") ; Assume sgml end block trust sgml
            default
          (save-excursion
            (tornado-template-calculate-indent-backward default)))))))

(defun tornado-template-indent-line ()
  "Indent current line as tornado code"
  (interactive)
  (let ((old_indent (current-indentation)) (old_point (point)))
    (move-beginning-of-line nil)
    (let ((indent (max 0 (tornado-template-calculate-indent))))
      (indent-line-to indent)
      (if (< old_indent (- old_point (line-beginning-position)))
          (goto-char (+ (- indent old_indent) old_point)))
      indent)))


;;;###autoload
(define-derived-mode tornado-template-mode html-mode  "Tornado-Template"
  "Major mode for editing tornado-template files"
  :group 'tornado-template
  ;; Disabling this because of this emacs bug:
  ;;  http://lists.gnu.org/archive/html/bug-gnu-emacs/2002-09/msg00041.html
  ;; (modify-syntax-entry ?\'  "\"" sgml-mode-syntax-table)
  (set (make-local-variable 'comment-start) "{#")
  (set (make-local-variable 'comment-start-skip) "{#")
  (set (make-local-variable 'comment-end) "#}")
  (set (make-local-variable 'comment-end-skip) "#}")
  ;; it mainly from sgml-mode font lock setting
  (set (make-local-variable 'font-lock-defaults)
       '((
          tornado-template-font-lock-keywords
          tornado-template-font-lock-keywords-1
          tornado-template-font-lock-keywords-2
          tornado-template-font-lock-keywords-3)
         nil t nil nil
         (font-lock-syntactic-keywords
          . sgml-font-lock-syntactic-keywords)))
  (set (make-local-variable 'indent-line-function) 'tornado-template-indent-line))

(define-key tornado-template-mode-map (kbd "C-c c") 'tornado-template-close-tag)
(define-key tornado-template-mode-map (kbd "C-c t") 'tornado-template-insert-tag)
(define-key tornado-template-mode-map (kbd "C-c v") 'tornado-template-insert-var)
(define-key tornado-template-mode-map (kbd "C-c #") 'tornado-template-insert-comment)

(provide 'tornado-template-mode)

;;; tornado-template-mode.el ends here
