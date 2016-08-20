;;; crappy-jsp-mode.el --- A pretty crappy major-mode for jsp.

;; Copyright (C) 2011 Magnar Sveen, Jostein Holje

;; Authors: Magnar Sveen <magnars@gmail.com>
;;          Jostein Holje <jostein.holje@gmail.com>
;; Keywords: jsp major mode
;; Package-Version: 20140311.231

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

;; An exceedingly basic jsp-mode, inheriting from html-mode.
;;
;; This crappy mode for jsp makes sure indentation sorta works.
;;
;; ## Indentation
;;
;; It gives you proper indentation when you need to comment out
;; whitespace with jsp-comments:
;;
;;     <div class="no-whitespace"><%--
;;       --%><div class="please"><%--
;;       --%></div><%--
;;     --%></div><%--
;;
;; or maybe not even properly, just nicer than html-mode.
;;
;; It also indents JavaScript inside `<script>`-tags.
;;
;; ## Highlighting
;;
;; In addition to that it highlights these `${}` in a horrible yellow
;; color.

;;; Code:

(require 'sgml-mode)
(require 'js)

(defvar cjsp-el-expr-face 'cjsp-el-expr-face
  "Face name to use for jstl el-expressions.")
(defface cjsp-el-expr-face
  '((((class color)) (:foreground "#FFFF00"))
    (t (:foreground "FFFF00")))
  "Face for jstl el-expressions.")

(defvar cjsp-font-lock-keywords
      (append
       sgml-font-lock-keywords-2
       (list
        (cons "\${[^}]+}" '(0 cjsp-el-expr-face t t))
        (cons "{{[^}}]+}}" '(0 cjsp-el-expr-face t t))
        )))

(defvar cjsp--script-tag-re
  "<script\\( type=\"text/javascript\"\\)?>")

(defun cjsp--in-script-tag (lcon)
  (and (eq (car lcon) 'text)
       (cdr lcon)
       (save-excursion
         (goto-char (cdr lcon))
         (looking-back cjsp--script-tag-re))))

(defun cjsp--in-pre-tag (lcon)
  (and (eq (car lcon) 'text)
       (cdr lcon)
       (save-excursion
         (goto-char (cdr lcon))
         (looking-back "<pre\\( [^>]*\\)?>\\(<code\\( [^>]*\\)?>\\)?"))))

(defun cjsp--script-indentation ()
  (if (or (looking-back (concat cjsp--script-tag-re "[\n\t ]+"))
          (looking-at "</script>"))
      (sgml-calculate-indent)
    (max (js--proper-indentation (save-excursion
                                   (syntax-ppss (point-at-bol))))
         (sgml-calculate-indent))))

(defun cjsp--in-jsp-comment (lcon)
  (and (eq (car lcon) 'tag)
       (looking-at "--%")
       (save-excursion (goto-char (cdr lcon)) (looking-at "<%--"))))

(defun cjsp--jsp-comment-indentation ()
  (forward-char 4)
  (max 0 (- (sgml-calculate-indent) 4)))

(defun jsp-calculate-indent (&optional lcon)
  (unless lcon (setq lcon (sgml-lexical-context)))
  (cond
   ((cjsp--in-pre-tag lcon)     nil) ; don't change indent in pre
   ((cjsp--in-script-tag lcon)  (cjsp--script-indentation))
   ((cjsp--in-jsp-comment lcon) (cjsp--jsp-comment-indentation))
   (t                           (sgml-calculate-indent lcon))))

(defun jsp-indent-line ()
  "Indent the current line as jsp."
  (interactive)
  (let* ((savep (point))
         (indent-col
          (save-excursion
            (back-to-indentation)
            (if (>= (point) savep) (setq savep nil))
            (jsp-calculate-indent))))
    (if (null indent-col)
        'noindent
      (if savep
          (save-excursion (indent-line-to indent-col))
        (indent-line-to indent-col)))))

(eval-after-load 'expand-region
  '(add-to-list 'expand-region-exclude-text-mode-expansions 'crappy-jsp-mode))

(define-derived-mode crappy-jsp-mode
  html-mode "Crappy JSP"
  "Major mode for jsp.
          \\{jsp-mode-map}"
  (setq indent-line-function 'jsp-indent-line)
  (setq font-lock-defaults '((cjsp-font-lock-keywords) nil t)))

(provide 'crappy-jsp-mode)

;;; crappy-jsp-mode.el ends here
