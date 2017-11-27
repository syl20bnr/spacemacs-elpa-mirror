;;; gle-mode.el --- Major mode to edit Graphics Layout Engine files  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Free Software Foundation, Inc

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Package-Requires: ((cl-lib "0.5"))
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major mode for files using the GLE (Graphics Layout Engine)
;; language.  See http://glx.sourceforge.net/
;; [ Not sure why the site uses "glx", while everything else seems to use
;;   "gle" instead.  ]

;; It provides:
;; - Rudimentary code highlighting.
;; - Automatic indentation.
;; - Flymake support (requires Emacs-26's fymake).
;; - Imenu support

;;;; TODO
;; - Fix highlighting of function calls?
;; - provide a completion-at-point-function
;; - auto-complete the `end`s and `next`s

;;; Code:

(require 'smie)
(require 'cl-lib)

(defgroup 'gle-mode ()
  "Major mode for GLE (Graphics Layout Engine) files."
  :group 'tools)

;;;; Syntax table

(defvar gle-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?! "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    ;; Backslash isn't used to escape a double quote in a string.
    (modify-syntax-entry ?\\ "." st)
    st))

(defconst gle-syntax-propertize
  (syntax-propertize-rules
   ;; The doc says that doubled quotes are used to escape quotes in a string,
   ;; tho running gle-4.2.5 on those strings gives me errors :-(
   ("\"\"\\|''"
    (0 (if (save-excursion (nth 3 (syntax-ppss (match-beginning 0))))
           (string-to-syntax ".")
         ;; If match-beg is not within a string, maybe it starts a string,
         ;; and maybe the second " doesn't end the string!
         (goto-char (1+ (match-beginning 0)))
         nil)))))

;;;; SMIE support

(defvar gle-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((var)
       (exp)
       (inst-else-inst (inst) (inst "else bloc" inst))
       (for-body (exp ";" inst))
       (var=exp (var "=" exp))
       (for-head (var=exp "to" exp-step))
       (exp-step (exp) (exp "step" exp))
       (until-body (exp ";" inst))
       (inst (inst ";" inst)
             ("begin" inst "end <thing>")
             ;; You can have "single-line" ifs (with inst right after "then"),
             ;; which can be extended with single line "else if"s.
             ;; Or you can have "if ... end if" blocs.
             ("if bloc" inst-else-inst "end if")
             ("sub" inst "end sub")
             ("for" for-body "next <var>")
             ("until" until-body "next")
             ("while" until-body "next")
             ("gsave" inst "grestore")))
     '((assoc ";"))))))

(defun gle-smie--disambiguate-if ()
  "Expects point to be right after `if`."
  (save-excursion
    (let ((eol (line-end-position))
          (lasttok nil)
          (tok nil))
      (while (<= (point) eol)
        (setq lasttok tok)
        (forward-comment (point-max))
        (setq tok (buffer-substring (point)
                                    (progn
                                      (or (> (skip-syntax-forward "w_") 0)
                                          (> (skip-syntax-forward ".") 0)
                                          (forward-char 1))
                                      (point)))))
      (if (equal lasttok "then")
          "if bloc"
        "if line"))))

(defun gle-smie-forward-token ()
  (let ((start (point)))
    (forward-comment (point-max))
    (if (and (not (eq ?\n (char-before start)))
             (< start (line-beginning-position)))
        (progn (goto-char start)
               (forward-line 1)
               ";")
      (let ((bolp (save-excursion
                    (skip-chars-backward " \t")
                    (memq (char-before) '(nil ?\n ?\;)))))
        (if (not bolp)
            (cond
             ((not (zerop (skip-chars-forward "^ \t\n;!"))) "<exp>")
             ((eobp) "")
             (t (cl-assert (looking-at ";"))
                (forward-char 1)
                ";"))
          (let ((tok (buffer-substring (point)
                                       (progn (skip-chars-forward "^ \t\n;!")
                                              (point)))))
            (cond
             ((looking-at "[ \t]*=") "<var>")
             ((equal tok "end")
              (cond
               ((looking-at "[ \t]+sub") (goto-char (match-end 0)) "end sub")
               ((looking-at "[ \t]+if") (goto-char (match-end 0)) "end if")
               ((looking-at "[ \t]+\\w+")
                (goto-char (match-end 0)) "end <thing>")
               (t tok)))
             ((equal tok "next")
              (cond
               ((looking-at "[ \t]+\\w+")
                (goto-char (match-end 0)) "next <var>")
               (t tok)))
             ((equal tok "if") (gle-smie--disambiguate-if))
             ((equal tok "else")
              (if (looking-at "[ \t]+if") "else line" "else bloc"))
             (t tok))))))))
              
(defun gle-smie-backward-token ()
  (let ((start (point)))
    (forward-comment (- (point)))
    (if (> start (line-end-position))
        ";"
      (let* ((end (point))
             (assign (looking-at "[ \t]*="))
             (tok (buffer-substring (progn (skip-chars-backward "^ \t\n;")
                                           (point))
                                    end))
             (bolp (save-excursion
                     (skip-chars-backward " \t")
                     (memq (char-before) '(nil ?\n ?\;)))))
        (cond
         ((and bolp (equal tok ""))
          (if (bobp) tok
            (cl-assert (eq (char-before) ?\;))
            (forward-char -1)
            ";"))
         (assign "<var>")
         (bolp
          (cond
           ((equal tok "if")
            (save-excursion (forward-char 2) (gle-smie--disambiguate-if)))
           ((equal tok "else")
            (if (looking-at "else[ \t]+if") "else line" "else bloc"))
           (t tok)))
         ((save-excursion
            (skip-chars-backward " \t")
            (and (memq (char-before) '(?d ?t))
                 (looking-back "^[ \t]*\\(end\\|nex\\(t\\)\\)"
                               (line-beginning-position))))
          (goto-char (match-beginning 1))
          (cond
           ((match-beginning 2) "next <var>")
           ((equal tok "sub") "end sub")
           ((equal tok "if") "end if")
           (t "end <thing>")))
         (t "<exp>"))))))

(defun gle-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:after . ";")
     (cond
      ((smie-rule-parent-p "for" "while" "sub" "begin" "gsave" "if bloc")
       (smie-rule-parent smie-indent-basic))))
    (`(:before . "else bloc") (smie-rule-parent 0))))

;;;; Font-lock

(defvar gle-font-lock-keywords
  '(("^[ \t]*\\(\\(?:\\sw\\|\\s_\\)+\\)[ \t]*="
     (1 font-lock-variable-name-face))
    ("^[ \t]*if[ \t][^!\n;]*[ \t]\\(then\\)\\_>"
     (1 font-lock-keyword-face))
    ("^[ \t]*for[ \t][^!\n;]*[ \t]\\(to\\)\\_>\\(?:[^!\n;]*[ \t]\\(step\\)\\_>\\)?"
     (1 font-lock-keyword-face) (2 font-lock-keyword-face nil t))
    ("^[ \t]*else[ \t]+\\(if\\)[ \t][^!\n;]*[ \t]\\(then\\)\\_>"
     (1 font-lock-keyword-face) (2 font-lock-keyword-face))
    ("^[ \t]*end[ \t]+\\(\\(?:\\sw\\|\\s_\\)+\\)"
     (1 font-lock-keyword-face))
    ("^[ \t]*sub[ \t]*\\(\\(?:\\sw\\|\\s_\\)+\\)"
     (1 font-lock-function-name-face))
    ;; FIXME: Actually, this can also be a function call!
    ("^[ \t]*\\(\\(?:\\sw\\|\\s_\\)+\\)" (1 font-lock-keyword-face))))

;;;; Flymake

(defcustom gle-program-name "gle"
  "Name of the `gle' program."
  :type 'string)

(defvar-local gle--flymake-proc nil)

(defun gle--flymake (report-fn &rest _args)
  "GLE backend for Flymake.
See `flymake-diagnostic-functions' for documentation of REPORT-FN."
  ;; Code largely inspired from `ruby-flymake'.
  (unless (executable-find gle-program-name)
    (error "Cannot find `gle' executable"))

  (when (process-live-p gle--flymake-proc)
    (delete-process gle--flymake-proc))

  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq
       gle--flymake-proc
       (make-process
        :name "gle-flymake" :noquery t :connection-type 'pipe
        :buffer (generate-new-buffer " *gle-flymake*")
        :command (list gle-program-name "-nosave" "-")
        :sentinel
        (lambda (proc _event)
          (when (eq 'exit (process-status proc))
            (let ((diagnostics '()))
              (unwind-protect
                  (if (with-current-buffer source
                        (not (eq proc gle--flymake-proc)))
                      (message "Skipping obsolete check for %s" proc)
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-min))
                      (while (search-forward-regexp
                              "^>> .*? (\\([0-9]+\\)) |\\(.*\\)|\n>>.*\n>> *\\(\\w+\\): *\\(.*\\)"
                              nil t)
                        (let ((line (string-to-number (match-string 1)))
                              (txt (match-string 2))
                              (kind (intern
                                     (format ":%s"
                                             (downcase (match-string 3)))))
                              (msg (match-string 4)))
                          (with-current-buffer source
                            (save-excursion
                              (goto-char (point-min))
                              (forward-line (1- line))
                              (push
                               (if (search-forward txt (line-end-position) t)
                                   (flymake-make-diagnostic source
                                                            (match-beginning 0)
                                                            (match-end 0)
                                                            kind
                                                            msg)
                                 (skip-chars-forward " \t")
                                 (flymake-make-diagnostic source
                                                          (point)
                                                          (line-end-position)
                                                          kind
                                                          msg))
                               diagnostics)))))))
                (kill-buffer (process-buffer proc)))
              (funcall report-fn diagnostics))))))
      (process-send-region gle--flymake-proc (point-min) (point-max))
      (process-send-eof gle--flymake-proc))))

;;;; Imenu

(defvar gle-imenu-generic-expression
  '(("Funs" "^[ \t]*sub[ \t]+\\(\\(?:\\s_\\|\\sw\\)+\\)" 1)
    ("Vars" "^[ \t]*\\(\\(?:\\s_\\|\\sw\\)+\\)[ \t]*=" 1)))

;;;; Top-level

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gle\\'" . gle-mode))

;;;###autoload
(define-derived-mode gle-mode prog-mode "GLE"
  "Major mode to edit Graphics Layout Engine files."
  (setq-local comment-start "!")
  (setq-local syntax-propertize-function gle-syntax-propertize)
  (smie-setup gle-smie-grammar #'gle-smie-rules
              :forward-token #'gle-smie-forward-token
              :backward-token #'gle-smie-backward-token)
  (setq-local font-lock-defaults
              '(gle-font-lock-keywords))
  (setq-local imenu-generic-expression gle-imenu-generic-expression)
  (add-hook 'flymake-diagnostic-functions 'gle--flymake nil 'local)
  )

;;;; ChangeLog:

;; 2017-11-26  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Add gle-mode package
;; 


(provide 'gle-mode)
;;; gle-mode.el ends here
