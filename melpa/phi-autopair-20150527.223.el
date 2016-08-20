;;; phi-autopair.el --- another simple-minded autopair implementation

;; Copyright (C) 2013-2015 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Package-Version: 20150527.223
;; Version: 1.0.0
;; Package-Requires: ((paredit "20"))

;;; Commentary:

;; This script provides a minor-mode phi-autopair-mode, that
;; inserts/deletes parens automatically.
;;
;; Put this script and "paredit.el" in a "load-path"ed direcctory,
;; and then
;;
;;   (require 'phi-autopair)
;;
;; You can enable phi-autopair-mode locally by calling command
;; "phi-autopair-mode", or globally by calling
;; "phi-autopair-global-mode" instead.
;;
;;   (phi-autopair-global-mode)
;;
;; See Readme.org for more informations.

;;; Change Log:

;; 1.0.0 first release

;;; Code:

(defconst phi-autopair-version "1.0.0")

;; + dependencies

;; do not load but just locate "paredit.el" so that we can load
;; "paredit" lazily.
(if (not (locate-library "paredit"))
    (error "Cannot open load file: paredit.el")
  (autoload 'paredit-splice-sexp-killing-forward "paredit")
  (autoload 'paredit-splice-sexp-killing-backward "paredit"))

;; + customs

(defgroup phi-autopair nil
  "another simple-minded autopair implementation."
  :group 'emacs)

(defcustom phi-autopair-lispy-modes
  '(lisp-mode emacs-lisp-mode lisp-interaction-mode
              scheme-mode gauche-mode racket-mode
              clojure-mode egison-mode)
  "list of major-modes for lisp-like languages"
  :group 'phi-autopair)

(defcustom phi-autopair-auto-insert-pairs t
  "when non-nil, \"(\" also inserts \")\"."
  :group 'phi-autopair)

(defcustom phi-autopair-auto-wrap-region t
  "when non-nil, the region is wrapped with \"(\", if the mark is
  active."
  :group 'phi-autopair)

(defcustom phi-autopair-auto-delete-pairs t
  "when non-nil, deleting paren from inside also deletes the
  partner paren."
  :group 'phi-autopair)

(defcustom phi-autopair-auto-delete-spaces t
  "when non-nil, deletion commands deletes all adjacent
whitespaces at a time."
  :group 'phi-autopair)

(defcustom phi-autopair-auto-delete-escape t
  "when non-nil, deletion commands deletes both escape character
  and escaped character at a time."
  :group 'phi-autopair)

(defcustom phi-autopair-cautious-close t
  "when non-nil, \")\" does not inserts \")\" but \"))\" does."
  :group 'phi-autopair)

(defcustom phi-autopair-cautious-delete nil
  "when non-nil, deletion commands never delete parens by one."
  :group 'phi-autopair)

(defcustom phi-autopair-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap delete-char] 'phi-autopair-delete-forward)
    (define-key map [remap hungry-delete] 'phi-autopair-delete-forward)
    (define-key map [remap delete-backward-char] 'phi-autopair-delete-backward)
    (define-key map [remap backward-delete-char] 'phi-autopair-delete-backward)
    (define-key map [remap backward-delete-char-untabify] 'phi-autopair-delete-backward)
    (define-key map [remap backward-kill-word] 'phi-autopair-delete-backward-word)
    (define-key map [remap kill-word] 'phi-autopair-delete-forward-word)
    map)
  "keymap for phi-autopair-mode buffers"
  :group 'phi-autopair)

;; + internal vars

(defvar phi-autopair--pairs nil
  "list of (OPENING-CHAR . (TYPE . CLOSING-STR)).")
(make-variable-buffer-local 'phi-autopair--pairs)

(defun phi-autopair--setup ()
  "setup keybinds and phi-autopair--pairs from the syntax-table"
  (setq phi-autopair--pairs nil)
  (let ((table (syntax-table))
        (open (car (string-to-syntax "(")))
        (paired (car (string-to-syntax "$")))
        (string (car (string-to-syntax "\""))))
    (while table
      (map-char-table
       (lambda (char entry)
         (let ((class (syntax-class entry)))
           (cond ((eq class paired)
                  (add-to-list 'phi-autopair--pairs
                               `(,char pair . ,(char-to-string char)))
                  (define-key phi-autopair-mode-map
                    (char-to-string char) 'phi-autopair-open))
                 ((eq class open)
                  (when (cdr entry)
                    (add-to-list 'phi-autopair--pairs
                                 `(,char pair . ,(char-to-string (cdr entry))))
                    (define-key phi-autopair-mode-map
                      (char-to-string char) 'phi-autopair-open)
                    (define-key phi-autopair-mode-map
                      (char-to-string (cdr entry)) 'phi-autopair-close)))
                 ((eq class string)
                  (add-to-list 'phi-autopair--pairs
                               `(,char string . ,(char-to-string char)))
                  (define-key phi-autopair-mode-map
                    (char-to-string char) 'phi-autopair-open)))))
       table)
      (setq table (char-table-parent table)))))

;; + minor-mode

(define-minor-mode phi-autopair-mode
  "another simple-minded autopair implementation."
  :init-value nil
  :keymap phi-autopair-mode-map
  (if phi-autopair-mode
      (progn
        (add-hook 'after-change-major-mode-hook 'phi-autopair--setup nil t)
        (phi-autopair--setup))
    (remove-hook 'after-change-major-mode-hook 'phi-autopair--setup t)))

(define-globalized-minor-mode phi-autopair-global-mode
  phi-autopair-mode
  (lambda () (phi-autopair-mode 1)))

;; + utility functions

(defun phi-autopair--syntax-info ()
  "return (IN-STRING . IN-COMMENT)"
  (if (and (boundp 'font-lock-mode) font-lock-mode)
      (let ((prev-face (when (> (point) 1)
                         (get-text-property (1- (point)) 'face)))
            (face (get-text-property (point) 'face)))
        (cons (and (memq face '(font-lock-string-face
                                font-lock-doc-face))
                   (memq prev-face '(font-lock-string-face
                                     font-lock-doc-face)))
              (and (memq face '(font-lock-comment-face
                                font-lock-comment-delimiter-face))
                   (memq prev-face '(font-lock-comment-face
                                     font-lock-comment-delimiter-face)))))
    (let ((syntax-ppss (syntax-ppss)))
      (cons (nth 3 syntax-ppss)
            (nth 4 syntax-ppss)))))

(defun phi-autopair--in-string-p ()
  (car (phi-autopair--syntax-info)))

(defun phi-autopair--in-comment-p ()
  (cdr (phi-autopair--syntax-info)))

(defun phi-autopair--escaped-p ()
  "non-nil iff the char at point is escaped"
  (save-excursion
    (= (mod (skip-syntax-backward "\\") 2) 1)))

(defun phi-autopair--string-quot-char ()
  "when called in a string, returns the character with which the
string is started."
  (save-excursion
    (while (and (not (zerop (skip-syntax-forward "^\"")))
                (looking-back "\s\"")))
    (unless (eobp) (char-after))))

;; + insert command

(defun phi-autopair-open ()
  "insert open paren."
  (interactive)
  (let ((open (char-to-string last-command-event)) pair)
    (if (or (not phi-autopair-auto-insert-pairs) ; not desired
            (phi-autopair--escaped-p)            ; escaped
            (null                                ; partner is not found
             (setq pair (cdr (assoc last-command-event phi-autopair--pairs)))))
        (insert open)
      (let ((type (car pair)) (close (cdr pair)))
        ;; escape string delimiters if we are in a string
        (when (and (eq type 'string)
                   (phi-autopair--in-string-p)
                   (= (phi-autopair--string-quot-char) (string-to-char open)))
          (setq open (concat "\\" open) close (concat "\\" close)))
        (if (and phi-autopair-auto-wrap-region (use-region-p))
            ;; wrap region
            (let ((beg (min (region-beginning) (region-end)))
                  (end (max (region-beginning) (region-end))))
              (deactivate-mark)
              (goto-char end)
              (insert close)
              (goto-char beg)
              (insert open))
          ;; if lispy-mode, add whitespaces around parens
          (when (and (not (phi-autopair--in-string-p))
                     (member major-mode phi-autopair-lispy-modes))
            (setq open (concat
                        (unless (looking-back "[\s\t\n]\\|\\s(\\|^\\|\\s'") " ")
                        open)
                  close (concat
                         close
                         (unless (looking-at "[\s\t\n]\\|\\s)\\|$") " "))))
          ;; insert parens
          (insert open)
          (save-excursion (insert close)))))))

(defun phi-autopair-close ()
  (interactive)
  (let ((close (char-to-string last-command-event)))
    (if (or (not phi-autopair-cautious-close)
            (phi-autopair--escaped-p)
            (eq last-command this-command))
        (insert close)
      (message (concat "Press again to insert " close ".")))))

;; + delete commands

(defun phi-autopair--delete-backward (&optional strict)
  "FOR INTERNAL USE. delete maybe one character backward."
  (let* ((syntax (phi-autopair--syntax-info))
         (in-string (car syntax))
         (not-in-comment (not (cdr syntax)))
         (escaped (and (save-excursion
                         (backward-char 1)
                         (phi-autopair--escaped-p))))
         (escaped-middle (and (not escaped)
                              (looking-back "\\s\\")
                              ;; without this, delete-backward will
                              ;; delete newline when an escape char at
                              ;; EOL is deleted backward.
                              (not (looking-at "\n")))))
    (cond ((and phi-autopair-auto-delete-escape
                escaped
                not-in-comment)
           (delete-char -2))
          ((and phi-autopair-auto-delete-escape
                escaped-middle
                not-in-comment)
           (delete-char -1)
           (delete-char 1))
          ((and phi-autopair-auto-delete-pairs
                (not escaped)
                (or (and not-in-comment
                         (not in-string)
                         (looking-back "\\s("))
                    (and in-string
                         (looking-back "\\s\""))))
           (condition-case err
               (paredit-splice-sexp-killing-backward)
             (error (unless strict (backward-delete-char 1)))))
          ((and phi-autopair-auto-delete-spaces
                (looking-back "[\s\t]"))
           (delete-region
            (point)
            (progn (skip-chars-backward "\s\t") (point))))
          ((or (not strict)
               (not (looking-back "\\s)\\|\\s(\\|\\s\"")))
           (delete-char -1))
          (t
           (backward-char 1)))))

(defun phi-autopair--delete-forward (&optional strict)
  "FOR INTERNAL USE. delete maybe one character forward."
  (let* ((syntax (phi-autopair--syntax-info))
         (in-string (car syntax))
         (not-in-comment (not (cdr syntax)))
         (escaped (phi-autopair--escaped-p))
         (escaped-forward (and (not escaped)
                               (looking-at "\\s\\"))))
    (cond ((and phi-autopair-auto-delete-escape
                escaped
                not-in-comment)
           (delete-char 1)
           (delete-char -1))
          ((and phi-autopair-auto-delete-escape
                escaped-forward
                not-in-comment)
           (delete-char 2))
          ((and phi-autopair-auto-delete-pairs
                (not escaped)
                (or (and not-in-comment
                         (not in-string)
                         (looking-at "\\s)"))
                    (and in-string
                         (looking-at "\\s\""))))
           (condition-case err
               (paredit-splice-sexp-killing-forward)
             (error (unless strict (delete-char 1)))))
          ((and phi-autopair-auto-delete-spaces
                (looking-at "[\s\t\n]"))
           (delete-region
            (point)
            (progn (skip-chars-forward "\s\t\n") (point))))
          ((or (not strict)
               (not (looking-at "\\s)\\|\\s(\\|\\s\"")))
           (delete-char 1))
          (t
           (forward-char 1)))))

(defun phi-autopair-delete-backward (&optional n)
  (interactive "p")
  (if (< n 0)
      (phi-autopair-delete-forward (- n))
    (dotimes (_ (or n 1))
      (phi-autopair--delete-backward phi-autopair-cautious-delete))))

(defun phi-autopair-delete-forward (&optional n)
  (interactive "p")
  (if (< n 0)
      (phi-autopair-delete-backward (- n))
    (dotimes (_ (or n 1))
      (phi-autopair--delete-forward phi-autopair-cautious-delete))))

(defun phi-autopair-delete-backward-word (&optional n)
  (interactive "p")
  (if (< n 0)
      (phi-autopair-delete-forward-word (- n))
    (dotimes (_ (or n 1))
      (while (progn
               (phi-autopair--delete-backward 'strict)
               (not (looking-back "\\<."))))
      (delete-char -1))))

(defun phi-autopair-delete-forward-word (&optional n)
  (interactive "p")
  (if (< n 0)
      (phi-autopair-delete-backward-word (- n))
    (dotimes (_ (or n 1))
      (while (progn
               (phi-autopair--delete-forward 'strict)
               (not (looking-at ".\\>"))))
      (delete-char 1))))

;; + workarounds

;; tell "delsel.el" that "phi-autopair-delete-xxx" should
;; "delete-selection".
(eval-after-load "delsel"
  '(dolist (symbol '(phi-autopair-delete-backward
                     phi-autopair-delete-forward
                     phi-autopair-delete-backward-word
                     phi-autopair-delete-forward-word))
     (put symbol 'delete-selection t)))

;; + provide

(provide 'phi-autopair)

;;; phi-autopair.el ends here
