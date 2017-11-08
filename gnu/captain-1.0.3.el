;;; captain.el --- CAPiTalization is Automatic IN emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: Ian Dunn <dunni@gnu.org>
;; Maintainer: Ian Dunn <dunni@gnu.org>
;; Keywords: editing
;; Version: 1.0.3

;; This file is part of GNU Emacs.

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

;; The captain handles capitalizing text as you type, freeing you to do other
;; things.

;; Invoke the captain across the globe with `global-captain-mode', or just on
;; one ship (buffer) at a time with `captain-mode'.

;; For normal people:

;; Automatically capitalizes the first word of a sentence as you type.  The
;; bounds of a sentence are determined by local variable
;; `captain-sentence-start-function'.  For example, you can set it to find the
;; start of a list or heading in `org-mode'.

;; In order to get the captain to start working, `captain-predicate' must be
;; set.  Otherwise, the slacker will just lie around all day doing nothing.

;; The following will tell the captain to only work on comments in programming
;; modes:

;; (add-hook 'prog-mode-hook
;;    (lambda ()
;;      (setq captain-predicate (lambda () (nth 8 (syntax-ppss (point)))))))

;; Or for text modes, work all the time:

;; (add-hook 'text-mode-hook
;;           (lambda ()
;;             (setq captain-predicate (lambda () t))))

;; Or don't work in source blocks in Org mode:

;; (add-hook
;;  'org-mode-hook
;;  (lambda ()
;;    (setq captain-predicate
;;          (lambda () (not (org-in-src-block-p))))))

;; It's also possible to automatically capitalize individual words using the
;; command `captain-capitalize-word'.  This will capitalize the word at point
;; and tell the captain about it, so he knows that you want it capitalized from
;; then on.  For a package that handles this for any automatic correction, see
;; the auto-correct package.

;; This solves a similar problem to that of Kevin Rodgers's auto-capitalize
;; package, but using more modern Emacs features.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'thingatpt)

(defun captain--default-predicate ()
  "The default predicate for determining whether the captain should work.

He does nothing by default."
  nil)

(defvar-local captain-predicate #'captain--default-predicate
  "Predicate to check for whether auto-capitalization should be handled.

Should be a function of no arguments, and return non-nil if
auto-capitalization should be performed at the current point.

For example, this could be a function to check if point is in a
comment.

This is `captain--default-predicate' by default, which returns
nil to avoid automatic capitalization happening everywhere, so to
start using captain, set it to a function of your choosing.")

(defun captain-should-capitalize-p ()
  "Return non-nil if the captain should auto-capitalize your work."
  (funcall captain-predicate))

(defun captain--default-sentence-start ()
  "Default value of `captain-sentence-start-function'.

Just runs `bounds-of-thing-at-point' for a sentence."
  (car (bounds-of-thing-at-point 'sentence)))

(defvar-local captain-sentence-start-function
  #'captain--default-sentence-start
  "Function to determine the start of the current sentence.

This should be a function of no arguments that returns the point
at which the current function begins.")

(defun captain--run ()
  "Automatically capitalize the word preceding point if it is the start of a sentence."
  ;; Only do this if the last inserted character isn't part of a word,
  ;; (preceding-char) is the previously inserted character;
  ;; We also need the character before that (It must be a word constituent, so
  ;; enter or space don't keep trying to capitalize the sentence).
  (when (and (captain-should-capitalize-p)
             (not (or (bobp) (eq (char-syntax (preceding-char)) ?w)))
             (eq (char-syntax (save-excursion (forward-char -1)
                                              (preceding-char)))
                 ?w))
    (save-excursion
      ;; Move back to the word that was just finished, and determine if it
      ;; starts a sentence.
      (backward-word-strictly)
      ;; Ensure we're still expected to capitalize.  Case: New character put us
      ;; inside a string, but backward-word-strictly brought us outside the
      ;; string.
      (when (captain-should-capitalize-p)
        (when-let ((word-bounds (bounds-of-thing-at-point 'word))
                   (sentence-start (funcall captain-sentence-start-function)))
          (cond
           ((let ((case-fold-search nil))
              (string-match-p "[[:upper:]]" (word-at-point)))
            nil)
           ;; This word does start a sentence, so capitalize it
           ((eq (car word-bounds) sentence-start) (capitalize-word 1))
           ;; Word bound will only ever be one greater than the sentence bound if
           ;; the sentence begins with some sort of punctuation.  Remember, spaces
           ;; don’t count, so if we have a sentence starting with "A ball...",
           ;; ’ball’ wont trigger this, only ’A’.
           ((eq (car word-bounds) (1+ sentence-start))
            (capitalize-word 1))))))))

(defun captain-capitalize-sentence ()
  "Tell the captain to capitalize the start of the current sentence."
  (interactive)
  (save-excursion
    (goto-char (funcall captain-sentence-start-function))
    (capitalize-word 1)))

;;;###autoload
(define-minor-mode captain-mode
  "Call the captain to automatically capitalize the start of every sentence.

The captain will also automatically capitalize words you've told
him you want capitalized with `captain-capitalize-word'.

\\{captain-mode-map}"
  :init-value nil
  :lighter " Captain"
  :global nil
  (if captain-mode
      (progn
        (add-hook 'post-self-insert-hook 'captain--run nil t))
    (remove-hook 'post-self-insert-hook 'captain--run t)))

;;;###autoload
(define-globalized-minor-mode global-captain-mode
  captain-mode captain-mode)

;; Support for capitalizing individual words automatically

(define-abbrev-table 'captain-mode-abbrev-table nil
  "Abbrev table where words that should be automatically capitalized are stored.

This is case sensitive by default so `expand-region-abbrevs'
won't keep trying to expand \"Name\" to \"Name\"."
  :enable-function #'captain-should-capitalize-p
  :case-fixed t)

;; Only enable the abbrev table when captain-mode is active
(add-to-list 'abbrev-minor-mode-table-alist
             `(captain-mode ,captain-mode-abbrev-table)
             'append
             #'equal)

(defun captain--start-of-word-p ()
  "Return non-nil if at the start of a word."
  (and
   ;; looking at a word constituent...
   (eq (char-syntax (following-char)) ?w)
   ;; ...but before us isn't a word constituent
   (not (eq (char-syntax (preceding-char)) ?w))))

(defun captain-capitalize-word ()
  "Tell the captain to capitalize the word at point.

After that, the captain will remember your choice, and
automatically capitalize the word from then on.

If not looking at a word, move forward to find the next word."
  (interactive)
  (save-excursion
    ;; Cases for where we are:
    (cond
     ;; Looking at the start of a word, so don't move
     ((captain--start-of-word-p))
     ((word-at-point)
      ;; If we're in the middle of a word, then move back to the start.
      (backward-word-strictly))
     (t
      ;; Otherwise, we're not on a word, so move forward to find the next one.
      ;; This is to remain consistent with the behavior of `capitalize-word'.
      (forward-to-word 1)))
    (when-let ((old-word (substring-no-properties (word-at-point)))
               (new-word (capitalize old-word)))
      ;; Store the abbrev so this word is automatically capitalized later.
      (define-abbrev captain-mode-abbrev-table
        old-word new-word nil :count 1))
    ;; Finally, capitalize the word at point.
    (capitalize-word 1)))

;;;; ChangeLog:

;; 2017-10-09  Ian Dunn  <dunni@gnu.org>
;; 
;; 	captain: Bumped version to 1.0.3
;; 
;; 	* packages/captain/captain.el: Bumped version.
;; 
;; 2017-10-09  Ian Dunn  <dunni@gnu.org>
;; 
;; 	captain: Added examples for setting captain-predicate to commentary
;; 
;; 	* packages/captain/captain.el (Commentary): Added examples.
;; 
;; 2017-10-08  Ian Dunn  <dunni@gnu.org>
;; 
;; 	captain: Bumped version
;; 
;; 	* packages/captain/captain.el: Bumped version to 1.0.2
;; 
;; 2017-10-08  Ian Dunn  <dunni@gnu.org>
;; 
;; 	captain: Fix docstring of captain-predicate to reflect recent change
;; 
;; 	* packages/captain/captain.el (captain-predicate): Fixed docstring.
;; 
;; 2017-10-08  Ian Dunn  <dunni@gnu.org>
;; 
;; 	captain: Change abbrev table to be case sensitive
;; 
;; 	This should avoid `expand-region-abbrevs' attempting to expand "Name" to
;; 	"Name".
;; 
;; 	* packages/captain/captain.el (captain-mode-abbrev-table): Make
;; 	case-fixed by
;; 	 default.
;; 
;; 2017-10-08  Ian Dunn  <dunni@gnu.org>
;; 
;; 	captain: Change default predicate to a function, and call it
;; 	unconditionally
;; 
;; 	* packages/captain/captain.el (captain--default-predicate): New defun.
;; 	 (captain-predicate): Use it by default.
;; 
;; 2017-09-05  Ian Dunn  <dunni@gnu.org>
;; 
;; 	captain.el: Reverted last commit
;; 
;; 	* captain.el (captain-predicate): Use nil by default, and add
;; 	documentation
;; 	 explaining why.
;; 
;; 2017-09-05  Ian Dunn  <dunni@gnu.org>
;; 
;; 	captain.el: Changed default predicate to enable capitalization
;; 
;; 2017-09-04  Ian Dunn  <dunni@gnu.org>
;; 
;; 	Added captain package
;; 
;; 	Single-file package to handle automatic capitalization
;; 
;; 	* packages/captain/captain.el: Added.
;; 


(provide 'captain)

;;; captain.el ends here
