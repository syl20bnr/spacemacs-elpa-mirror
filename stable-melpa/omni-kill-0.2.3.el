;;; omni-kill.el --- Kill all the things

;; Copyright (C) 2014-2015  Adrien Becchis

;; Author: Adrien Becchis <adriean.khisbe@live.fr>
;; Created:  2014-09-06
;; Version: 0.2.3
;; Package-Version: 0.2.3
;; Keywords: convenience, editing, tools

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

;; `omni-kill' is there to assist you as emacs user to quickly copy,
;; select, kill, ... all the things that are at point such as emails,
;; url, sentence...
;;
;; It's built on top of `thingatpt', and just provide wrapper functions.

;;; Code:

(require 'thingatpt)

(defgroup omni-kill nil
    "Commands to kill/delete/copy/younameit all the things at point."
    :group 'editing)

(defcustom omni-kill-thing-list
  '(symbol list sexp defun filename url email word sentence whitespace line number page )
  "List of THING symbols for which omni kill will create a function."
  :type '(repeat symbol)
  :group 'omni-kill)

(defcustom omni-kill-naming-scheme
  "omni-%s-%s"
  "Naming pattern of generated patters.

It must contains two '%s placeholders:
- The first placeholder correspond to the action,
- the second to the thing.

Changing this would only have effect at next startup."
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x) (and (stringp x)
                            (string-match-p "\\s_*%s\\s_*%s\\s_*" x)
                            ;; ¤note: hack since did not managed to make the anchors work
                            (not (string-match-p "\\s-" x))))))
  :group 'omni-kill)
;; §name: maybe copy-this-THING rather than omny-copy by default

(defcustom omni-kill-thing-to-letter-alist
  '((?d . defun)
    (?e . email)
    (?f . filename)
    (?l . line)
    (?l . list)
    (?n . number)
    (?P . page)
    (?p . sentence)
    (?S . sexp)
    (?s . symbol)
    (?u . url)
    (?W . whitespace)
    (?w . word))
  "Alist to store the letter associated with a thing for the vi like functions."
  :type '(alist :key-type char :value-type symbol))

;; ¤> thing at point wrappers
;; §todo: select thing at pt

;; kill-thing at point

(defun omni-kill-kill-thing-at-point (thing)
  "Kill the THING at point if any.

Returns nil."
  (let ((frontier (bounds-of-thing-at-point thing) ))
    ;; §later: try catch error?
    (if frontier
         (kill-region (car frontier) (cdr frontier))
        (message "There is not a %s at point!" thing))
    ;; §check if can be chained.
    nil))

(defun omni-kill-delete-thing-at-point (thing)
  "Delete the THING at point if any.

Returns nil."
  (let ((frontier (bounds-of-thing-at-point thing) ))
    ;; §later: try catch error?
    (if frontier
         (delete-region (car frontier) (cdr frontier))
        (message "There is not a %s at point!" thing))
    nil))

(defun omni-kill-select-thing-at-point (thing)
  ;; ¤note: similar to mark...
  "Select the THING at point if any.

Returns nil."
  (let ((frontier (bounds-of-thing-at-point thing) ))
    ;; §later: try catch error?
    (if frontier
        (progn
          (push-mark) ; save old mark
          (set-mark (car frontier))
          (goto-char  (cdr frontier)))
        (message "There is not a %s at point!" thing))
    nil))
;; §maybe: message should go up, in thegenerated functions (note: after macro extraction?)


(defun omni-kill-copy-thing-at-point (thing)
  "Try to copy the THING at point.  (use `kill-new' for now).

Returns the value grabed, otherwise nil."
  ;; §later: reconsider signature? (send the catch?)
  (let ((the-thing (thing-at-point thing) ))
    ;; §later: try catch error?
    (message "%s" the-thing)
    (if the-thing
        (progn
          (kill-new (format "%s" the-thing))
          ;; ¤note: this is to protect from number grabing
          (message "%s was copied" thing)
          the-thing)
      (progn
        (message "There is not a %s at point!" thing)
             nil))))

;; §later:  make it in the clipboard.
;; §see: clipboard function: clipboard-yank, etc!!!!!
;; §see: xsel

;; §later: store-thing! :)

;;; ¤> Function generators
(defun omni-kill-generate-all-the-fun (thing)
  "Generate all the functions associated with the given THING."
  (omni-kill-generate-copy-command thing)
  (omni-kill-generate-delete-command thing)
  (omni-kill-generate-kill-command thing)
  (omni-kill-generate-select-command thing)
  (mapc (lambda (a) (omni-kill-generate-dispatch-command a))
        '("copy" "delete" "kill" "select")))
;; §later: factorize macros + §next bump: two level multiplexer!!! on action, then on thing!!

(defun omni-kill-help ()
  "Display the letter to thing associations for the omni-dispatch functions."
  (interactive)
  (message "%s%s"
           (propertize "Letter2Thing: " 'face 'font-lock-type-face)
           (mapconcat (lambda (cs) (format "%c:%s" (car cs) (cdr cs)))
                      omni-kill-thing-to-letter-alist " ")))

(defmacro omni-kill-generate-dispatch-command (action)
  "Generate a dispath command for the given ACTION."
 `(defun ,(intern (format "omni-%s" (eval action))) (char)
       ,(format "%s the thing associated with the given CHAR.
Association are stored in the `omni-kill-thing-to-letter-alist' variable" (capitalize (eval action))) ;§todo: doc
       (interactive "cPick a thing:");§later: recap list
       (let ((thing (cdr-safe (assoc char omni-kill-thing-to-letter-alist))))
         (if thing
             (,(intern (format "omni-kill-%s-thing-at-point" (eval action) )) thing)
           (progn (message "No thing is associated at letter %s  (for memory refresh, run `omni-help')" char)
           nil)))))

;; §maybe: macro generate the macro... ^^
(defmacro omni-kill-generate-copy-command (symb)
  "Generate a copy command for the given SYMB."
 `(defun ,(intern (format omni-kill-naming-scheme "copy" (eval symb))) ()
       ,(format "Copy the %s at point" (eval symb))
       (interactive)
       (omni-kill-copy-thing-at-point ',(eval symb))))

(defmacro omni-kill-generate-delete-command (symb)
  "Generate a delete command for the given SYMB."
  `(defun ,(intern (format omni-kill-naming-scheme "delete" (eval symb))) ()
       ,(format "Delete the %s at point"  (eval symb))
       (interactive)
       (omni-kill-delete-thing-at-point ',(eval symb))))

(defmacro omni-kill-generate-kill-command ( symb)
  "Generate a kill command for the given SYMB."
  `(defun ,(intern (format omni-kill-naming-scheme "kill" (eval symb))) ()
     ,(format "Kill the %s at point"  (eval symb))
     (interactive)
     (omni-kill-kill-thing-at-point ',(eval symb))))

(defmacro omni-kill-generate-select-command ( symb)
  "Generate a select command for the given SYMB."
  `(defun ,(intern (format omni-kill-naming-scheme "select" (eval symb))) ()
     ,(format "Select the %s at point"  (eval symb))
     (interactive)
     (omni-kill-select-thing-at-point ',(eval symb))))

(defun omni-kill-get-all-the-things()
  "Generate all the omni functions for the list of things."
  ;; §tofix: eager macro expansion failure
  ;; §wtf: does not happen when manually load.
  (mapc (lambda (arg) (omni-kill-generate-all-the-fun arg))
        omni-kill-thing-list))
;; §maybe: user custom for list?

;; set up all commands:
(omni-kill-get-all-the-things)

(provide 'omni-kill)
;;; omni-kill.el ends here
