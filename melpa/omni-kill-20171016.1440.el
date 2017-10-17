;;; omni-kill.el --- Kill all the things  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2017  Adrien Becchis

;; Author: Adrien Becchis <adriean.khisbe@live.fr>
;; Created:  2014-09-06
;; Version: 0.6.0
;; Package-Version: 20171016.1440
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

;; `omni-kill' is there to assist you as emacs user to quickly kill,
;; select, copy, ... all the things that are at point such as emails,
;; url, sentence...
;;
;; It's built on top of `thingatpt', and just provide wrapper functions.

;;; Code:

(require 'thingatpt)

(defgroup omni-kill nil
    "Commands to kill/delete/copy/younameit all the things at point."
    :group 'editing)

(defcustom omni-kill-thing-list
  '(symbol list sexp defun filename url email word sentence whitespace line page)
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
                            (numberp (string-match-p "\\s_*%s\\(\\s_\\|\\sw\\)*%s\\s_*" x))
                            (not (string-match-p "\\s-" x))))))
  :initialize 'custom-initialize-default
  :set (lambda (symb val)
         (omni-kill-destroy-all-the-things)
         (set-default symb val)
         (omni-kill-get-all-the-things))
  :group 'omni-kill)
;; §name: maybe copy-this-THING rather than omny-copy by default

(defcustom omni-kill-thing-to-letter-alist
  '((?d . defun)
    (?e . email)
    (?f . filename)
    (?l . line)
    (?L . list)
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

(defvar omni-kill-action-alist
  '(("kill" . kill-region)
    ("delete" . delete-region)
    ("select" .  select-region)
    ("copy" . copy-region)
    ("narrow" . narrow-to-region)
    ("save" . register-region)
    ("start" . start-region)
    ("end" . end-region)))

;; kill-thing at point
(defun omni-kill--do-thing-at-point (action thing)
  "ACTION the THING at point if any.

Returns nil."
  (let ((frontier (bounds-of-thing-at-point thing)))
    ;; §later: try catch error?
    (if frontier
        (funcall action (car frontier) (cdr frontier))
      (message "There is not a %s at point!" thing))
    ;; §check if can be chained.
    nil))
;; §maybe: message should go up, in thegenerated functions (note: after macro extraction?)

(defun select-region (start end)
  "Select region between START END."
  (push-mark) ; save old mark
  (set-mark start)
  (goto-char end))

(defun copy-region (start end)
  "Copy region between START END."
  (save-restriction
    (narrow-to-region start end)
    (kill-new (buffer-string))
    (widen)))

(defun register-region (start end)
  "Save region between START END to queries register."
  (let ((register (register-read-with-preview "Specify Register:")))
    (copy-to-register register start end)))

(defun start-region (start end)
  "Go to start of region between START END."
  (goto-char start))

(defun end-region (start end)
  "Go to end of region between START END."
  (goto-char (1- end)))

;; §later:  make it in the clipboard.
;; §see: clipboard function: clipboard-yank, etc!!!!!
;; §see: xsel

;; §later: store-thing! :)

;;; ¤> Function generators
;; §maybe: next bump: two level multiplexer!!! on action, then on thing!!

(defun omni-kill-help ()
  "Display the letter to thing associations for the omni-dispatch functions."
  (interactive)
  (message "%s%s"
           (propertize "Letter2Thing: " 'face 'font-lock-type-face)
           (mapconcat (lambda (cs) (format "%c:%s" (car cs) (cdr cs)))
                      omni-kill-thing-to-letter-alist " ")))

(defun omni-kill--generate-dispatch-command (command)
  "Generate a dispath command for the given COMMAND."
  (let ((command-name (intern (format "omni-%s" command))))
    (defalias command-name
      (lambda (char-or-thing)
        (interactive "cPick a thing:")  ;§later: recap list
        (let ((thing (if (symbolp char-or-thing) char-or-thing
                       (cdr-safe (assoc char-or-thing omni-kill-thing-to-letter-alist)))))
          (if thing
              (omni-kill--do-thing-at-point (cdr (assoc command omni-kill-action-alist)) thing)
            (progn
              (message "No thing is associated at letter '%s' (for memory refresh, run `omni-help')" (char-to-string char))
              nil)))))
    (put command-name 'function-documentation (format "%s the thing associated with the given CHAR-OR-THING.
Association are stored in the `omni-kill-thing-to-letter-alist' variable" (capitalize command)))))



(defun omni-kill--destroy-dispatch-command (command)
  "Generate a dispath command for the given COMMAND."
  (fmakunbound (intern (format "omni-%s" command))))

(defun omni-kill--generate-command (command symb)
  "Generate a COMMAND command for the given SYMB."
  (let ((command-name (intern (format omni-kill-naming-scheme command symb))))
    (defalias command-name
      (lambda ()
        (interactive)
        (omni-kill--do-thing-at-point (cdr (assoc command omni-kill-action-alist)) symb)))
    (put command-name 'function-documentation
         (format "%s the %s at point." (capitalize command) symb))))

(defun omni-kill--destroy-command (command symb)
  "Generate a dispath command for the given COMMAND."
  (fmakunbound (intern (format omni-kill-naming-scheme command symb))))


(defun omni-kill-get-all-the-things()
  "Generate all the omni functions for the list of things."
  ;; §tofix: eager macro expansion failure
  ;; §wtf: does not happen when manually load.
  (mapc (lambda (action)
          (omni-kill--generate-all-the-fun action))
        (mapcar 'car omni-kill-action-alist)))
;; §maybe: user custom for list?

(defun omni-kill--generate-all-the-fun (action)
  "Generate all the functions associated with the given ACTION."
  (omni-kill--generate-dispatch-command action)
  (mapc (lambda (thing)
          (omni-kill--generate-command action thing))
        omni-kill-thing-list))

(defun omni-kill-destroy-all-the-things ()
  "Unenerate all the omni functions for the list of things."
  (mapc (lambda (action)
          (omni-kill--destroy-dispatch-command action)
          (mapc (lambda (thing)
                  (omni-kill--destroy-command action thing))
                omni-kill-thing-list))
        (mapcar 'car omni-kill-action-alist)))

;; set up all commands:
(omni-kill-get-all-the-things)

(provide 'omni-kill)
;;; omni-kill.el ends here
