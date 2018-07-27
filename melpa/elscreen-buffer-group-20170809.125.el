;;; elscreen-buffer-group.el --- elscreen buffer group

;; Copyright (C) 2012-2015 Jeff Gran

;; Author: Jeff Gran <jeff@jeffgran.com>
;;	Author: Ryan C. Thompson
;; URL: https://github.com/jeffgran/elscreen-buffer-group
;; Package-Version: 20170809.125
;; Created: 7 Nov 2012
;; Keywords: buffer
;; Version: 1.0.1
;; Package-Requires: ((emacs "24.4") (elscreen "0") (cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This code is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Emacs. If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This is a rewrite/overhaul of an existing package called elscreen-buffer-list,
;; fixed for emacs 24 and the latest version of elscreen from MELPA.
;;
;; Enabling this package gives each elscreen its own buffer group.
;; When a buffer is first displayed, it automatically gets added to the
;; group of the current elscreen. Then, while you're in that elscreen,
;; you'll only be able to see those buffers that belong to that elscreen.
;;
;; This works by hijacking some low-level buffer access functions, which
;; are the only functions (that I can find) that generate the master
;; list of buffers, at the lowest level. If you find a place where this
;; doesn't look, report it and I'm sure we can find a function to advise.
;;
;; In order to give you an "out" to see ALL the buffers in case you want to,
;; you can add a command name (a symbol) to 'elscreen-buffer-group-skip-commands and
;; elscreen-buffer-group will "skip" the filtering advice on that command. By default
;; this is set to just 'ibuffer, but you can make it whatever you want. All
;; other commands will use the filtered list.
;;
;; Usage:
;;
;; You have to be using elscreen, then just require it.
;;
;; (require 'elscreen)
;; (require 'elscreen-buffer-group)
;;
;; You can choose which commands do NOT filter the buffer list:
;;
;; (setq 'elscreen-buffer-group-skip-commands `(my-special-buffer-switching-command))
;;
;; You can turn on/off exclusivity, meaning a buffer can ONLY belong to one
;; screen at a time. If this is nil, a buffer can be in more than one screen.
;; If it's non-nil, adding a buffer to a screen (displaying it while in that
;; screen) will remove it from all other screens:
;;
;; (setq elscreen-buffer-group-exclusive nil)
;;


;;; Code:


(require 'cl-lib)
(require 'elscreen)

(defvar elscreen-buffer-group-skip-commands `(ibuffer)
  "List of commands that should NOT filter to only show the current screen's buffer group.")

(defvar elscreen-buffer-group-exclusive t
  "Non-nil means a buffer can only belong to one screen at once.")

(define-minor-mode elscreen-buffer-group-global-mode
  "Toggle elscreen buffer groups."
  :global t
  (if elscreen-buffer-group-global-mode
      (progn
        (setq elscreen-buffer-group-global-mode t)
        (ad-enable-advice 'display-buffer 'around 'elscreen-buffer-group-display-buffer-advice)
        (ad-enable-advice 'kill-buffer 'around 'elscreen-buffer-group-dont-kill-scratch)
        (ad-enable-advice 'switch-to-prev-buffer 'around 'elscreen-buffer-group-switch-to-prev-buffer)
        (ad-enable-advice 'elscreen-kill 'before 'elscreen-buffer-group-kill-buffers)
        (ad-enable-advice 'internal-complete-buffer 'around 'elscreen-buffer-group-internal-complete-buffer)
        (ad-enable-advice 'buffer-list 'around 'elscreen-buffer-group-buffer-list)
        (ad-enable-advice 'switch-to-buffer 'around 'elscreen-buffer-group-switch-to-buffer-advice)
        (message "elscreen-buffer-group is on"))
    (progn
      (setq elscreen-buffer-group-global-mode nil)
      (ad-disable-advice 'display-buffer 'around 'elscreen-buffer-group-display-buffer-advice)
      (ad-disable-advice 'kill-buffer 'around 'elscreen-buffer-group-dont-kill-scratch)
      (ad-disable-advice 'switch-to-prev-buffer 'around 'elscreen-buffer-group-switch-to-prev-buffer)
      (ad-disable-advice 'elscreen-kill 'before 'elscreen-buffer-group-kill-buffers)
      (ad-disable-advice 'internal-complete-buffer 'around 'elscreen-buffer-group-internal-complete-buffer)
      (ad-disable-advice 'buffer-list 'around 'elscreen-buffer-group-buffer-list)
      (ad-disable-advice 'switch-to-buffer 'around 'elscreen-buffer-group-switch-to-buffer-advice)
      (message "elscreen-buffer-group is off"))))

(defun elscreen-buffer-group-add-buffer-to-list (arg)
  "Add the buffer to the current screen's elscreen-buffer-group-list elscreen property.

ARG is either a buffer or a buffer name that can be used to get the buffer via
(get-buffer ARG)"
  (let* ((screen-properties (elscreen-get-screen-property (elscreen-get-current-screen)))
         (elscreen-buffer-group-list (elscreen-buffer-group-get-alist 'elscreen-buffer-group-list screen-properties))
         (the-new-buffer (if (stringp arg)
                             (get-buffer arg)
                           arg)))

    ;; add the new buffer to the list
    (if (null elscreen-buffer-group-list)
        (push the-new-buffer elscreen-buffer-group-list)
      (add-to-list 'elscreen-buffer-group-list the-new-buffer))

    ;; set the elscreen property to the new changed one.
    (elscreen--set-alist 'screen-properties 'elscreen-buffer-group-list elscreen-buffer-group-list)
    (elscreen-set-screen-property (elscreen-get-current-screen) screen-properties)

    ;; also (maybe) remove it from any other lists
    (when elscreen-buffer-group-exclusive
      (mapc
       (lambda (screen)
         (unless (or (eq (elscreen-get-current-screen) screen)
                     (string= "*scratch*" (buffer-name the-new-buffer))) ; let the scratch buffer always stay
           (elscreen-buffer-group-remove-buffer-from-list the-new-buffer screen)))
       (elscreen-get-screen-list)))

    ;; "refresh" the screen/tabs display in the top line
    (elscreen-run-screen-update-hook)))

(defun elscreen-buffer-group-remove-buffer-from-list (buffer screen)
  "Remove BUFFER from the buffer list for SCREEN."
  (let ((the-buffer-list (elscreen-buffer-group-get-ordered-buffer-list screen))
        (screen-properties (elscreen-get-screen-property screen)))
    (elscreen--set-alist 'screen-properties 'elscreen-buffer-group-list (remove buffer the-buffer-list))
    (elscreen-set-screen-property screen screen-properties)))

(defun elscreen-buffer-group-get-ordered-buffer-list (&optional screen)
  "Return the saved list of buffers which have been accessed in SCREEN or the current screen,
ordered by recency."
  (elscreen-buffer-group-reorder-buffer-list (elscreen-buffer-group-get-raw-buffer-list screen)))


(defun elscreen-buffer-group-get-raw-buffer-list (&optional screen)
  "Return the saved list of buffers which have been accessed in this screen"
  (let ((screen-properties (elscreen-get-screen-property (or screen (elscreen-get-current-screen)))))
    (cl-remove-if-not 'buffer-live-p  (or (elscreen-buffer-group-get-alist 'elscreen-buffer-group-list screen-properties)
                                          (list (get-buffer "*scratch*"))))))

;;make ido-switch-buffer (& friends) use my buffer list
(eval-after-load 'ido
  '(add-hook 'ido-make-buffer-list-hook 'elscreen-buffer-group-filter-ido-buffer-list))

(defun elscreen-buffer-group-filter-ido-buffer-list ()
  "Filter ido's buffer list and history list."
  (setq ido-temp-list (mapcar 'buffer-name (elscreen-buffer-group-get-ordered-buffer-list))))

(defun elscreen-buffer-group-reorder-buffer-list (the-list)
  "Set buffers in THE-LIST to be the most recently used, in order."
    (ad-deactivate 'buffer-list)
    (let ((real-buffer-list (buffer-list)))
      (ad-activate 'buffer-list)
      ;;(message (prin1-to-string real-buffer-list))
      (elscreen-buffer-group-filter-buffer-list the-list real-buffer-list)))

(defun elscreen-buffer-group-filter-buffer-list (the-list real-buffer-list)
  "Return only elements from THE-LIST that are also in REAL-BUFFER-LIST.

The intention is that REAL-BUFFER-LIST is the buffer list from c-source code and THE-LIST is
from elscreen-buffer-group, so we only want to keep the ones from here."
  (if (member this-command elscreen-buffer-group-skip-commands)
      real-buffer-list
    ;; can't use cl-intersection here because the whole point of this is to keep
    ;; the ordering, and cl-intersection does not keep the ordering. :/
    (delq nil
          (mapcar (lambda (x)
                    (and
                     (member (buffer-name x) (mapcar 'buffer-name the-list))
                     x ))
                  real-buffer-list))))


;; these two are to add any newly shown buffer to the buffer list of the current screen
(defadvice display-buffer (around elscreen-buffer-group-display-buffer-advice activate)
  "Add any newly displayed buffer to the current screen's buffer group."
  (let* ((ret-val ad-do-it)
          (the-buffer (cond
                       ((bufferp ret-val)
                        ret-val)
                       ((windowp ret-val)
                        (window-buffer ret-val))
                       (t
                        (throw "wat did this return?")))))
     ;;(message (prin1-to-string the-buffer))
     (elscreen-buffer-group-add-buffer-to-list the-buffer)
     (setq ad-return-value ret-val)))

(defadvice switch-to-buffer (around elscreen-buffer-group-switch-to-buffer-advice activate)
  "Add any newly displayed buffer to the current screen's buffer group."
  (let* ((ret-val ad-do-it)
          (the-buffer (cond
                       ((bufferp ret-val)
                        ret-val)
                       ((windowp ret-val)
                        (window-buffer ret-val))
                       (t
                        (throw "wat did this return?")))))
     ;;(message (prin1-to-string the-buffer))
     (elscreen-buffer-group-add-buffer-to-list the-buffer)
     (setq ad-return-value ret-val)))


(defadvice buffer-list (around elscreen-buffer-group-buffer-list activate)
  "Make the built-in function (buffer-list) return MY buffer list instead."
  (when (not (member this-command 'elscreen-buffer-group-skip-commands))
    (setq ad-return-value (elscreen-buffer-group-get-ordered-buffer-list))))


(defadvice internal-complete-buffer (around elscreen-buffer-group-internal-complete-buffer activate)
  "This is a c function that completes for a buffer, optionally with a predicate.

Basically we hack in here and add another predicate to whatever predicates are already there,
if any, so that this only matches/returns buffers in the current elscreen."
  (lexical-let ((string (ad-get-arg 0))
                (pred (ad-get-arg 1)))
    (when (not (member this-command 'elscreen-buffer-group-skip-commands))
      (ad-set-arg 1 (lambda (buffer-dot-name)
                      (and (if pred (funcall pred buffer-dot-name) t)
                           (member (cdr buffer-dot-name) (elscreen-buffer-group-get-ordered-buffer-list)))))))
  ad-do-it)

(defadvice elscreen-kill (before elscreen-buffer-group-kill-buffers activate)
  "When you kill a screen, kill all the buffers in its list."
  (mapcar '(lambda (b) (kill-buffer b)) (elscreen-buffer-group-get-raw-buffer-list)))

(defadvice switch-to-prev-buffer (around elscreen-buffer-group-switch-to-prev-buffer activate)
"This is for when you kill a buffer.

It looks for a buffer to show next.  We
want to make sure it only shows one from the list of buffers in the current
screen"
  ;; nth 1 means the 'next' one (the 'first' one is the current one we're closing)
  (let* ((last-buffer (nth 1 (elscreen-buffer-group-get-ordered-buffer-list)))
         (the-buffer (or (and (not (eq last-buffer (window-buffer (selected-window))))
                              last-buffer)
                         (get-buffer "*scratch*"))))
    (set-window-buffer (selected-window) the-buffer)))

(defadvice kill-buffer (around elscreen-buffer-group-dont-kill-scratch activate)
  "Don't kill the scratch buffer."
  (unless (string= (buffer-name (current-buffer)) "*scratch*")
      ad-do-it))

(defun elscreen-buffer-group-get-alist (key alist)
  "Convenience method to get a value by KEY from ALIST."
  (cdr (assoc key alist)))

(provide 'elscreen-buffer-group)

;;; elscreen-buffer-group.el ends here
