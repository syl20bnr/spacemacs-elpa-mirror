;;; unify-opening.el --- Make everything use the same mechanism to open files

;; Copyright (C) 2015 Damien Cassou

;; Author: Damien Cassou <damien.cassou@gmail.com>
;; Url: https://github.com/DamienCassou/unify-opening
;; Package-Version: 20151116.1648
;; GIT: https://github.com/DamienCassou/unify-opening
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Created: 16 Jan 2015
;; Keywords: dired org mu4e open runner extension file

;; This file is not part of GNU Emacs.

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
;; Make everything use the same mechanism to open files. Currently,
;; `dired` has its mechanism, `org-mode` uses something different (the
;; `org-file-apps` variable), and `mu4e` something else (a simple
;; prompt). This package makes sure that each package uses the
;; mechanism of `dired`. I advise you to install the
;; [`runner`](https://github.com/thamer/runner) package to improve the
;; `dired` mechanism.

;;; Code:

;; Avoid warnings about undefined variables and functions
(declare-function dired-do-async-shell-command "dired-aux")
(declare-function dired-guess-shell-command "dired-x")
(declare-function mu4e~view-get-attach "ext:mu4e-view")
(eval-when-compile (defvar org-file-apps))

(defun unify-opening-find-cmd (file)
  "Return a string representing the best command to open FILE.
This method uses `dired-guess-shell-command'.  The runner package, which I
  recommend, will modify the behavior of `dired-guess-shell-command' to
  work better."
  (require 'dired-x)
  (dired-guess-shell-command (format  "Open %s " file) (list file)))

(defun unify-opening-open (file &optional cmd)
  "Open FILE with CMD if provided, ask for best CMD if not.
Asking for best CMD to use to open FILE is done through
`unify-opening-find-cmd'."
  (let ((cmd (or cmd (unify-opening-find-cmd file))))
    (require 'dired-aux)
    (dired-do-async-shell-command cmd 0 (list file))))

(with-eval-after-load "mm-decode"
  (defun unify-opening-mm-interactively-view-part (handle)
    "Une unify-opening to display HANDLE.
Designed to replace `mm-interactively-view-part'."
    (let ((tmpfile (make-temp-file
                    "emacs-mm-part-"
                    nil
                    (mm-handle-filename handle))))
      (mm-save-part-to-file handle tmpfile)
      (unify-opening-open tmpfile)))
  (advice-add #'mm-interactively-view-part :override
              #'unify-opening-mm-interactively-view-part))

(with-eval-after-load "org"
  (add-to-list 'org-file-apps '(t . (unify-opening-open file))))

(with-eval-after-load "mu4e"
  (defun unify-opening-mu4e-view-open-attachment-with (args)
    "Use unify-opening to select which command to open attachments with."
    (let* ((msg (car args))        ;; 1st original argument
           (attachnum (cadr args)) ;; 2nd original argument
           (cmd (car (cddr args))) ;; 3rd original argument
           (attachment (mu4e~view-get-attach msg attachnum))
           (attachment-filename (plist-get attachment :name)))
      (list msg
            attachnum
            (or cmd
                (unify-opening-find-cmd attachment-filename)))))

  (advice-add
   'mu4e-view-open-attachment-with
   :filter-args
   'unify-opening-mu4e-view-open-attachment-with)

  (defun unify-opening-mu4e-view-open-attachment (original-fun &optional msg attnum)
    "Open attachment number ATTNUM from MSG.
If MSG is nil use the message returned by `message-at-point'.
If ATTNUM is nil ask for the attachment number."
    (interactive)
    (let* ((msg (or msg (mu4e-message-at-point)))
           (attnum (or attnum
                       (mu4e~view-get-attach-num "Attachment to open" msg)))
           (att (or (mu4e~view-get-attach msg attnum)))
           (index (plist-get att :index))
           (docid (mu4e-message-field msg :docid))
           (mimetype (plist-get att :mime-type)))
      (if (and mimetype (string= mimetype "message/rfc822"))
          (funcall original-fun msg attnum)
        (mu4e-view-open-attachment-with msg attnum))))

  (advice-add
   'mu4e-view-open-attachment
   :around
   'unify-opening-mu4e-view-open-attachment))

;;; When listing files from Helm, make sure the "Open file externally" action
;;; uses `unify-opening'.
(with-eval-after-load "helm-external"
  (defun unify-opening-helm-get-default-program-for-file (filename)
    "Use `unify-opening-find-cmd' to select which command to open files with.
This method will be triggered when typing\\<helm-find-files-map> \\[helm-ff-run-open-file-externally] during execution of `helm-find-files' (\\<global-map>\\[helm-find-files])."
    (unify-opening-find-cmd filename))
  (advice-add
   'helm-get-default-program-for-file
   :override
   'unify-opening-helm-get-default-program-for-file))

;;; Make sure to use Helm (if it is loaded) when choosing an application to open
;;; a file.
(with-eval-after-load "helm"
  (with-eval-after-load "dired-x"
    (defun my:dired-guess-shell-command (original-fun prompt files)
      "Ask user with PROMPT for a shell command, guessing a default from FILES."
      (let ((default (dired-guess-default files)))
        (if (or (null default) (not (listp default)))
            (funcall original-fun prompt files)
          (let ((choice (helm
                         :prompt "command: "
                         :sources `(((name . "Commands")
                                     (candidates . ,default)
                                     (action . (("Execute" . identity))))))))
            (or choice (funcall original-fun prompt files))))))
    (advice-add
     'dired-guess-shell-command
     :around
     'my:dired-guess-shell-command)))

(provide 'unify-opening)

;;; unify-opening.el ends here
