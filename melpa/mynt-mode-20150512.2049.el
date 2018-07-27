;;; mynt-mode.el --- Minor mode to work with the mynt static site generator

;; Author: Christian Brassat
;; URL: https://github.com/crshd/mynt-mode
;; Package-Version: 20150512.2049
;; Version: 0.2.1
;; Created: 2015-03-12
;; Keywords: convenience
;; Package-Requires: ((virtualenvwrapper "20131514"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; mynt is great.  But I've gotten sick of switching back and forth between Emacs
;; and Terminal.  That's where this package comes in; common actions around mynt
;; are wrapped in Emacs commands, callable by quick and memorable keybindings.
;; Yay!

;; To enable this minor mode, call `mynt-mode` to enable it.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(defvar mynt-location nil
  "The base location of your mynt install.")
(defvar mynt-source nil
  "mynt source directory.")
(defvar mynt-destination nil
  "mynt destination (production) directory.")
(defvar mynt-venv nil
  "Virtualenv used for mynt.")

(defvar mynt-projects
  '(("default" . ((location    . "~/www/mynt/")
                  (source      . "source")
                  (destination . "output")
                  (venv        . nil))))
  "Alist of alist of mynt projects.")

(defun mynt-pick-project (project)
  "Pick the current mynt project."
  (interactive (list (completing-read "Choose a project: " mynt-projects)))
  (let ((p (assoc project mynt-projects)))
    (setq mynt-location (cdr (assoc 'location p))
          mynt-source (cdr (assoc 'source p))
          mynt-destination (cdr (assoc 'destination p))
          mynt-venv (cdr (assoc 'venv p)))))

(defun mynt-make-post (title tags layout)
  "Create a new post for mynt.
Argument TITLE Title of the new post.
Argument TAGS Comma-separated list of tags for the post.
Argument LAYOUT Layout to be used.  Defaults to post.html."
  ;; Query for frontmatter values
  (interactive (list
                (read-string "Title: ")
                (read-string "Tags: ")
                (read-string "Layout (post.html): " nil nil "post.html")))

  ;; Define file name
  (let* ((date (format-time-string "%Y-%m-%d-%H-%M-"))
         (filename (concat date (replace-regexp-in-string " " "-" title) ".md")))

    ;; Create new buffer
    (switch-to-buffer (find-file (concat mynt-source "_posts/" filename)))

    ;; If markdown-mode is installed, use it.
    (when (require 'markdown-mode nil 'noerror)
      (markdown-mode))

    ;; Insert frontmatter
    (insert "---\ntitle: " title "\ntags: [" tags "]\nlayout: " layout "\n---\n\n")))

(defun mynt-shell-command (command &optional force source)
  "Call mynt COMMAND.
Optional argument FORCE Use -f flag.
Optional argument SOURCE Command takes a source argument."
  (if mynt-venv
      (venv-workon mynt-venv))
  (let ((s (concat mynt-location mynt-source))
        (d (concat mynt-location mynt-destination)))
    (start-process-shell-command
     (concat "mynt-" command)
     "mynt"
     (concat "mynt " command
             (if force " -f " " ")
             (if source (concat s " "))
             d))))

(defun mynt-generate ()
  "Generate mynt output."
  (interactive)
  (mynt-shell-command "gen" t t))

(defun mynt-watch ()
  "Watch for change in your mynt source."
  (interactive)
  (mynt-shell-command "watch" t t))

(defun mynt-serve ()
  "Serve local mynt."
  (interactive)
  (mynt-shell-command "serve"))

(defun mynt-interrupt-processes ()
  "Interrupt all mynt processes."
  (interactive)
  (interrupt-process "mynt"))

;;;###autoload
(mynt-pick-project (car (car mynt-projects)))

(define-minor-mode mynt-mode
  "Minor mode to work with mynt"
  :lighter " mynt"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c m p") 'mynt-make-post)
            (define-key map (kbd "C-c m g") 'mynt-generate)
            (define-key map (kbd "C-c m w") 'mynt-watch)
            (define-key map (kbd "C-c m s") 'mynt-serve)
            map))

(provide 'mynt-mode)

;;; mynt-mode.el ends here
