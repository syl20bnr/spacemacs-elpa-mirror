;;; bshell.el --- manage and track multiple inferior shells

;; Copyright (C) 2015 - 2017 Paul Landes

;; Version: 0.1
;; Package-Version: 20170116.1117
;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: interactive shell management
;; URL: https://github.com/plandes/bshell
;; Package-Requires: ((emacs "25") (buffer-manage "0.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package allows an Emacs user to create, delete, rename and fast swtich
;; between multiple inferior shells using the buffer-manager library.  This is
;; done by extending the `buffer-manage` and using its functions and
;; keybindings to manage multiple inferior shells.  To summarize, this
;; includes:
;; * A major mode for switching to, deleting and starting new shells.
;; * Fast switching with customized key bindings through the customize
;;   framework.
;; * Track and rename shells by name through the shell entry management mode.
;; * Interact with buffer shell (entries) as objects with a straight forward
;;   API.

;;; Usage:

;; You can start a new shell with `C-x C-h`.  Do this again to get another
;; shell that lives as a *separate* process in a buffer.  Use `C-tab` to get a
;; list of shells in the *Entries* buffer where you can rename, delete, switch
;; or add new shells.  To "fast" switch use `C-x C-h`, which changes the
;; current window's shell to the last used or next shell based on the current
;; cycling method.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'eieio-custom)
(require 'shell)
(require 'buffer-manage)
(require 'bookmark)

(defclass bshell-entry (buffer-entry) ())

(cl-defmethod buffer-entry-create-buffer ((this bshell-entry))
  (shell))

(cl-defmethod buffer-manage-entry-jump-directory ((this bshell-entry) bookmark)
  "Jump to directory to the directory given from BOOKMARK."
  (require 'bookmark)
  (bookmark-maybe-load-default-file)
  (let* ((file (bookmark-get-filename bookmark))
	 (dir (if (file-directory-p file)
		  file
		(file-name-directory file))))
    (buffer-entry-insert this (format "cd %s" dir) t)))

(defclass bshell-manager (buffer-manager) ())

(cl-defmethod buffer-manager-conical-name ((this bshell-manager)) "bshell")

(cl-defmethod buffer-manager-name ((this bshell-manager)) "shell")

(cl-defmethod buffer-manager-create-entry ((this bshell-manager) &rest args)
  (apply 'bshell-entry nil args))

(cl-defmethod buffer-manager-start-dir ((this bshell-manager)) default-directory)

(cl-defmethod buffer-manage-read-working-directory ((this bshell-manager))
  "Read an entry name by prompting the user by the entry's working directory."
  (cl-flet ((entry-wd
	      (entry)
	      (with-current-buffer (buffer-entry-buffer entry)
		(let ((dir (abbreviate-file-name default-directory)))
		  (format "%s (%s)" dir (buffer-entry-name entry))))))
    (let ((completion-ignore-case t))
      (buffer-manager-read-name this "Switch by dir" t nil #'entry-wd))))

(cl-defmethod buffer-manager-interactive-functions ((this bshell-manager)
						    singleton-variable-sym)
  (append
   (cl-call-next-method this singleton-variable-sym)
   `(("jump-directory"
      (defun ,(intern (format "%s-jump-directory"
			      (buffer-manager-conical-name this)))
	  (bookmark)
	"Jump to a bookmark in the current buffer."
	(interactive (list (bookmark-completing-read "Jump to directory")))
	(let* ((this ,singleton-variable-sym)
	       (entry (buffer-manager-current-instance this)))
	  (if entry (buffer-manage-entry-jump-directory entry bookmark)))))
     ("switch-by-working-directory"
      (defun ,(intern (format "%s-switch-by-working-directory"
			      (buffer-manager-conical-name this)))
	  (name)
	"Switch to an entry prompting by working directory."
	(interactive
	 (list (buffer-manage-read-working-directory ,singleton-variable-sym)))
	(let* ((this ,singleton-variable-sym)
	       (entry (buffer-manager-switch this (or name 'cycle))))))))))

(cl-defmethod buffer-manager-key-bindings ((this bshell-manager))
  (append (cl-call-next-method this)
	  '(("jump-directory" shell-mode-map "C-c C-g")
	    ("rename" shell-mode-map "C-c C-t")
	    ("switch-by-working-directory" shell-mode-map "C-c C-q"))))

(defgroup bshell nil
  "Interactive Object Oriented Shell"
  :group 'buffer-manage
  :prefix "bshell-")

(defcustom bshell-manager-singleton
  (bshell-manager "singleton")
  "The singleton bshell manager."
  :group 'bshell
  :type 'object)

;; creates interactive function `bshell-new' etc
(buffer-manager-create-interactive-functions
 bshell-manager-singleton 'bshell-manager-singleton)

(provide 'bshell)

;;; bshell.el ends here
