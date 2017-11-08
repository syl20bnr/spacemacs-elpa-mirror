;;; tramp-theme.el --- Custom theme for remote buffers

;; Copyright (C) 2016-2017 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: convenience, faces
;; Package: tramp-theme
;; Version: 0.2
;; Package-Requires: ((emacs "24.1"))

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is not an own custom theme by itself.  Rather, it is a custom
;; theme to run on top of other custom themes.  It shall be loaded
;; always as the last custom theme, because it inherits existing
;; settings.

;; This custom theme extends `mode-line-buffer-identification' by the
;; name of the remote host.  It also allows to change faces according
;; to the value of `default-directory' of a buffer.  See
;; `tramp-theme-face-remapping-alist' for customization options.

;;; Code:

;; This is needed for the customized variables.
(require 'dired)
(require 'em-dirs)

(deftheme tramp
  "A custom theme to decorate buffers when they are remote.
It can be combined with other custom themes.")

(defcustom tramp-theme-face-remapping-alist
  `((nil "^root$"
    (mode-line-buffer-id
     (:inherit mode-line-buffer-id
      :inverse-video
      ;; If the face uses already :inverse-video, we deactivate it.
      ;; Happens on displays of type 'tty, for example.
      ,(null
	(face-inverse-video-p
	 'mode-line-buffer-id nil '(mode-line default)))))))
  "Face remapping for decoration of a remote buffer.
This is an alist of items (HOST USER REMAPPING-LIST).  HOST and
USER are regular expressions, or nil.  REMAPPING-LIST must be an
alist of face remappings as used by `face-remapping-alist'.  If
USER matches the remote user part of `default-directory', and
HOST matches the remote host part of `default-directory',
REMAPPING-LIST is applied to the current buffer.

For instance, the following settings change the background color
to \"Red\" for frames connected to the remote host \"foo\", it
changes the background color to \"Green\" for frames connected to
the remote host \"bar\", and it inverses the fringe face for
frames using the remote user \"root\":

    ((nil \"^root$\" (fringe (:inherit fringe :inverse-video t)))
     (\"^foo$\" nil (default (:background \"Red\")))
     (\"^foo$\" nil (dired-directory (:background \"Red\")))
     (\"^foo$\" nil (eshell-prompt (:foreground \"White\")))
     (\"^bar$\" nil (default (:background \"Green\")))
     (\"^bar$\" nil (dired-directory (:background \"Green\"))))

Per default, `mode-line-buffer-identification' is displayed
inverse for buffers which are editable with \"root\" permissions."
  :group 'tramp
  :type `(repeat (list (choice :tag "Host regexp" regexp (const nil))
		       (choice :tag "User regexp" regexp (const nil))
		       (list :tag "Face Remapping"
			     face (plist :value-type sexp)))))

(defun tramp-theme-original-value (variable)
  "Return the original value of VARIABLE before loading `tramp-theme'."
  (let ((theme-value (get variable 'theme-value)))
    (or (cdr (car (delete (assoc 'tramp theme-value) theme-value)))
	(get variable 'tramp-theme-original-value))))

(defvar-local tramp-theme-face-remapping-cookies nil
  "Cookies store of local face remapping settings.")

(defun tramp-theme-mode-line-buffer-identification ()
  "Return a list suitable for `mode-line-buffer-identification'.
It indicates the remote host being used, if any.

Per side effect, it enables also face remapping in the current buffer."
  ;; Clear previous face remappings.
  (mapc 'face-remap-remove-relative tramp-theme-face-remapping-cookies)
  (setq tramp-theme-face-remapping-cookies nil)

  (append
   (when (custom-theme-enabled-p 'tramp)
     (let ((host (file-remote-p default-directory 'host))
	   (user (file-remote-p default-directory 'user)))
       ;; Apply `tramp-theme-face-remapping-alist'.
       (dolist (elt tramp-theme-face-remapping-alist)
	 (when (and (string-match (or (nth 0 elt) "") (or host ""))
		    (string-match (or (nth 1 elt) "") (or user "")))
	   (push (face-remap-add-relative (car (nth 2 elt)) (cdr (nth 2 elt)))
		 tramp-theme-face-remapping-cookies)))

       ;; The extended string.
       (when host
	 ;; Do not use FQDN.
	 (when (string-match "^[^0-9][^.]*\\(\\..*\\)" host)
	   (setq host (substring host 0 (match-beginning 1))))
	 (list
	  (propertize
	   (concat (propertize host 'help-echo (purecopy "Host name")) ": ")
	   'face 'mode-line-buffer-id 'mouse-face 'mode-line-highlight)))))

   ;; That's the original definition.
   (tramp-theme-original-value 'mode-line-buffer-identification)))

(defun tramp-theme-hook-function ()
  "Modify `mode-line-buffer-indication'.
Used in different hooks, in order to accelerate the redisplay."
  (setq
   mode-line-buffer-identification
   (tramp-theme-mode-line-buffer-identification)))

(unless (custom-theme-enabled-p 'tramp)
  ;; Save the original value.
  (unless (get 'mode-line-buffer-identification 'tramp-theme-original-value)
    (put 'mode-line-buffer-identification
	 'tramp-theme-original-value
	 mode-line-buffer-identification))

  (custom-theme-set-variables
   'tramp
   ;; Extend `mode-line-buffer-identification' by host name.
   '(mode-line-buffer-identification
     '(:eval (tramp-theme-mode-line-buffer-identification)))
   ;; `dired-mode' overwrites `mode-line-buffer-identification'.  We
   ;; want to use our own extension.
   '(dired-mode-hook
     (cons
      'tramp-theme-hook-function
      (delete 'tramp-theme-hook-function dired-mode-hook)))
   ;; Redisplay doesn't happen immediately.  So we trigger it via
   ;; `find-file-hook' and `eshell-directory-change-hook'.
   '(find-file-hook
     (cons
      'tramp-theme-hook-function
      (delete 'tramp-theme-hook-function find-file-hook)))
   '(eshell-directory-change-hook
     (cons
      'tramp-theme-hook-function
      (delete 'tramp-theme-hook-function eshell-directory-change-hook)))))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

;;;; ChangeLog:

;; 2017-09-28  Michael Albinus  <michael.albinus@gmx.de>
;; 
;; 	* packages/tramp-theme/tramp-theme.el: Increase Version to 0.2.
;; 
;; 	(tramp-theme-face-remapping-alist): Adapt docstring.
;; 	(tramp-theme-face-remapping-cookies): New defvar.
;; 	(tramp-theme-mode-line-buffer-identification): Apply face remapping
;; 	relative.
;; 
;; 2016-07-11  Paul Eggert	 <eggert@cs.ucla.edu>
;; 
;; 	Fix some quoting problems in doc strings
;; 
;; 	Most of these are minor issues involving, e.g., quoting `like this' 
;; 	instead of 'like this'.	 A few involve escaping ` and ' with a preceding
;; 	\= when the characters should not be turned into curved single quotes.
;; 
;; 2016-06-14  Michael Albinus  <michael.albinus@gmx.de>
;; 
;; 	* packages/tramp-theme/tramp-theme.el: Remove file-local variables.
;; 
;; 2016-02-17  Michael Albinus  <michael.albinus@gmx.de>
;; 
;; 	* packages/tramp-theme/tramp-theme.el: Add autoload cookie.
;; 
;; 2016-02-17  Michael Albinus  <michael.albinus@gmx.de>
;; 
;; 	Oops, release tramp-theme 0.1.1
;; 
;; 	* packages/tramp-theme/README:
;; 	* packages/tramp-theme/tramp-theme.el: New files.
;; 
;; 2016-02-17  Michael Albinus  <michael.albinus@gmx.de>
;; 
;; 	; Minor changes to tramp-theme.el
;; 
;; 2016-02-17  Michael Albinus  <michael.albinus@gmx.de>
;; 
;; 	Add tramp-theme
;; 
;; 	* packages/tramp-theme/README:
;; 	* packages/tramp-theme/tramp-theme.el: New files.
;; 


(provide-theme 'tramp)

;;; TODO:

;; * Use a :type for `tramp-theme-face-remapping-alist' which allows
;;   to edit the faces.  Maybe use (widget-get custom-face-edit :args)
;;   for this.

;;; tramp-theme.el ends here
