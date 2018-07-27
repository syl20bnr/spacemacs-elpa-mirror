;;; scratches.el --- Multiple scratches in any language

;; Copyright (C) 2015 Zhang Kai Yu

;; Author: Zhang Kai Yu <yeannylam@gmail.com>
;; Keywords: scratch
;; Package-Version: 20151006.416
;; Package-Requires: ((dash "2.11.0") (f "0.17.0"))

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

;; Create and save multiple scratches in any language with this package.

;;; Code:

(require 'f)
(require 'dash)
(require 'ido)

(defgroup scratches nil
  "Multiple scratches in any language."
  :group 'convenience
  :prefix "scratch-")

(defcustom scratches-save-location
  (expand-file-name "scratches" user-emacs-directory)
  "The directory to store scratches."
  :group 'scratches
  :type 'string)

(defcustom scratches-untitled-name
  "Untitled"
  "The untitled name."
  :group 'scratches
  :type 'string)

(defcustom scratches-auto-incremental-name
  'scratches--default-auto-incremental-name
  "The auto incremental name function."
  :group 'scratches
  :type 'function)

(defcustom scratches-keymap-prefix (kbd "C-c C-r")
  "Scratches keymap prefix. You must set your own easy to use keymap prefix."
  :group 'scratches
  :type 'string)

(defvar scratches-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'scratches-visit-scratch)
    (define-key map (kbd "4 f") #'scratches-visit-scratch-other-window)
    (define-key map (kbd "5 f") #'scratches-visit-scratch-other-frame)
    (define-key map (kbd "n") #'scratches-new-scratch-dwim)
    (define-key map (kbd "4 n") #'scratches-new-scratch-other-window-dwim)
    (define-key map (kbd "5 n") #'scratches-new-scratch-other-frame-dwim)
    (define-key map (kbd "k") #'scratches-kill-all-scratches)
    map)
  "Keymap for Scratches commands after `scratches-keymap-prefix'.")
(fset 'scratches-command-map scratches-command-map)

(defvar scratches-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map scratches-keymap-prefix 'scratches-command-map)
    map)
  "Keymap for Scratches mode.")

(defvar scratches-last-visited-scratch-file nil
  "Last visited scratches file.")

(defun scratches--default-auto-incremental-name ()
  "Return the name that can be used for a new scratch file."
  (let* ((basen scratches-untitled-name)
         (retval basen)
         (index 0))
    (while (f-exists? (f-expand retval scratches-save-location))
      (setq index (1+ index))
      (setq retval (format "%s %s" basen index)))
    retval))

(defun scratches--maybe-create-scratch-dir ()
  "Create scratch directory if it's not exist."
  (unless (file-directory-p scratches-save-location)
    (make-directory scratches-save-location)))

(defun scratches--get-scratch-name ()
  "Get a scratch file name."
  (scratches--maybe-create-scratch-dir)
  (let (file-names)
    (setq file-names (-map (lambda (f) (f-relative f scratches-save-location))
                           (f-files scratches-save-location nil t)))
    (ido-completing-read "Visit scratch: " file-names nil nil)))

(defun scratches-visit-scratch (name)
  "Visit scratch file with NAME."
  (interactive (list (scratches--get-scratch-name)))
  (find-file (f-expand name scratches-save-location)))

(defun scratches-visit-scratch-other-window (name)
  "Visit scratch file with NAME other window."
  (interactive (list (scratches--get-scratch-name)))
  (find-file-other-window (f-expand name scratches-save-location)))

(defun scratches-visit-scratch-other-frame (name)
  "Visit scratch file with NAME other window."
  (interactive (list (scratches--get-scratch-name)))
  (find-file-other-frame (f-expand name scratches-save-location)))

(defun scratches-new-scratch-dwim ()
  "Automatically create a new scratch based on current mode."
  (interactive)
  (scratches--maybe-create-scratch-dir)
  (let ((mm major-mode))
    (find-file (f-expand (funcall scratches-auto-incremental-name)
                         scratches-save-location))
    (funcall (indirect-function mm))))

(defun scratches-new-scratch-other-window-dwim ()
  "Automatically create a new scratch based on current mode."
  (interactive)
  (scratches--maybe-create-scratch-dir)
  (let ((mm major-mode))
    (find-file-other-window (f-expand (funcall scratches-auto-incremental-name)
                                      scratches-save-location))
    (funcall (indirect-function mm))))

(defun scratches-new-scratch-other-frame-dwim ()
  "Automatically create a new scratch based on current mode."
  (interactive)
  (scratches--maybe-create-scratch-dir)
  (let ((mm major-mode))
    (find-file-other-frame (f-expand (funcall scratches-auto-incremental-name)
                                     scratches-save-location))
    (funcall (indirect-function mm))))

(defun scratches-kill-all-scratches ()
  "Kill all scratch buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (f-child-of?
           (or (buffer-file-name buffer) "") scratches-save-location)
      (kill-buffer buffer)))
  (message "Killed all scratch buffers."))

(defun scratches-record-last-scratch ()
  "Record last scratch file visited."
  (let ((cb (buffer-file-name (current-buffer))))
    (if (f-child-of? cb  scratches-save-location)
        (setq scratches-last-visited-scratch-file cb))))

(defun scratches-visit-last-scratch ()
  "Visit last scratch file."
  (interactive)
  (if (and scratches-last-visited-scratch-file
           (f-file? scratches-last-visited-scratch-file))
      (find-file scratches-last-visited-scratch-file)
    (switch-to-buffer "*scratches*")))

;; (defun scratches-switch-scratch ()
;;   "Switch opened scratch."
;;   (interactive))

;; (defun scratches-switch-scratch-dwim ()
;;   "Switch to scratch do what you mean."
;;   (interactive))

;; (defun scratches-switch-scratch-other-window-dwim ()
;;   "Switch to scratch do what you mean."
;;   (interactive))

;; (defun scratches-switch-scratch-other-frame-dwim ()
;;   "Switch to scratch do what you mean."
;;   (interactive))

;;;###autoload
(define-minor-mode scratches-mode
  "Multiple scratches in any language.

When called interactively, toggle `scratches-mode'.  With prefix
ARG, enable `scratches-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `scratches-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `scratches-mode'.
Otherwise behave as if called interactively.

\\{scratches-mode-map}"
  :lighter projectile-mode-line
  :keymap scratches-mode-map
  :group 'convenience
  :require 'scratches
  (if scratches-mode
      (progn
        (add-hook 'find-file-hook 'scratches-record-last-scratch nil t))
    (remove-hook 'find-file-hook 'scratches-record-last-scratch t)))

;;;###autoload
(define-globalized-minor-mode scratches-global-mode
  scratches-mode
  scratches-mode)

(provide 'scratches)
;;; scratches.el ends here
