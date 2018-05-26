;;; passmm.el --- A minor mode for pass (Password Store).  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018 Peter Jones <pjones@devalot.com>

;; Author: Peter Jones <pjones@devalot.com>
;; Homepage: https://github.com/pjones/passmm
;; Package-Requires: ((emacs "24.4") (password-store "0"))
;; Package-Version: 0.3.1
;; Version: 0.3.1
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This is a minor mode that uses `dired' to display all password
;; files from the password store.  It also contains an optional
;; interface for Helm.

;;; License:
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:
(require 'dired)
(require 'helm nil t)
(require 'password-store)

(defgroup passmm nil
  "A minor mode for pass (Password Store)."
  :version "0.2.0"
  :prefix "passmm-"
  :group 'applications)

(defcustom passmm-store-directory
  (or (getenv "PASSWORD_STORE_DIR") "~/.password-store")
  "The directory pass uses to store passwords in."
  :type 'string
  :group 'passmm)

(defcustom passmm-kill-timeout
  (let ((env (getenv "PASSWORD_STORE_CLIP_TIME")))
    (if env (string-to-number env) 45))
  "How long to wait before removing a password from the kill ring."
  :type 'number
  :group 'passmm)

(defcustom passmm-password-length 15
  "Length of generated passwords."
  :type 'integer
  :group 'passmm)


;;; Nothing interesting after this point.
(defvar passmm-buffer-name "*passwords*"
  "Name to use for the dired buffer.")

(defvar passmm-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-p +")  'passmm-generate-password)
    (define-key map (kbd "RET")        'passmm-edit-entry)
    (define-key map (kbd "C-<return>") 'passmm-kill-password)
    map)
  "Default keymap for passmm.")

(defvar passmm-helm-source
  (when (fboundp 'helm)
    (helm-make-source "Password File" 'helm-source-sync
      :candidates #'password-store-list
      :action '(("Kill Password" . passmm-kill-password)
                ("Edit Password" . passmm-edit-entry))))
  "Internal variable to track password files for Helm.")

;;;###autoload
(defun passmm-list-passwords ()
  "List all passwords using a `dired' buffer.

The created `dired' buffer will have `passmm' running inside it.
If a `passm' buffer already exists it is made to be the current
buffer and refreshed."
  (interactive)
  (let ((buf (or (get-buffer passmm-buffer-name)
                 (dired-noselect (expand-file-name passmm-store-directory)
                                 (concat dired-listing-switches " -R")))))
    (if (string= (buffer-name buf) passmm-buffer-name)
        (with-current-buffer buf
          (revert-buffer))
      (with-current-buffer buf
        (rename-buffer passmm-buffer-name)
        (passmm-mode 1)))
    (switch-to-buffer buf)))

;;;###autoload
(defun passmm-helm ()
  "Helm interface for passmm."
  (interactive)
  (if (fboundp 'helm)
      (helm :sources 'passmm-helm-source
            :buffer "*helm-passmm*")
    (error "Helm doesn't appear to be installed")))

(defun passmm-edit-entry (entry &optional keep-password)
  "Edit a password file for ENTRY.

If ENTRY is nil, use the file under point in the `dired' buffer.

The buffer will be narrowed so that it doesn't actually show the
password (the first line).  If KEEP-PASSWORD is non-nil then no
narrowing will be used and the entire file will be shown."
  (interactive (list (dired-get-file-for-visit)
                     current-prefix-arg))
  (let ((name (passmm-entry-to-file-name entry)))
    (if (and (file-exists-p name) (not (file-directory-p name)))
        (progn
          (find-file name)
          (when (not keep-password)
            (passmm-narrow-buffer (current-buffer))))
      (dired-maybe-insert-subdir name))))

(defun passmm-kill-password (entry &optional show-entry)
  "Store a password on the kill ring for ENTRY.

The password is taken from the file that is at point.  After
`passmm-kill-timeout' seconds, the password will be removed from
the kill ring and the system clipboard.

If SHOW-ENTRY is non-nil also display the password file narrowed
so that it doesn't show the password line."
  (interactive (list (dired-get-file-for-visit)
                     current-prefix-arg))
  (let ((name (passmm-entry-to-file-name entry))
        history-pointer password buffer)
    (if (and (file-exists-p name) (not (file-directory-p name)))
        (save-excursion
          (find-file name)
          (goto-char (point-min))
          (setq password (buffer-substring-no-properties
                          (point) (progn (end-of-line) (point))))
          (kill-new password)
          (setq history-pointer kill-ring-yank-pointer)
          (if show-entry (setq buffer (current-buffer))
            (kill-buffer))
          (message "Copied %s to clipboard. Will clear in %s seconds."
                   (passmm-relative-path name) passmm-kill-timeout)
          (run-at-time passmm-kill-timeout nil
            (lambda ()
              ;; This is a bit of a mess.  We need to figure out if
              ;; the system clipboard still contains the original
              ;; password, and if so remove it.  However, if the
              ;; clipboard hasn't changed since we last set it then
              ;; Emacs reports the system clipboard as `nil'.
              (when (and interprogram-paste-function interprogram-cut-function)
                (let ((clipboard (funcall interprogram-paste-function)))
                  (when (or (string-equal password clipboard)
                            (and (not clipboard)
                                 (eq history-pointer kill-ring-yank-pointer)))
                    (funcall interprogram-cut-function ""))))
              (setcar history-pointer "")
              (message "Password cleared."))))
      (message "%s is not a file" name))
    (when buffer
      (passmm-narrow-buffer buffer)
      (switch-to-buffer buffer))))

(defun passmm-generate-password (ask-dir)
  "Generate a password entry after asking for its name.

If ASK-DIR is non-nil then you'll be prompted for the name of
directory to store the entry in.  Otherwise it's taken from the
current directory in the `dired' buffer."
  (interactive "P")
  (let* ((store (expand-file-name passmm-store-directory))
         (adir (if (and (string= major-mode "dired-mode") (not ask-dir))
                   (dired-current-directory)
                 (read-directory-name "New Password Location: " store)))
         (rdir (file-relative-name adir store))
         (name (read-string (concat "Password Name (in " rdir "): ")))
         (jump (lambda nil (dired-goto-file (concat adir name ".gpg")))))
    (passmm-pass jump "generate" (concat (file-name-as-directory rdir) name)
                 (number-to-string passmm-password-length))))

(defun passmm-relative-path (path)
  "Make the absolute PATH relative to the password store."
  (let ((store (expand-file-name passmm-store-directory)))
    (file-name-sans-extension (file-relative-name path store))))

(defun passmm-narrow-buffer (buffer)
  "Narrow BUFFER so that it doesn't include the first line."
  (with-current-buffer buffer
    (widen)
    (goto-char (point-min))
    (forward-line)
    (forward-whitespace 1)
    (forward-line 0)
    (narrow-to-region (point) (point-max))))

(defun passmm-entry-to-file-name (entry)
  "Convert ENTRY into an absolute file name."
  (let* ((ext (file-name-extension entry))
         (file (if (and ext (string= ext "gpg")) entry
                 (concat entry ".gpg"))))
    (if (file-name-absolute-p entry) file
      (concat (file-name-as-directory
               (expand-file-name passmm-store-directory))
              file))))

(defun passmm-pass (callback &rest args)
  "Run the pass program and invoke CALLBACK when it completes.
ARGS are given directly to pass unchanged.  (Note: CALLBACK is
invoked with the passmm/dired buffer active.)"
  (let ((p (apply 'start-process "pass" "*pass*" "pass" args)))
    (set-process-sentinel p
      (lambda (_process _event)
        (save-excursion
          (with-current-buffer (or (get-buffer passmm-buffer-name)
                                   (passmm-list-passwords))
            (revert-buffer t t t)
            (and callback (funcall callback))))))))

(define-minor-mode passmm-mode
  "This is a minor mode that uses `dired' to display all password
files from the password store.  It supports the following features:

  * Generate new passwords, storing them in the current `dired'
    subdir (or optionally prompting for a directory).  (See:
    `passmm-generate-password'.)

  * Store the password of a file into the Emacs kill ring and the
    system clipboard for N seconds.  (See:
    `passmm-kill-password'.)

  * Edit a password file with narrowing so the password isn't
    show.  (See: `passmm-edit-entry'.)

Typically you'll want to start passmm by calling
`passmm-list-passwords'."
  :lighter " pass"
  :group 'applications
  :keymap passmm-mode-map)

(provide 'passmm)
;;; passmm.el ends here
