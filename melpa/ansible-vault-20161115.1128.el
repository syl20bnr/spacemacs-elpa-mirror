;;; ansible-vault.el --- Minor mode for editing ansible vault files

;; Copyright (C) 2016 Zachary Elliott
;;
;; Authors: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>
;; URL: http://github.com/zellio/ansible-vault-mode
;; Package-Version: 20161115.1128
;; Created: 2016-09-25
;; Version: 0.3.3
;; Keywords: ansible, ansible-vault, tools
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; License:

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Code:

(defconst ansible-vault-version "0.3.3"
  "`ansible-vault' version.")

(defgroup ansible-vault nil
  "`ansible-vault' application group."
  :group 'applications
  :link '(url-link :tag "Website for ansible-vault-mode"
                   "https://github.com/zellio/ansible-vault-mode")
  :prefix "ansible-vault-")

(defcustom ansible-vault-command "ansible-vault"
  "`ansible-vault' shell command."
  :type 'string
  :group 'ansible-vault)

(defcustom ansible-vault-pass-file (expand-file-name ".vault-pass" "~")
  "File containing `ansible-vault' password.

This file is used for encryption and decryption of ansible vault
files.  If it is set to `nil' `ansible-vault-mode' will prompt
you for a password."
  :type 'string
  :group 'ansible-vault)

;;TODO: Make this more robust to version changes
(defvar ansible-vault--file-header "$ANSIBLE_VAULT;1.1;AES256"
  "`ansible-vault' file header for identification of encrypted buffers.

This will probably change at somepoint in the future and break
everything and that will be sad.")

(defvar ansible-vault--point 0
  "Internal variable for `ansible-vault-mode'

This is used to store the point between the encryption and
decryption process on save to maintain continuity.")

(defun ansible-vault--is-vault-file ()
  "Identifies if the current buffer is an encrypted
  `ansible-vault' file.

This function just looks to see if the first line of the buffer
is `ansible-vault--file-header'."
  (let ((header-length (+ 1 (length ansible-vault--file-header))))
    (and (> (point-max) header-length)
         (string= ansible-vault--file-header
                  (buffer-substring-no-properties (point-min) header-length)))
    ))

(defun ansible-vault--error-buffer ()
  "Generate or return `ansible-vault' error report buffer."
  (or (get-buffer "*ansible-vault-error*")
      (let ((buffer (get-buffer-create "*ansible-vault-error*")))
        (save-current-buffer
          (set-buffer buffer)
          (setq-local buffer-read-only t))
        buffer)))

(defun ansible-vault--call-command (command)
  "Generate `ansible-vault' command with common args.

Ansible vault is called with the same arguments in both the
encryption and decryption case. Use this to generate the
substring shared between them."
  (format "%s --vault-password-file='%s' --output=- %s"
          ansible-vault-command
          ansible-vault-pass-file
          command))

(defun ansible-vault-decrypt-current-buffer ()
  "In place decryption of `current-buffer' using `ansible-vault'."
  (let ((inhibit-read-only t))
    (shell-command-on-region
     (point-min) (point-max)
     (ansible-vault--call-command 'decrypt)
     (current-buffer) t
     (ansible-vault--error-buffer))
    ))

(defun ansible-vault-encrypt-current-buffer ()
  "In place encryption of `current-buffer' using `ansible-vault'."
  (let ((inhibit-read-only t))
    (shell-command-on-region
     (point-min) (point-max)
     (ansible-vault--call-command 'encrypt)
     (current-buffer) t
     (ansible-vault--error-buffer))
    ))

(defvar ansible-vault-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `ansible-vault' minor mode.")

(defun ansible-vault--before-save-hook ()
  "`before-save-hook' for files managed by `ansible-vault-mode'.

Saves the current position and encrpyts the file before writing
to disk."
  (setq-local ansible-vault--point (point))
  (ansible-vault-encrypt-current-buffer))

(defun ansible-vault--after-save-hook ()
  "`after-save-hook' for files managed by `ansible-vault-mode'.

Decrypts the file, and returns the point to the position saved by
the `before-save-hook'."
  (ansible-vault-decrypt-current-buffer)
  (set-buffer-modified-p nil)
  (goto-char ansible-vault--point)
  (setq-local ansible-vault--point 0))

;;;###autoload
(define-minor-mode ansible-vault-mode
  "Minor mode for manipulating ansible-vault files"
  :lighter " ansible-vault"
  :keymap ansible-vault-mode-map
  :group 'ansible-vault

  (if ansible-vault-mode
      ;; Enable the mode
      (progn
        ;; Disable backups
        (setq-local backup-inhibited t)

        ;; Disable auto-save
        (if auto-save-default (auto-save-mode -1))

        ;; Decrypt the current buffer first if it needs to be
        (when (ansible-vault--is-vault-file)
            (ansible-vault-decrypt-current-buffer)
            (set-buffer-modified-p nil))

        ;; Add mode hooks
        (add-hook 'before-save-hook 'ansible-vault--before-save-hook t t)
        (add-hook 'after-save-hook 'ansible-vault--after-save-hook t t))

    ;; Disable the mode
    (remove-hook 'after-save-hook 'ansible-vault--after-save-hook t)
    (remove-hook 'before-save-hook 'ansible-vault--before-save-hook t)

    ;; Re-encrypt the current buffer
    (if (not (ansible-vault--is-vault-file))
        (ansible-vault-encrypt-current-buffer))

    (if auto-save-default (auto-save-mode 1))

    (setq-local backup-inhibited nil)))

(provide 'ansible-vault)

;;; ansible-vault.el ends here
