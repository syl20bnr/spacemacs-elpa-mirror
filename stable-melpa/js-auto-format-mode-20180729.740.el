;;; js-auto-format-mode.el --- Minor mode for auto-formatting JavaScript code  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Masafumi Koba <ybiquitous@gmail.com>

;; Author: Masafumi Koba <ybiquitous@gmail.com>
;; Version: 1.1.0
;; Package-Version: 20180729.740
;; Package-Requires: ((emacs "24"))
;; Keywords: languages
;; URL: https://github.com/ybiquitous/js-auto-format-mode
;; Created: Apr 2016
;; License: GNU General Public License v3.0
;; Distribution: This file is not part of Emacs

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

;; Usage:
;;
;;   (add-hook 'js-mode-hook #'js-auto-format-mode)
;;
;; To customize:
;;
;;   M-x customize-group RET js-auto-format RET
;;
;; For details, please see `https://github.com/ybiquitous/js-auto-format-mode'.

;;; Code:

(defgroup js-auto-format nil
  "Minor mode for auto-formatting JavaScript code."
  :group 'languages
  :prefix "js-auto-format-"
  :link '(url-link :tag "Repository" "https://github.com/ybiquitous/js-auto-format-mode"))

(defcustom js-auto-format-command "eslint"
  "Executable command."
  :group 'js-auto-format
  :type 'string
  :safe #'stringp)

(defcustom js-auto-format-command-args "--fix --format=unix"
  "Argument(s) of command."
  :group 'js-auto-format
  :type 'string
  :safe #'stringp)

(defcustom js-auto-format-disabled nil
  "Disable this mode."
  :group 'js-auto-format
  :type 'boolean
  :safe #'booleanp)

(defconst js-auto-format-buffer "*JS Auto Format*")

(defun js-auto-format-full-command ()
  "Return full command with all arguments."
  (format "%s %s %s"
    (shell-quote-argument (executable-find js-auto-format-command))
    js-auto-format-command-args
    (shell-quote-argument (expand-file-name buffer-file-name))))

;;;###autoload
(defun js-auto-format-enabled-p ()
  "Test whether js-auto-format-mode is enabled."
  (and
    (not buffer-read-only)
    (not js-auto-format-disabled)
    (buffer-file-name)
    (not (string-match-p "/node_modules/" buffer-file-name))))

;;;###autoload
(defun js-auto-format-execute ()
  "Format JavaScript source code."
  (interactive)
  (when (js-auto-format-enabled-p)
    (let* ((command (js-auto-format-full-command))
            (buffer js-auto-format-buffer)
            (saved-current-buffer (current-buffer)))

      (message "js-auto-format-execute: %s" command)

      (with-output-to-temp-buffer buffer
        (let* ((exit-status (call-process-shell-command command nil buffer nil)))
          (revert-buffer t t t)
          (pop-to-buffer buffer)
          (if (zerop exit-status) (quit-window t) (shrink-window-if-larger-than-buffer))
          (pop-to-buffer saved-current-buffer)
          (if (fboundp 'flycheck-buffer) (flycheck-buffer)))))))

;;;###autoload
(define-minor-mode js-auto-format-mode
  "Minor mode for auto-formatting JavaScript code"
  :group 'js-auto-format
  :lighter " AutoFmt"
  (if js-auto-format-mode
    (add-hook 'after-save-hook 'js-auto-format-execute t t)
    (remove-hook 'after-save-hook 'js-auto-format-execute t)))

(provide 'js-auto-format-mode)
;;; js-auto-format-mode.el ends here
