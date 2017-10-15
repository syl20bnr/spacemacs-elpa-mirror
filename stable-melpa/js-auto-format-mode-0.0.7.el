;;; js-auto-format-mode.el --- Minor mode for auto-formatting JavaScript code  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 ybiquitous <ybiquitous@gmail.com>

;; Author:  ybiquitous <ybiquitous@gmail.com>
;; Version: 0.0.7
;; Package-Version: 0.0.7
;; Keywords: languages, tools
;; URL: https://github.com/ybiquitous/js-auto-format-mode

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

;; # Setup
;;
;; Install `Node.js' and `ESLint':
;;
;;    npm install -g eslint
;;
;; Put `~/.eslintrc.json'. See `http://eslint.org/'.
;;
;; Enable `js-auto-format-mode':
;;
;;    (add-hook 'js-mode-hook 'js-auto-format-mode)
;;
;; # Change settings
;;
;;    M-x customize-group RET js-auto-format RET
;;
;; # Disabled in any directories
;;
;;    M-x add-dir-local-variable RET js-mode RET js-auto-format-disabled RET t

;;

;;; Code:

;;;###autoload
(defcustom js-auto-format-command "eslint"
  "JavaScript auto format command."
  :group 'js-auto-format
  :type 'string
  :safe #'stringp)

;;;###autoload
(defcustom js-auto-format-command-args "--fix"
  "JavaScript auto format command arguments."
  :group 'js-auto-format
  :type 'string
  :safe #'stringp)

;;;###autoload
(defcustom js-auto-format-disabled nil
  "JavaScript auto format disabled."
  :group 'js-auto-format
  :type 'boolean
  :safe #'booleanp)

(defun js-auto-format-command-dir ()
  (let* ((root (locate-dominating-file default-directory "node_modules"))
         (local-path (expand-file-name "node_modules/.bin" root)))
    (if (file-directory-p local-path)
        (expand-file-name "node_modules/.bin" root))))

(defun js-auto-format-command-path ()
  (let* ((command-path (expand-file-name js-auto-format-command (js-auto-format-command-dir))))
    (if (file-exists-p command-path) command-path js-auto-format-command)))

;;;###autoload
(defun js-auto-format-execute ()
  "Format JavaScript source code."
  (interactive)
  (unless js-auto-format-disabled
    (progn
      (let* ((command (format "\"%s\" %s \"%s\""
                              (js-auto-format-command-path)
                              js-auto-format-command-args
                              (expand-file-name buffer-file-name))))
        (message "js-auto-format-execute: %s" command)
        (shell-command command nil "*Messages*")
        (revert-buffer t t)
        (js-auto-format-mode t)))))

;;;###autoload
(define-minor-mode js-auto-format-mode
  "Minor mode for auto-formatting JavaScript code"
  :init-value nil
  :lighter " AutoFmt"
  (if js-auto-format-mode
      (add-hook 'after-save-hook 'js-auto-format-execute t t)
    (remove-hook 'after-save-hook 'js-auto-format-execute t)))

(provide 'js-auto-format-mode)
;;; js-auto-format-mode.el ends here
