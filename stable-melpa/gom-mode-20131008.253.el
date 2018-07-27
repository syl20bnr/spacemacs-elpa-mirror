;;; gom-mode.el --- Major mode for Gomfile

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-gom-mode
;; Package-Version: 20131008.253
;; Version: 0.01

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

;;; Code:

(defgroup gom nil
  "A Gomfile major mode"
  :group 'gom)

(defcustom gom-command "gom"
  "The Gom command"
  :type 'string
  :group 'gom)

(defcustom gom-mode-hook nil
  "Hook called by `gom-mode'"
  :type 'hook
  :group 'gom)

(defvar gom-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<backtab>") 'gom-indent-shift-left)
    map)
  "Keymap for Gomfile major mode.")

;;
;; Syntax highlighting
;;

(defvar gom-keywords
  '("group" "do" "end" "gom"))

(defvar gom-keywords-regexp
  (format "\\<%s\\>" (regexp-opt gom-keywords 'words)))

(defvar gom-option-regexp
  "\\(:\\S-+\\)\\s-*=>")

(defvar gom-tag-regexp
  "\\(:\\S-+\\)")

(defvar gom-font-lock-keywords
  `((,gom-keywords-regexp . font-lock-keyword-face)
    (,gom-option-regexp 1 font-lock-constant-face)
    (,gom-tag-regexp . font-lock-builtin-face)))

;;;###autoload
(define-derived-mode gom-mode ruby-mode "Gom"
  "Major mode for editing Gomfile"

  ;; code for syntax highlighting
  (setq font-lock-defaults '((gom-font-lock-keywords))))

;;;###autoload
(add-to-list 'auto-mode-alist '("Gomfile\\'" . gom-mode))

(provide 'gom-mode)

;;; gom-mode.el ends here
