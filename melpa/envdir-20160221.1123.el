;;; envdir.el --- Modify environment according to files in a specified directory

;; Copyright (C) 2015-2016 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/envdir-mode
;; Package-Version: 20160221.1123
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (dash "2.10.0") (f "0.17.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the README for more details.

;;; Code:

(require 'dash)
(require 'f)

(defgroup envdir nil
  "Modify environment according to files in a specified directory."
  :group 'languages)

(defcustom envdir-mode-line-format
  '(:eval
    (when envdir-active-directory
      (concat "Envdir:" (f-base envdir-active-directory) " ")))
  "How `envdir-mode' will indicate current envdir in the mode-line."
  :group 'envdir)

(defvar envdir-active-directory nil
  "Currently activated environment directory.")

;;;###autoload
(defun envdir-set (dirname)
  "Set environment variables from DIRNAME."
  (interactive "DEnvdir: ")
  (envdir-unset)
  (--map (setenv (car it) (cdr it))
         (envdir-read-directory dirname))
  (setq envdir-active-directory dirname)
  (force-mode-line-update))

;;;###autoload
(defun envdir-unset ()
  "Unset environment variables from last activated directory."
  (interactive)
  (when envdir-active-directory
    (--map (setenv (car it))
           (envdir-read-directory envdir-active-directory))
    (setq envdir-active-directory nil))
  (force-mode-line-update))

(defvar envdir-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") 'envdir-set)
    (define-key map (kbd "C-c C-n") 'envdir-unset)
    map)
  "Keymap for `envdir-mode'.")

;;;###autoload
(define-minor-mode envdir-mode
  "Minor mode for envdir interaction.
\\{envdir-mode-map}"
  :global t
  :lighter ""
  :keymap envdir-mode-map
  (if envdir-mode
      (add-to-list 'mode-line-misc-info envdir-mode-line-format)
    (setq mode-line-misc-info
          (delete envdir-mode-line-format mode-line-misc-info))))

(defun envdir-read-directory (dirname)
  "Read environment variables from DIRNAME."
  (--map (cons (f-filename it) (s-trim-right (f-read-text it)))
         (f-files dirname)))

(provide 'envdir)

;;; envdir.el ends here
