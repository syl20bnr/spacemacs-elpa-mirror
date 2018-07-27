;;; postcss-sorting.el --- postcss-sorting interface

;; Copyright (C) 2017 Peiwen Lu

;; Author: Peiwen Lu <hi@peiwen.lu>
;; Version: 0.0.2
;; Package-Version: 20180211.956
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/P233/postcss-sorting.el

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

;; Provides an interactive command `postcss-sorting-buffer' to sort CSS buffer
;; using postcss with postcss-sorting plugin.  Support postcss-cli v5 and
;; postcss-sorting v3.

;; Heavily inspired by the stylefmt.el package:
;; https://github.com/KeenS/stylefmt.el

;;; Usage:

;; Install the postcss-cli tool and postcss-sorting plugin:
;; Run 'npm install -g postcss-cli postcss-sorting'.

;; Indicate your postcss config file's path:
;; (setq postcss-sorting-config-file "path/to/config/file")

;; Call `postcss-sorting-buffer' to sort the current buffer.

;; If you want to automatically sort before saving a file,
;; add the following hook to your Emacs configuration:

;; (eval-after-load 'css-mode
;;   '(add-hook 'css-mode-hook
;;              (lambda ()
;;                (add-hook 'before-save-hook 'postcss-sorting-buffer t t))))

;;; Code:

(defvar postcss-sorting-program "postcss"
  "The executable to use for postcss-sorting plugin.")

(defvar postcss-sorting-config-file "~/.postcssrc.json"
  "PostCSS config file path.")

(defvar postcss-sorting-popup-errors t
  "Display error buffer when postcss-sorting fails.")


(defun postcss-sorting-get-shell-command ()
  "Construct postcss shell command."
  (mapconcat 'identity (list postcss-sorting-program
                             (concat "-c " (shell-quote-argument (expand-file-name postcss-sorting-config-file)))) " "))


(defun postcss-sorting--call (cur-buffer output-buffer-name)
  "Execute the postcss command.  Process CUR-BUFFER in the OUTPUT-BUFFER-NAME buffer."
  (with-current-buffer (get-buffer-create output-buffer-name)
    (erase-buffer)
    (insert-buffer-substring cur-buffer)
    (if (zerop (shell-command-on-region (point-min) (point-max) (postcss-sorting-get-shell-command) cur-buffer t output-buffer-name))
        (progn (copy-to-buffer cur-buffer (point-min) (point-max))
               (kill-buffer))
      (when postcss-sorting-popup-errors
        (display-buffer output-buffer-name))
      (error "Postcss-sorting failed, see %s buffer for details" output-buffer-name))))


;;;###autoload
(defun postcss-sorting-buffer ()
  "Sort the current buffer using postcss with postcss-sorting plugin."
  (interactive)
  (unless (executable-find postcss-sorting-program)
    (error "Could not locate executable \"%s\"" postcss-sorting-program))

  (let ((cur-point (point))
        (cur-win-start (window-start))
        (output-buffer-name "*postcss-sorting*"))
    (postcss-sorting--call (current-buffer) output-buffer-name)
    (goto-char cur-point)
    (set-window-start (selected-window) cur-win-start))
  (message "Sorted buffer with postcss-sorting."))



(provide 'postcss-sorting)
;;; postcss-sorting.el ends here
