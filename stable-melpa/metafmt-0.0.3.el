;;; metafmt.el --- Run metafmt on buffers when saving them

;; Copyright (C) 2015 Lorenzo Villani
;;
;; Author: Lorenzo Villani <lorenzo@villani.me>
;; Maintainer: Lorenzo Villani <lorenzo@villani.me>
;; Created: 27 Dec 2015
;;
;; Keywords: languages, tools
;; Package-Version: 0.0.3
;; Homepage: https://github.com/lvillani/metafmt

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3 of the License.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;###autoload
(define-minor-mode metafmt-mode
  "Enable format-on-save for all supported files through metafmt"
  :lighter " fmt"
  (if metafmt-mode
      (add-hook 'before-save-hook 'metafmt-before-save t t)
    (remove-hook 'before-save-hook 'metafmt-before-save t)))

;;;###autoload
(define-globalized-minor-mode global-metafmt-mode metafmt-mode (lambda () (metafmt-mode t)))

(defun metafmt-before-save ()
  (let ((command `("metafmt" "-editor" "emacs" "-syntax" ,(symbol-name major-mode) "-"))
        (old-point (point))
        (old-window-start (window-start))
        (tmp-buffer (get-buffer-create " *metafmt*")))
    (unless (null command)
      (message "Formatting buffer...")
      (unwind-protect
        (when (zerop (metafmt-run-command tmp-buffer command))
          (erase-buffer)
          (insert-buffer-substring tmp-buffer)
          (goto-char old-point)
          (set-window-start (get-buffer-window) old-window-start t))
        (kill-buffer tmp-buffer)))))

(defun metafmt-run-command (buf command)
  (apply #'call-process-region (point-min) (point-max) (car command) nil buf nil (cdr command)))

(provide 'metafmt)
;;; metafmt.el ends here
