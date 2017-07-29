;;; isortify.el --- (automatically) format python buffers using isort.

;; Copyright (C) 2016 Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; Homepage: https://github.com/proofit404/isortify
;; Version: 0.0.1
;; Package-Version: 20170726.1254
;; Package-Requires: ()

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Isortify uses isort to format a Python buffer.  It can be called
;; explicitly on a certain buffer, but more conveniently, a minor-mode
;; 'isort-mode' is provided that turns on automatically running isort
;; on a buffer before saving.
;;
;; Installation:
;;
;; Add isortify.el to your load-path.
;;
;; To automatically format all Python buffers before saving, add the function
;; isort-mode to python-mode-hook:
;;
;; (add-hook 'python-mode-hook 'isort-mode)
;;
;;; Code:

(defun isortify-call-bin (input-buffer output-buffer)
  "Call process isort on INPUT-BUFFER saving the output to OUTPUT-BUFFER.
Return the exit code."
  (with-current-buffer input-buffer
    (call-process-region (point-min) (point-max) "isort" nil output-buffer nil "--multi-line" "3" "--trailing-comma" "-")))

;;;###autoload
(defun isortify-buffer ()
  "Try to isortify the current buffer.
If isort exits with an error, the output will be shown in a help-window."
  (interactive)
  (let* ((original-buffer (current-buffer))
         (original-point (point))
         (tmpbuf (generate-new-buffer "*isortify*"))
         (exit-code (isortify-call-bin original-buffer tmpbuf)))
    (unwind-protect
        (if (not (zerop exit-code))
            (error "Isort failed, see %s buffer for details" (buffer-name tmpbuf))
          (with-current-buffer tmpbuf
            (copy-to-buffer original-buffer (point-min) (point-max)))
          (kill-buffer tmpbuf)
          (goto-char original-point)))))

;;;###autoload
(define-minor-mode isort-mode
  "Automatically run isort before saving."
  :lighter " Isort"
  (if isort-mode
      (add-hook 'before-save-hook 'isortify-buffer nil t)
    (remove-hook 'before-save-hook 'isortify-buffer t)))

(provide 'isortify)

;;; isortify.el ends here
