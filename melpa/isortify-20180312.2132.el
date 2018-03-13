;;; isortify.el --- (automatically) format python buffers using isort.

;; Copyright (C) 2016-2018 Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; Homepage: https://github.com/proofit404/isortify
;; Version: 0.0.1
;; Package-Version: 20180312.2132
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

(defvar isortify-multi-line-output nil)

(defvar isortify-trailing-comma nil)

(defvar isortify-known-first-party nil)

(defun isortify-call-bin (input-buffer output-buffer)
  "Call process isort on INPUT-BUFFER saving the output to OUTPUT-BUFFER.

Return isort process the exit code."
  (with-current-buffer input-buffer
    (let (args)
      (when isortify-multi-line-output
        (push "--multi-line" args)
        (push (number-to-string isortify-multi-line-output) args))
      (when isortify-trailing-comma
        (push "--trailing-comma" args))
      (when isortify-known-first-party
        (dolist (project isortify-known-first-party)
          (push "--project" args)
          (push project args)))
      (push "-" args)
      (let ((process (apply 'start-file-process "isortify" output-buffer "isort" (reverse args))))
        (set-process-sentinel process (lambda (process event)))
        (process-send-region process (point-min) (point-max))
        (process-send-eof process)
        (accept-process-output process nil nil t)
        (while (process-live-p process)
          (accept-process-output process nil nil t))
        (process-exit-status process)))))

;;;###autoload
(defun isortify-buffer (&optional display)
  "Try to isortify the current buffer.

Show isort output, if isort exit abnormally and DISPLAY is t."
  (interactive (list t))
  (let* ((original-buffer (current-buffer))
         (original-point (point))
         (original-window-pos (window-start))
         (tmpbuf (get-buffer-create "*isortify*")))
    ;; This buffer can be left after previous isort invocation.  It
    ;; can contain error message of the previous run.
    (with-current-buffer tmpbuf
      (erase-buffer))
    (condition-case err
        (if (not (zerop (isortify-call-bin original-buffer tmpbuf)))
            (error "Isort failed, see %s buffer for details" (buffer-name tmpbuf))
          (with-current-buffer tmpbuf
            (copy-to-buffer original-buffer (point-min) (point-max)))
          (kill-buffer tmpbuf)
          (goto-char original-point)
          (set-window-start (selected-window) original-window-pos))
      (error (message "%s" (error-message-string err))
             (when display
               (pop-to-buffer tmpbuf))))))

;;;###autoload
(define-minor-mode isort-mode
  "Automatically run isort before saving."
  :lighter " Isort"
  (if isort-mode
      (add-hook 'before-save-hook 'isortify-buffer nil t)
    (remove-hook 'before-save-hook 'isortify-buffer t)))

(provide 'isortify)

;;; isortify.el ends here
