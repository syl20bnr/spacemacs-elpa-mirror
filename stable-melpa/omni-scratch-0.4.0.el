;;; omni-scratch.el --- Easy and mode-specific draft buffers

;; Copyright (C) 2014-2017  Adrien Becchis

;; Author: Adrien Becchis <adriean.khisbe@live.fr>
;; Created:  2014-07-27
;; Version: 0.1.1
;; Package-Version: 0.4.0
;; Keywords: convenience, languages, tools
;; Url: https://github.com/AdrieanKhisbe/omni-scratch.el

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

;; Easily create scratch buffer to edit in the curreny major mode you are using.
;; Some features like custom scratch buffer with specific minor-modes,
;; copy on quit, etc, might be added over time.

;;; Building-Notes:

;; §todo: switch to THE buffer associated with current programming mode.
;; §maybe: integration with popwin.

;;; Code:

(defcustom omni-scratch-default-mode 'fundamental-mode
  "Default omni-scratch mode for the scratch buffer."
  :type 'symbol :group 'omni-scratch)

(defvar omni-scratch-latest-scratch-buffer (get-buffer "*scratch*")
  "The Latest scratch buffer used.")

(defvar omni-scratch-origin-buffer nil
  "The last normal buffer from which command was invoked")

(defvar omni-scratch-buffers-list '()
  "List of scratch buffers.")

(defun omni-scratch-create-scratch-buffer (name mode)
  "Create or switch to NAME buffer in specified MODE."
  ;; §later: option noselect?
  ;; §maybe: create or also switch to?
  ;; §TODO: rename?, kill interactive?
  (let ((buffer (get-buffer-create name) ))
    (switch-to-buffer buffer)
    (setq omni-scratch-latest-scratch-buffer buffer)
    (funcall mode)
    (omni-scratch-mode)
    ;; §later: apply eventual modification to local modes.
    ;; [and var: maybe identify the scratch buffer]: local var and register in alist or so
    buffer))

(defun omni-scratch-goto-latest ()
  "Switch to the `omni-scratch-latest-scratch-buffer' used."
  (interactive)
  (setq omni-scratch-origin-buffer (current-buffer))
  ;; §note: improve using ring. (so that handle dead buffer)
  (switch-to-buffer omni-scratch-latest-scratch-buffer))
(defalias 'omni-scratch-goto-last 'omni-scratch-goto-latest)

;; §todo: default mode
;; §maybe: specific background

;;;###autoload
(defun omni-scratch-buffer ()
  "Create a new scratch buffer and switch to. Unless if in scratch buffer already"
  (interactive)
  (if (bound-and-true-p omni-scratch-mode)
      (progn (switch-to-buffer omni-scratch-origin-buffer)
             (setq omni-scratch-origin-buffer nil))
      (progn (setq omni-scratch-origin-buffer (current-buffer))
        (switch-to-buffer
         (omni-scratch-create-scratch-buffer "*scratch:draft*" omni-scratch-default-mode)))))

;; ¤note: for now just one scratch buffer.
;; §todo: later many different?
;;;###autoload
(defun omni-scratch-major-buffer ()
  "Create a new scratch buffer and switch to with current major mode."
  (interactive)
  (if (bound-and-true-p omni-scratch-mode)
      (progn (switch-to-buffer omni-scratch-origin-buffer)
             (setq omni-scratch-origin-buffer nil))
    (progn (setq omni-scratch-origin-buffer (current-buffer))
           (let ((buffer-name
                  (replace-regexp-in-string "\\(.*\\)-mode" "*scratch:\\1*"
                                            (symbol-name major-mode))))
             (add-to-list 'omni-scratch-buffers-list buffer-name)
             (switch-to-buffer
              (omni-scratch-create-scratch-buffer
               buffer-name major-mode))))))

;; §later: scratch minor modefor this buffer: quick exist, copy content. save to file.
;; §later: filter mode where not applyable: ibuffer and others..

(defun omni-scratch-quit ()
  "Quit the current omni-buffer."
  ;; §Todo: protection to not being call in a non omni-scratch buffer
  (interactive)
  (kill-ring-save (buffer-end -1) (buffer-end 1))
  (setq omni-scratch-buffers-list
        (remove (buffer-name) omni-scratch-buffers-list))
  (kill-buffer))

(defun omni-scratch-buffers ()
  "Helm select the scratch buffer."
  (interactive)
  (let ((buffer-name
         (helm :sources (list
                         (helm-build-sync-source "default"
                           :candidates '("*scratch:draft*" "*scratch*"))
                         (helm-build-sync-source "major mode"
                           :candidates omni-scratch-buffers-list))
               :buffer "*omni-scratch-buffers*")))
    (switch-to-buffer (get-buffer buffer-name))))

(define-minor-mode omni-scratch-mode
  "Scratch buffer mode."
  :lighter " β")


(provide 'omni-scratch)
;;; omni-scratch.el ends here
