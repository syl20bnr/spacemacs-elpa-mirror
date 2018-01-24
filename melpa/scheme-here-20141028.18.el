;;; scheme-here.el --- cmuscheme extension for multiple inferior processes
;;
;; Copyright (C) 2007, 2008 Dimitris Vyzovitis
;; Copyright (C) 2013, 2014 Wei Zhao
;;
;; Author: Dimitris Vyzovitis <vyzo@media.mit.edu>
;; Maintainer: Wei Zhao <kaihaosw@gmail.com>
;; Credit: https://github.com/vyzo
;; URL: https://github.com/kaihaosw/scheme-here
;; Package-Version: 20141028.18
;; Version: 0.8
;; Keywords: scheme
;; EmacsWiki: RunSchemeHere
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;;; Commentary:
;; This is an extension of cmuscheme for multiple buffer-local
;; inferior scheme processes.
;;
;;; Change Log:
;; Oct 28 2014: add "cmd" arg for run-scheme-here
;; Aug 03 2014: add scheme-here-trace-procedure scheme-here-expand-current-form scheme-here-eval-buffer
;; Oct 04 2013: add scheme-here-load-file
;; Dec 08 2008: updated headers, provide 'scheme-here
;; Mar 12 2007: initial version
;;
;;; Code:

(require 'comint)
(require 'scheme)
(require 'cmuscheme)

(defun run-scheme-here (cmd)
  "Run a new scheme process at the directory of the current buffer.
   If a process is already running, switch to its buffer."
  (interactive (list (if current-prefix-arg
			 (read-string "Run Scheme: " scheme-program-name)
                       scheme-program-name)))
  (let* ((proc (format "scheme: %s" default-directory))
         (buf (format "*%s*" proc)))
    (unless (comint-check-proc buf)
      (let ((cmdlist (split-string cmd)))
        (set-buffer
         (apply 'make-comint-in-buffer proc buf (car cmdlist) nil (cdr cmdlist)))
        (inferior-scheme-mode)))
    (pop-to-buffer buf)))

(defun scheme-here-buffer ()
  (format "*scheme: %s*" default-directory))

(defun scheme-here-proc ()
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-scheme-mode)
                                      (current-buffer)
                                    (scheme-here-buffer)))))
    (or proc (error "No scheme process here"))))

(defun switch-to-scheme-here ()
  "Switch to the buffer-local scheme process."
  (interactive)
  (let ((buf (scheme-here-buffer)))
    (if (get-buffer buf)
        (pop-to-buffer buf)
      (error "No scheme process here"))
    (push-mark)))

(defun scheme-here-send-region (start end)
  "Send the current region to the buffer-local scheme process."
  (interactive "r")
  (let ((proc (scheme-here-proc)))
    (comint-send-region proc start end)
    (comint-send-string proc "\n")))

(defun scheme-here-send-sexp ()
  "Send the last sexp to the buffer-local scheme process."
  (interactive)
  (scheme-here-send-region (save-excursion (backward-sexp) (point)) (point)))

(defun scheme-here-send-def ()
  "Send the current definition to the buffer-local scheme process."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (scheme-here-send-region (point) end))))

(defun scheme-here-eval-buffer ()
  "Send the current buffer to the buffer-local scheme process."
  (interactive)
  (scheme-here-send-region (point-min) (point-max)))

(defun scheme-here-send-region/switch (start end)
  "Send the current region to the buffer-local scheme process.
   Then switch to its buffer."
  (interactive "r")
  (scheme-here-send-region start end)
  (switch-to-scheme-here))

(defun scheme-here-send-sexp/switch ()
  "Send the last sexp to the buffer-local scheme process.
   Then switch to its buffer."
  (interactive)
  (scheme-here-send-sexp)
  (switch-to-scheme-here))

(defun scheme-here-send-def/switch ()
  "Send the current definition to the buffer-local scheme process.
   Then switch to its buffer."
  (interactive)
  (scheme-here-send-def)
  (switch-to-scheme-here))

(defun scheme-here-load-file (file-name)
  "Load a Scheme file into the buffer-local scheme process."
  (interactive (comint-get-source "Load Scheme file: " scheme-prev-l/c-dir/file
                                  scheme-source-modes t))
  (comint-check-source file-name)
  (setq scheme-prev-l/c-dir/file (cons (file-name-directory file-name)
                                       (file-name-nondirectory file-name)))
  (comint-send-string (scheme-here-proc) (concat "(load \""
                                                 file-name
                                                 "\"\)\n"))
  (switch-to-scheme-here))

(defun scheme-here-trace-procedure (proc &optional untrace)
  "Trace procedure PROC in the buffer-local scheme process.
With a prefix argument switch off tracing of procedure PROC."
  (interactive
   (list (let ((current (symbol-at-point))
               (action (if current-prefix-arg "Untrace" "Trace")))
           (if current
               (read-string (format "%s procedure [%s]: " action current) nil nil (symbol-name current))
             (read-string (format "%s procedure: " action))))
         current-prefix-arg))
  (when (= (length proc) 0)
    (error "Invalid procedure name"))
  (comint-send-string (scheme-here-proc)
                      (format
                       (if untrace scheme-untrace-command scheme-trace-command)
                       proc))
  (comint-send-string (scheme-here-proc) "\n"))

(defun scheme-here-expand-current-form ()
  "Macro-expand the form at point in the buffer-local scheme process."
  (interactive)
  (let ((current-form (scheme-form-at-point)))
    (if current-form
        (progn
          (comint-send-string (scheme-here-proc)
                              (format
                               scheme-macro-expand-command
                               current-form))
          (comint-send-string (scheme-here-proc) "\n"))
      (error "Not at a form"))))

(defun scheme-here-hook ()
  (define-key scheme-mode-map "\C-x\M-se" 'scheme-here-send-sexp)
  (define-key scheme-mode-map "\C-x\M-sr" 'scheme-here-send-region)
  (define-key scheme-mode-map "\C-x\M-sd" 'scheme-here-send-def)
  (define-key scheme-mode-map "\C-x\M-sb" 'scheme-here-eval-buffer)
  (define-key scheme-mode-map "\C-x\M-st" 'scheme-here-trace-procedure)
  (define-key scheme-mode-map "\C-x\M-sx" 'scheme-here-expand-current-form)
  (define-key scheme-mode-map "\C-x\M-s\M-e" 'scheme-here-send-sexp/switch)
  (define-key scheme-mode-map "\C-x\M-s\M-r" 'scheme-here-send-region/switch)
  (define-key scheme-mode-map "\C-x\M-s\M-d" 'scheme-here-send-def/switch)
  (define-key scheme-mode-map "\C-x\M-sl" 'scheme-here-load-file))

(add-hook 'scheme-mode-hook 'scheme-here-hook)

(provide 'scheme-here)

;;; scheme-here.el ends here
