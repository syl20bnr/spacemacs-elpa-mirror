;;; mbsync.el --- run mbsync to fetch mails

;; Copyright (C) 2012-2017 Dimitri Fontaine

;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; Version: 0.1.1
;; Package-Version: 20171117.1343
;; URL: https://github.com/dimitri/mbsync-el

;; This file is NOT part of GNU Emacs.

;; mbsync-el is free software, see the file LICENSE.

;;; Commentary:
;;
;; Run mbsync to fetch mails

;;; News:

;;;; Changes since 0.0.1:
;; - Ensure only one process runs at a time.
;;   If you wish to run several at a time (e.g. with different configurations),
;;   let-bind `mbsync-buffer-name' around invocations to keep them unique.

;;; Code:

(defgroup mbsync nil "mbsync customization group"
  :group 'convenience)

(defcustom mbsync-exit-hook nil
  "Hook run after `mbsync' is done."
  :group 'mbsync
  :type 'hook)

(defcustom mbsync-executable (executable-find "mbsync")
  "Where to find the `mbsync' utility."
  :group 'mbsync
  :type 'string)

(defcustom mbsync-args '("-a")
  "List of options to pass to the `mbsync' command."
  :group 'mbsync
  :type '(repeat string))

(defcustom mbsync-auto-accept-certs nil
  "Accept all certificates if true."
  :group 'mbsync
  :type 'boolean)

(defcustom mbsync-verbose t
  "Print messages on start and finish."
  :group 'mbsync
  :type 'boolean)

(defface mbsync-font-lock-error-face
  '((t (:foreground "yellow" :background "red" :bold t)))
  "Face description for all errors."
  :group 'mbsync)

(defvar mbsync-process-filter-pos nil)

(defvar mbsync-buffer-name "*mbsync*")

(defun mbsync-info (&rest args)
  "Show user the message ARGS if we're being `mbsync-verbose'."
  (let ((inhibit-message (not mbsync-verbose)))
    (apply #'message args)))

(defun mbsync-process-filter (proc string)
  "Filter for `mbsync', auto accepting certificates.
Arguments PROC, STRING as in `set-process-filter'."
  (with-current-buffer (process-buffer proc)
    (unless (bound-and-true-p mbsync-process-filter-pos)
      (make-local-variable 'mbsync-process-filter-pos)
      (setq mbsync-process-filter-pos (point-min)))

    (save-excursion
      (let ((inhibit-read-only t))
	(goto-char (point-max))
	(insert string)

	;; accept certificates
	(goto-char mbsync-process-filter-pos)
	(while (re-search-forward "Accept certificate?" nil t)
          (if mbsync-auto-accept-certs
              (process-send-string proc "y\n")
            (message "mbsync blocked, waiting for certificate acceptance")))))

    (save-excursion
	;; message progress
	(goto-char mbsync-process-filter-pos)
	(while (re-search-forward (rx bol "Channel " (+ (any alnum)) eol) nil t)
	  (mbsync-info "%s" (match-string 0))))

    (let (err-pos)
      (save-excursion
        ;; errors
        (goto-char mbsync-process-filter-pos)
        (while (re-search-forward (rx (or
                                       (and bol "Maildir error:" (* anything) eol)
                                       (and bol "Error:" (* anything) eol)
                                       (and bol "gpg: decryption failed: " (* anything) eol)
                                       (and bol "Skipping account " (* anything) eol) ))
                                  nil t)
          (message "%s" (match-string 0))
          (overlay-put (make-overlay (match-beginning 0)
                                     (match-end 0))
                       'face 'mbsync-font-lock-error-face)
          (switch-to-buffer-other-window (current-buffer))
          (setq err-pos (match-beginning 0))))
      (when err-pos
        (goto-char err-pos)))

    (setq mbsync-process-filter-pos (point-max))))

(defun mbsync-sentinel (proc change)
  "Mail sync is over, message it then run `mbsync-exit-hook'.
Arguments PROC, CHANGE as in `set-process-sentinel'."
  (when (eq (process-status proc) 'exit)
    (mbsync-info "mbsync is done")
    (run-hooks 'mbsync-exit-hook)))

(defun mbsync-get-proc ()
  "Get the running mbsync process (or nil if no such)."
  (let ((b (get-buffer "*mbsync*")))
    (and (buffer-live-p b)
         (get-buffer-process b))))

;;;###autoload
(defun mbsync (&optional show-buffer)
  "Run the `mbsync' command, asynchronously, then run `mbsync-exit-hook'.
If SHOW-BUFFER, also show the *mbsync* output."
  (interactive "P")
  (if (mbsync-get-proc)
      (message "Please wait, mbsync is already fetching, see buffer *mbsync* for details.")
    (let* ((dummy (when (get-buffer mbsync-buffer-name)
                    (kill-buffer mbsync-buffer-name)))
           (proc (apply 'start-process
                        mbsync-buffer-name
                        mbsync-buffer-name
                        mbsync-executable
                        mbsync-args)))
      (set-process-filter proc 'mbsync-process-filter)
      (set-process-sentinel proc 'mbsync-sentinel)))
  (when show-buffer
    (set-window-buffer (selected-window)
                       (process-buffer (mbsync-get-proc)))))

(provide 'mbsync)

;;; mbsync.el ends here
