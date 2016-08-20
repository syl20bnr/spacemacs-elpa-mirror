;;; flycheck-stack.el --- Flychecker using stack ghci

;; Copyright (c) 2016 Chris Done. All rights reserved.

;; Package-Requires: ((flycheck "26") (haskell-mode "13"))
;; Package-Version: 20160520.244

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Just a flycheck mode that enables the use of stack ghci to report
;; error messages.

;;; Code:

(require 'flycheck)
(require 'haskell-interactive-mode)

(defvar flycheck-stack-callbacks (list)
  "List of callbacks waiting for output. FIFO.")
(make-variable-buffer-local 'flycheck-stack-callbacks)

(defvar flycheck-stack-arguments (list)
  "Arguments used to call the stack process.")
(make-variable-buffer-local 'flycheck-stack-arguments)

(defvar flycheck-stack-project-root nil
  "The project root of the current buffer.")
(make-variable-buffer-local 'flycheck-stack-project-root)

(defun flycheck-stack-targets ()
  "Set the targets to use for stack ghci."
  (interactive)
  (let ((targets (split-string (read-from-minibuffer "Targets: ")
                               " "
                               t)))
    (flycheck-stack-destroy)
    (flycheck-stack-get-worker-create targets (current-buffer))))

(defun flycheck-stack-buffer ()
  "Get the worker buffer for the current directory."
  (let ((buffer (flycheck-stack-get-buffer-create)))
    (if (get-buffer-process buffer)
        buffer
      (flycheck-stack-get-worker-create))))

(defun flycheck-stack-process ()
  "Get the worker process for the current directory."
  (get-buffer-process (flycheck-stack-buffer)))

(defun flycheck-stack-destroy ()
  "Stop the current worker process and kill its associated buffer."
  (interactive)
  (with-current-buffer (flycheck-stack-get-buffer-create)
    (when (get-buffer-process (current-buffer))
      (kill-process (get-buffer-process (current-buffer)))
      (delete-process (get-buffer-process (current-buffer))))
    (kill-buffer (current-buffer))))

(defun flycheck-stack-reload ()
  "Restart the worker process to reflect the latest project configuration."
  (interactive)
  (flycheck-stack-destroy)
  (flycheck-buffer))

(defun flycheck-stack-get-worker-create (&optional targets source-buffer)
  "Start a GHCi worker."
  (let* ((buffer (flycheck-stack-get-buffer-create)))
    (if (get-buffer-process buffer)
        buffer
      (let* ((options (list "--no-load"
                            "--no-build"))
             (main-is (list))
             (arguments (append options
                                targets
                                main-is))
             (process (with-current-buffer buffer
                        (message "Booting up stack ghci ...")
                        (apply #'start-process "stack" buffer "stack" "ghci"
                               arguments))))
        (process-send-string process ":set -fobject-code\n")
        (process-send-string process ":set prompt \"\\4\"\n")
        (with-current-buffer buffer
          (setq flycheck-stack-arguments arguments)
          (setq flycheck-stack-callbacks
                (list (cons source-buffer
                            (lambda (source-buffer _msg)
                              (when source-buffer
                                (with-current-buffer source-buffer
                                  (when flycheck-mode
                                    (run-with-timer 0 nil
                                                    'flycheck-stack-call-in-buffer
                                                    (current-buffer)
                                                    'flycheck-buffer))))
                              (message "Booted up stack ghci!"))))))
        (set-process-filter process
                            (lambda (process string)
                              (when (buffer-live-p (process-buffer process))
                                (with-current-buffer (process-buffer process)
                                  (goto-char (point-max))
                                  (insert string)
                                  (flycheck-stack-read-buffer)))))
        (set-process-sentinel process
                              (lambda (process change)
                                (when (buffer-live-p (process-buffer process))
                                  (when (not (process-live-p process))
                                    (switch-to-buffer (process-buffer process))
                                    (goto-char (point-max))
                                    (insert "\n---\n
This is the buffer where Emacs talks to stack ghci. It's normally hidden,
but a problem occcured.\n")
                                    (insert "\nThe process ended. Here is the reason:\n"
                                            "  " change
                                            "\n")
                                    (insert "For troubleshooting purposes, here are the arguments used to launch stack ghci:\n"
                                            (format "  stack ghci %s"
                                                    (mapconcat #'identity
                                                               flycheck-stack-arguments
                                                               " "))
                                            "\n\n")
                                    (insert "You can kill this buffer when you're ready.\n")))))
        buffer))))

(defun flycheck-stack-read-buffer ()
  "In the process buffer, we read what's in it."
  (goto-char (point-min))
  (when (search-forward "\4" (point-max) t 1)
    (let* ((next-callback (car (last flycheck-stack-callbacks)))
           (state (car next-callback))
           (func (cdr next-callback)))
      (setq flycheck-stack-callbacks (butlast flycheck-stack-callbacks))
      (let ((string (buffer-substring (point-min) (1- (point)))))
        (if next-callback
            (funcall func state string)
          (when debug-on-error
            (warn "Received output but no callback in `flycheck-stack-callbacks': %S"
                  string)))))
    (delete-region (point-min) (point))))

(defun flycheck-stack-get-buffer-create ()
  "Get or create the stack buffer for this current directory and
the given targets."
  (let* ((root (flycheck-stack-project-root))
         (default-directory root))
    (with-current-buffer
        (get-buffer-create (concat " "
                                   (file-name-nondirectory root)
                                   " "
                                   root))
      (cd root)
      (current-buffer))))

(defun flycheck-stack-project-root ()
  "Get the directory where the stack.yaml is placed for this
project, or the global one."
  (if flycheck-stack-project-root
      flycheck-stack-project-root
    (setq flycheck-stack-project-root
          (with-temp-buffer
            (save-excursion
              (call-process "stack" nil
                            (current-buffer)
                            nil
                            "path"
                            "--project-root"
                            "--verbosity" "silent"))
            (buffer-substring (line-beginning-position) (line-end-position))))))

(defun flycheck-stack-check (checker cont)
  "Run a check and pass the status onto CONT."
  (let ((file-buffer (current-buffer)))
    (write-region (point-min) (point-max) (buffer-file-name))
    (clear-visited-file-modtime)
    (with-current-buffer (flycheck-stack-buffer)
      (setq flycheck-stack-callbacks
            (append flycheck-stack-callbacks
                    (list (cons (list :cont cont
                                      :file-buffer file-buffer
                                      :checker checker)
                                (lambda (state string)
                                  (with-current-buffer (plist-get state :file-buffer)
                                    (funcall (plist-get state :cont)
                                             'finished
                                             (flycheck-stack-parse-errors-warnings
                                              (plist-get state :checker)
                                              (current-buffer)
                                              string))))))))))
  (process-send-string (flycheck-stack-process)
                       (concat ":l " (buffer-file-name) "\n")))

(defun flycheck-stack-parse-errors-warnings (checker buffer string)
  "Parse from the given STRING a list of flycheck errors and
warnings, adding CHECKER and BUFFER to each one."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((messages (list)))
      (while (search-forward-regexp
              (concat "[\r\n]\\([A-Z]?:?[^ \r\n:][^:\n\r]+\\):\\([0-9()-:]+\\):"
                      "[ \n\r]+\\([[:unibyte:][:nonascii:]]+?\\)\n[^ ]")
              nil t 1)
        (let* ((file (match-string 1))
               (location-raw (match-string 2))
               (msg (match-string 3))
               (type (cond ((string-match "^Warning:" msg)
                            (setq msg (replace-regexp-in-string "^Warning: *" "" msg))
                            'warning)
                           ((string-match "^Splicing " msg) 'splice)
                           (t                               'error)))
               (location (haskell-process-parse-error
                          (concat file ":" location-raw ": x")))
               (line (plist-get location :line))
               (column (plist-get location :col)))
          (when (string= (buffer-file-name buffer)
                         file)
            (setq messages
                  (cons (flycheck-error-new-at
                         line column type msg
                         :checker checker
                         :buffer buffer)
                        messages)))))
      messages)))

(defun flycheck-stack-call-in-buffer (buffer func &rest args)
  "Utility function which calls FUNC in BUFFER with ARGS."
  (with-current-buffer buffer
    (apply func args)))

(flycheck-define-generic-checker 'stack
  "A syntax and type checker for Haskell using a stack ghci
worker process."
  :start 'flycheck-stack-check
  :modes '(haskell-mode)
  :next-checkers '((warning . haskell-hlint)))

(add-to-list 'flycheck-checkers 'stack)

(provide 'flycheck-stack)

;;; flycheck-stack.el ends here
