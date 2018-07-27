;;; gore-mode.el --- Simple mode for gore, a command-line evaluator for golang.

;; Copyright (C) 2013  Sergey Pashaev

;; Author: Sergey Pashaev <sergey.pashaev@gmail.com>
;; Maintainer: Sergey Pashaev <sergey.pashaev@gmail.com>
;; Created: 27th October 2013
;; Keywords: go, repl
;; Package-Version: 20151123.1927
;; Package-Requires: ((go-mode "1.0.0"))

;; This file is NOT part of GNU Emacs.

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

;;; Source code
;;
;; Source code can be found here:
;;   http://github.com/sergey-pashaev/gore-mode

;;; Style note
;;
;; This codes uses the Emacs style of:
;;
;;    gore--private-function
;;
;; for private functions.

;;; Commentary:
;;
;; This is a simple comint-based mode for gore (command-line evaluator
;; for golang code) which you can install from here:
;;   http://github.com/sriram-srinivasan/gore

;;; Usage:
;;
;; First of all install go-mode from MELPA and gore from here:
;;  http://github.com/sriram-srinivasan/gore
;;
;; Put gore-mode.el in your load path:
;; (add-to-list 'load-path "/<path to gore-mode dir here>/")
;;
;; Add (require 'gore-mode) to your .emacs
;;
;; Set correct gore binary path:
;; (setq gore-bin-path "/your/path/to/gore/binary")
;;
;; M-x run-gore to enable gore-mode in *gore* buffer
;; M-x gore-mode to enable gore-mode in current-buffer

(require 'comint)
(require 'go-mode)

(defconst gore-output-prompt "GORE> ")
(defconst gore-input-prompt  ">>>>> ")
(defconst gore-buffer-name "*gore*")

(defvar gore-expr-mode nil
  "Expr mode variable. If true - all user input will be wrapped like \"println(\" + %user_input% + \")\"")
(defvar gore-buffer nil)

;; TODO: check environment variables for GOPATH
(defvar gore-bin-path ""
  "gore binary path. Should be setted by user.")

(defun gore--eval (string)
  "Evaluates string through gore and returns all stdout+stderr"
  (shell-command-to-string (concat gore-bin-path " '" string "'")))

(defun gore--process-input (string)
  "User input processing. Checking for `gore-expr-mode', wrapping user input"
  ;; TODO: escape ' and "
  (if gore-expr-mode
      (progn
        (setq gore-expr-mode nil)
        (concat "println(" string ")"))
    string))

(defun gore--process-output (output)
  "gore output processing. Colorize if errors, drop line of minuses from gore output, etc."
  (concat (propertize gore-output-prompt 'font-lock-face '(:foreground "blue"))
          (if (string-match "== Error ========" output)
              (propertize (substring output 0 -1)  'font-lock-face '(:foreground "red"))
            ;; drop first line of minuses
            (substring output 34))))

(defun gore--input-sender (proc input)
  (progn
    (comint-output-filter proc (gore--process-output (gore--eval (gore--process-input input))))
    (comint-output-filter proc gore-input-prompt)))

(defun gore--set-local-keys ()
  (local-set-key (kbd "C-<return>") 'gore-send-expr))

(defun gore-send-expr ()
  (interactive)
  (setq gore-expr-mode t)
  (comint-send-input))

;;;###autoload
(defun run-gore ()
  "Run a go repl in `*gore*' buffer."
  (interactive)
  (setq gore-buffer (get-buffer-create gore-buffer-name))
  (switch-to-buffer gore-buffer)
  (set-buffer gore-buffer)
  (gore-mode))

;;;###autoload
(define-derived-mode gore-mode comint-mode "GORE"
  "Run a go repl in current buffer."
  :syntax-table go-mode-syntax-table
  (setq comint-input-sender 'gore--input-sender)
  (setq comint-prompt-read-only t)
  (setq gore-buffer (get-buffer-create gore-buffer-name)) 
  (unless (comint-check-proc (current-buffer))
    (let ((fake-proc (start-process "gore"
                                    (current-buffer)
                                    "cat")))
      (set-process-query-on-exit-flag fake-proc nil)
      (insert "** Go Repl started **\n")
      (set-marker
       (process-mark fake-proc) (point))
      (comint-output-filter fake-proc gore-input-prompt))))

(add-hook 'gore-mode-hook 'gore--set-local-keys)

(provide 'gore-mode)

;;; gore-mode.el ends here
