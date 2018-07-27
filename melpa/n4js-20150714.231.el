;;; n4js.el --- Neo4j Shell

;;; Copyright (C) 2015 TruongTx

;;; Author: TruongTx <me@truongtx.me>
;;; Version: 0.1
;; Package-Version: 20150714.231
;;; URL: https://github.com/tmtxt/n4js.el
;;; Package-Requires: ((emacs "24") (cypher-mode "0"))
;;; Keywords: neo4j, shell, comint

;;; License:

;; This program is free software: you can redistribute it and/or modify
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

;; Put this file in your Emacs lisp path (e.g. ~/.emacs.d/site-lisp)
;; and add the following line to your .emacs:

;; (require 'n4js)

;; Type M-x n4js-start to run Neo4j Shell.
;; See also `comint-mode' to check key bindings.

(require 'comint)
(require 'cypher-mode)

;;; Code:
(defvar n4js-cli-program "neo4j-shell"
  "The cli program to start neo4j shell.")

(defvar n4js-cli-arguments '()
  "List of command line arguments to pass to neo4j shell cli programm.")

(defvar n4js-font-lock-keywords cypher-font-lock-keywords
  "Font lock keywords list, default is to taken from cypher-mode.")

(defvar n4js-pop-to-buffer nil
  "Whether to pop up the neo4j shell buffer after sending command to execute.")

(defvar n4js-pop-to-buffer-function 'pop-to-buffer
  "The function to pop up the neo4j shell buffer.")

(define-derived-mode n4js-mode comint-mode "Neo4j Shell"
  "Major mode for `n4js-start'."
  ;; not allow the prompt to be deleted
  (setq comint-prompt-read-only t)
  ;; font lock keywords
  (set (make-local-variable 'font-lock-defaults) '(n4js-font-lock-keywords t)))

(defun n4js-pop-to-buffer ()
  "Pop the neo4j shell buffer to the current window."
  (apply n4js-pop-to-buffer-function '("*neo4j-shell*")))

;;; Taken from masteringemacs with some changes
;;; https://www.masteringemacs.org/article/comint-writing-command-interpreter
;;;###autoload
(defun n4js-start ()
  "Start neo4j shell comint mode."
  (interactive)
  (let ((buffer (comint-check-proc "*neo4j-shell*")))
    ;; pop to the "*neo4j-shell*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'n4js-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create "*neo4j-shell*")
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "neo4j-shell" nil n4js-cli-program nil
             n4js-cli-arguments)
      (n4js-mode))))

;;; Send the query string to neo4j shell to execute
(defun n4js-send-string (string)
  "Send the input string to neo4j shell process."
  (if (not (comint-check-proc "*neo4j-shell*"))
      (message "No neo4j shell process started")
    (progn
      (process-send-string "*neo4j-shell*" (concat string "\n"))
      (when n4js-pop-to-buffer
        (n4js-pop-to-buffer)))))

(defun n4js-send-region (beg end)
  "Send the region from beg to end to neo4j process."
  (let ((string (buffer-substring-no-properties beg end)))
    (n4js-send-string string)))

(defun n4js-send-current-region ()
  "Send the selected region to neo4j shell process."
  (interactive)
  (let* ((beg (region-beginning))
         (end (region-end)))
    (n4js-send-region beg end)))

(defun n4js-send-buffer ()
  "Send the current buffer to neo4j shell process."
  (interactive)
  (let* ((beg (point-min))
         (end (point-max)))
    (n4js-send-region beg end)))

(defun n4js-send-paragraph ()
  "Send the current paragraph to neo4j shell process."
  (interactive)
  (let ((beg (save-excursion
               (backward-paragraph)
               (point)))
        (end (save-excursion
               (forward-paragraph)
               (point))))
    (n4js-send-region beg end)))

(defun n4js-send-region-or-buffer ()
  "Send the selected region if presented, otherwise, send the whole buffer."
  (interactive)
  (if (use-region-p)
      (n4js-send-current-region)
    (n4js-send-buffer)))

(defun n4js-send-dwim ()
  "Send the selected region presented, otherwise, send the current paragraph."
  (interactive)
  (if (use-region-p)
      (n4js-send-current-region)
    (n4js-send-paragraph)))

;;;###autoload
(defun n4js-switch-to-buffer ()
  "Switch to neo4j shell buffer."
  (interactive)
  (if (comint-check-proc "*neo4j-shell*")
      (switch-to-buffer "*neo4j-shell*")
    (n4js-start)))

(provide 'n4js)

;;; n4js.el ends here
