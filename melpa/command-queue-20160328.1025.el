;;; command-queue.el --- shell command queue

;; Copyright (C) 2016 by Yuki INOUE

;; Author: Yuki INOUE <inouetakahiroki at gmail.com>
;; Package-Requires: ((emacs "24.3"))
;; Package-Version: 20160328.1025
;; URL: https://github.com/Yuki-Inoue/command-queue
;; Version: 0.0.1

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


;;; why not use shell-command-queue?
;;;   @ https://www.emacswiki.org/emacs/shell-command-queue.el

;; * I didn't want to manage process names.
;; * I didn't want to kick `run' after `add'.
;;   * unless on errornous situation, developer just want the queue
;;     to be processed as soon as he publish the command
;; * license information was not clear.

;;; How to use

;; M-x command-queue-publish-command
;;  and enter the shell command you want to execute.


(defvar-local command-queue--commands-list nil
  "The command list.")

(defvar-local command-queue--process-running-flag nil
  "Whether the process is running.")

(defcustom command-queue-buffer-name "*command queue*"
  "The buffer for command queue execution."
  :type '(string)
  :group 'command-queue)

(defun command-queue--append-string-to-current-buffer (string)
  "Append STRING to the end of current buffer."
  (save-excursion
    (goto-char (point-max))
    (insert string)))

(defun command-queue--publish-commands (buffer-or-name &rest commands)
  (let ((buffer (get-buffer-create buffer-or-name)))
    (with-current-buffer buffer
      (setq command-queue--commands-list
            (append command-queue--commands-list commands))
      (unless command-queue--process-running-flag
        (command-queue--start-next-command)))))

;;;###autoload
(defun command-queue-publish-command (command)
  (interactive "sCommand: ")
  (switch-to-buffer-other-window command-queue-buffer-name)
  (goto-char (point-max))
  (command-queue--publish-commands
   command-queue-buffer-name command))

(defun command-queue--start-next-command ()
  "Run the first command in the list"
  (if (null command-queue--commands-list)
      (command-queue--append-string-to-current-buffer "---\nDone.\n---\n")
    (let* ((command (car command-queue--commands-list))
           (process-name (concat "command-queue: " command)))
      (setq command-queue--commands-list (cdr command-queue--commands-list))
      (command-queue--append-string-to-current-buffer (format ">>> %s\n" command))
      (let ((process (start-process-shell-command process-name (current-buffer)
                                                  command)))
        (set-process-sentinel process 'command-queue--sentinel)
        (setq command-queue--process-running-flag t)))))

(defun command-queue--sentinel (process signal)
  "After a process exited, call `start-next-command' again"
  (when (eq (process-status process) 'exit)
    (let ((buffer (process-buffer process))
          (exit-status (process-exit-status process)))
      (with-current-buffer buffer
        (setq command-queue--process-running-flag nil)
        (if (not (eq exit-status 0))
            (command-queue--abort)
          (command-queue--start-next-command))))))

(defun command-queue--abort ()
  (command-queue--append-string-to-current-buffer "*** aborted\n\nremainings:\n")
  (dolist (command command-queue--commands-list)
    (command-queue--append-string-to-current-buffer (concat command "\n"))))

(provide 'command-queue)

;;; command-queue.el ends here
