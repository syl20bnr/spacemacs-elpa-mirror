;;; bbdb-handy.el --- BBDB window as email-address chooser when write an email

;; Copyright (c) 2015, Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/bbdb-handy
;; Package-Version: 20150707.1752
;; Version: 0.0.1
;; Package-Requires: ((bbdb "3.1"))

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; ## Introduce ##
;;
;; bbdb-handy is a BBDB tool, when in headers (TO: and CC:) of message-mode buffer,
;; Type TAB key will will pop up a BBDB window as email-address chooser.
;;
;; ![snapshot1.gif](snapshots/snapshot1.gif)
;;
;; ## Download ##
;;
;;     https://github.com/tumashu/bbdb-handy
;;
;; ## Install ##
;;
;; 1. Config melpa: http://melpa.org/#/getting-started
;; 2. M-x package-install RET bbdb-handy RET
;; 3. Add code to your emacs config file:（for example: ~/.emacs）：
;;
;; ```lisp
;; (require 'bbdb-handy)
;; ```

;; ## Usage ##
;;
;; ### The easiest way ###
;;
;; Add the below line to your emacs config file.
;;
;; ```
;; (bbdb-handy-enable)
;; ```
;;
;; ### The manual way ###
;;
;; The function `bbdb-handy-enable' only rebind some key in `bbdb-mode-map'
;; and `message-mode-map', user can do this job by hand, for example:
;;
;; ```
;; ;; bbdb-mode
;; (define-key bbdb-mode-map "g" 'bbdb-handy-display-all-records)
;; (define-key bbdb-mode-map "q" 'bbdb-handy-quit-window)
;; (define-key bbdb-mode-map "p" 'bbdb-handy-bbdb-push-mail)
;; (define-key bbdb-mode-map "\C-s" 'bbdb-handy-search-records)
;; (define-key bbdb-mode-map "b" 'bbdb-handy-search-records)
;; (define-key bbdb-mode-map "\C-c\C-c" 'bbdb-handy-push-mail)
;; (define-key bbdb-mode-map (kbd "RET") 'bbdb-handy-push-mail-and-quit-window)
;; ;; Message-mode
;; (define-key message-mode-map "\t" 'bbdb-handy-message-tab)
;; ```

;;; Code:
(require 'bbdb)
(require 'message)

(defvar bbdb-handy-push-buffer nil
  "An alist, record buffer, buffer-window and window-point")

(defun bbdb-handy-push-mail (records &optional n verbose)
  "Push email-address(es) of `records' to buffer in `bbdb-handy-push-buffer'."
  (interactive (list (bbdb-do-records)
                     (or (consp current-prefix-arg)
                         current-prefix-arg)
                     t))
  (setq records (bbdb-record-list records))
  (if (not records)
      (if verbose (message "No records"))
    (let ((to (bbdb-mail-address records n nil verbose))
          (buffer (cdr (assoc 'buffer bbdb-handy-push-buffer))))
      (when buffer
        (with-current-buffer buffer
          (when (not (string= "" to))
            (when (save-excursion
                    (let* ((end (point))
                           (begin (line-beginning-position))
                           (string (buffer-substring-no-properties
                                    begin end)))
                      (and (string-match-p "@" string)
                           (not (string-match-p ", *$" string)))))
              (insert ", "))
            (insert to)
            (message "%s, will be push to buffer: \"%s\"" to buffer))
          (setcdr (assoc 'window-point bbdb-handy-push-buffer) (point)))))))

(defun bbdb-handy-quit-window ()
  "Quit BBDB window and return message window.
Before quit, this command will do some clean jobs."
  (interactive)
  ;; Hide header line in BBDB window.
  (with-current-buffer bbdb-buffer-name
    (setq header-line-format nil))
  (quit-window)
  ;; Update window point in Message window.
  (set-window-point
   (cdr (assoc 'window bbdb-handy-push-buffer))
   (cdr (assoc 'window-point bbdb-handy-push-buffer)))
  ;; Reset variable `bbdb-handy-push-buffer'
  (setq bbdb-handy-push-buffer nil))

(defun bbdb-handy-push-mail-and-quit-window ()
  "Push email-address to Message window and quit BBDB window."
  (interactive)
  (if bbdb-handy-push-buffer
      (progn (call-interactively 'bbdb-handy-push-mail)
             (bbdb-handy-quit-window))
    (message "Variable `bbdb-handy-push-buffer' set to `nil', Do nothing!!")))

(defun bbdb-handy-grab-word ()
  "Grab word at point, which used to build search string."
  (buffer-substring
   (point)
   (save-excursion
     (skip-syntax-backward "w")
     (point))))

(defun bbdb-handy ()
  "Open BBDB window as an email-address selector,
if Word at point is found, BBDB will search this word
and show search results in BBDB window. This command
only useful in Message buffer."
  (interactive)
  (let ((buffer (current-buffer))
        (bbdb-pop-up-window-size 1.0)
        prefix-string)

    ;; Update `bbdb-handy-push-buffer'
    (if (derived-mode-p 'message-mode)
        (setq prefix-string (bbdb-handy-grab-word)
              bbdb-handy-push-buffer
              `((buffer . ,buffer)
                (window . ,(get-buffer-window))
                (window-point . ,(point))))
      (setq bbdb-handy-push-buffer nil
            prefix-string nil))

    ;; Call bbdb
    (if (and prefix-string (> (length prefix-string) 0))
        (progn
          (delete-char (- 0 (length prefix-string)))
          (bbdb prefix-string))
      (if (save-excursion
            (let* ((end (point))
                   (begin (line-beginning-position))
                   (string (buffer-substring-no-properties
                            begin end)))
              (string-match-p "@.*>$" string)))
          ;; When point at "email@email.com><I>",
          ;; launch `bbdb-complete-mail'.
          (let ((bbdb-pop-up-window-size 0.2)
                (bbdb-complete-mail-allow-cycling t))
            (message "Cycling current user's email address!")
            (bbdb-complete-mail)
            ;; Close bbdb-buffer's window when complete with
            ;; `bbdb-complete-mail'
            (let ((window (get-buffer-window bbdb-buffer-name)))
              (if (window-live-p window)
                  (quit-window nil window))))
        (bbdb "")))

    ;; Update `header-line-format'
    (when (derived-mode-p 'message-mode)
      (with-current-buffer bbdb-buffer-name
        (setq header-line-format
              (format "## Type `C-c C-c', `p' or `RET' to push email to buffer \"%s\". ##"
                      (buffer-name buffer)))))))

(defun bbdb-handy-message-tab ()
  "A command which will be bound to TAB key in message-mode,
when in message headers, this command will launch `bbdb-handy',
when in message body, this command will indent regular text."
  (interactive)
  (cond
   ;; Type TAB launch bbdb-handy when in header.
   ((and (save-excursion
           (let ((point (point)))
             (message-goto-body)
             (> (point) point)))
         (not (looking-back "^\\(Subject\\|From\\): *.*"
                            (line-beginning-position)))
         (not (looking-back "^" (line-beginning-position))))
    (bbdb-handy))
   (message-tab-body-function (funcall message-tab-body-function))
   (t (funcall (or (lookup-key text-mode-map "\t")
                   (lookup-key global-map "\t")
                   'indent-relative)))))

(defun bbdb-handy-search-records ()
  (interactive)
  (call-interactively 'bbdb)
  (message "Type `g', show all contacts records"))

(defun bbdb-handy-display-all-records ()
  (interactive)
  (bbdb-display-all-records)
  (message "Show all contacts records ..."))

(defun bbdb-handy-keybinding-setup ()
  "Setup bbdb-handy Keybindings."
  (define-key bbdb-mode-map "g" 'bbdb-handy-display-all-records)
  (define-key bbdb-mode-map "q" 'bbdb-handy-quit-window)
  (define-key bbdb-mode-map "p" 'bbdb-handy-push-mail)
  (define-key bbdb-mode-map "\C-s" 'bbdb-handy-search-records)
  (define-key bbdb-mode-map "b" 'bbdb-handy-search-records)
  (define-key bbdb-mode-map "\C-c\C-c" 'bbdb-handy-push-mail)
  (define-key bbdb-mode-map (kbd "RET") 'bbdb-handy-push-mail-and-quit-window))

(defun bbdb-handy-enable ()
  "Enable bbdb-handy, it will rebind TAB key in `message-mode-map'."
  (interactive)
  (require 'message)
  (add-hook 'bbdb-mode-hook 'bbdb-handy-keybinding-setup)
  (define-key message-mode-map "\t" 'bbdb-handy-message-tab)
  (message "BBDB-handy: Override BBDB keybindings: g, q, p, CTRL-s, b, CTRL-c CTRL-c, RET"))


(provide 'bbdb-handy)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; bbdb-handy.el ends here
