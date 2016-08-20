;;; mobdebug-mode.el --- Major mode for MobDebug -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; URL: https://github.com/deftsp/mobdebug-mode
;; Package-Version: 20140109.1946
;; Package-Requires: ((lua-mode "20130419") (emacs "24"))
;; Keywords:

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

;;

;;; Code:

(require 'lua-mode)
(require 'comint)

(defconst mobdebug-keywords
  '("connect" "set"))

(defvar mobdebug-font-lock-keywords
  (list
   ;; highlight all the reserved commands.
   `(,(concat "\\_<" (regexp-opt mobdebug-keywords) "\\_>") . font-lock-keyword-face))
  "Additional expressions to highlight in `mobdebug-mode'.")

(defvar mobdebug-prompt "MobDebug> "
  "Prompt for `mobdebug'.")

(defface mobdebug-buffer-face
  '((t (:foreground "#839496" :background "#002b36")))
  "*Face used when mobdebug minor mode on")

(defvar mobdebug-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'completion-at-point)
    map)
  "Keymap for MobDebug mode.")

(defvar mobdebug-buffer nil "The current MobDebug process buffer.")

(defvar mobdebug-working-file nil "File in which MobDebug is debuging.")

(defvar mobdebug-use-evil-binding nil "If non-nil use evil mobdebug minor mode.")

(define-derived-mode mobdebug-mode comint-mode "MobDebug"
  "Major mode for `mobdebug'.

\\<mobdebug-mode-map>"
  :syntax-table lua-mode-syntax-table
  (setq comint-prompt-regexp (concat "^" (regexp-quote mobdebug-prompt)))
  (setq comint-prompt-read-only t)
  (setq comint-process-echoes nil)
  (set (make-local-variable 'comint-output-filter-functions)
       '(ansi-color-process-output
         comint-postoutput-scroll-to-bottom
         comint-truncate-buffer
         mobdebug-output-filter))
  (set (make-local-variable 'comint-input-filter-functions)
       '(mobdebug-input-filter))
  (set (make-local-variable 'paragraph-separate) "\\'") ; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-start) comint-prompt-regexp)
  (set (make-local-variable 'font-lock-defaults) '(mobdebug-font-lock-keywords t))
  (set (make-local-variable 'comint-completion-addsuffix) '("/" . ""))
  (setq mode-line-process '(":%s on " (:eval (or mobdebug-working-file "_"))))
  ;; Useful for `hs-minor-mode'.
  (setq-local comment-start "--")
  (setq-local comment-use-global-state t))

(defun mobdebug-get-buffer-by-path (file-path)
  (get-buffer (file-name-nondirectory file-path)))

(defun mobdebug-output-filter (output)
  (cond ((string-match "Paused at file \\(.*\\) line \\([0-9]*\\)" output)
         (let* ((file-path (match-string 1 output))
                (line-num (string-to-number (match-string 2 output)))
                (file-name (file-name-nondirectory file-path))
                (buffer (get-buffer file-name)))

           (cond ((and file-name (not line-num)) ;; first paused
                  (setq mobdebug-working-file file-name))
                 ((and buffer line-num)
                  (with-current-buffer buffer
                    (let ((mark-bol (save-excursion
                                      (goto-line line-num)
                                      (point-marker))))
                      (if (not (string= mobdebug-working-file file-name))
                          (setq mobdebug-working-file file-name))
                      (setq overlay-arrow-position mark-bol)))))))))

(defun mobdebug-input-filter (input)
  (let ((buffer (and mobdebug-working-file (get-buffer mobdebug-working-file))))
    (when buffer
      (with-current-buffer buffer
        (let ((command (mobdebug-trim-string input)))
          (cond ((member command '("run" "step" "out" "over" "exit"))
                 (setq overlay-arrow-position nil))))))))

;; http://ergoemacs.org/emacs/modernization_elisp_lib_problem.html
(defun mobdebug-trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defvar mobdebug-lua-path "mobdebug.sh")
(defvar mobdebug-arguments '())

(defun run-mobdebug ()
  "Run an inferior instance of `mobdebug' inside Emacs."
  (interactive)
  (let* ((mobdebug-program mobdebug-lua-path)
         (buffer (comint-check-proc "MobDebug")))
    ;; pop to the "*MobDebug*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer
     (if (or buffer (not (derived-mode-p 'mobdebug-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*MobDebug*"))
       (current-buffer)))

    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "MobDebug" buffer
             mobdebug-program mobdebug-arguments)
      (mobdebug-mode)
      (setq mobdebug-buffer "*MobDebug*"))))

(defun run-mobdebug-persistent-window ()
  (interactive)
  (let ((win (selected-window)))
    (run-mobdebug)
    (select-window win)))

(defvar mobdebug-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "F3") 'mobdebug-print-working-buffer)
    (define-key map [f2] 'mobdebug-setb)
    (define-key map [f7] 'mobdebug-into)
    (define-key map [f8] 'mobdebug-over)
    (define-key map [f9] 'mobdebug-run)
    (define-key map [C-f9] 'mobdebug-out)

    map)
  "Keymap for mobdebug-minor-mode.")

;;;###autoload
(define-minor-mode mobdebug-minor-mode
  "Toggle MobDebug mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state."
  :lighter " MobDebug"
  :keymap mobdebug-minor-mode-map
  (if mobdebug-minor-mode
      (progn
        (buffer-face-set 'mobdebug-buffer-face)
        (buffer-face-mode t)
        (if (and (fboundp 'evil-mobdebug-minor-mode) mobdebug-use-evil-binding)
            (evil-mobdebug-minor-mode +1)))
    (buffer-face-set nil)
    (if (and (fboundp 'evil-mobdebug-minor-mode) mobdebug-use-evil-binding)
            (evil-mobdebug-minor-mode -1))))

(defun mobdebug-send (string)
  (let ((buffer (and mobdebug-buffer (get-buffer mobdebug-buffer))))
    (if buffer
        (with-current-buffer buffer
          (comint-simple-send (mobdebug-proc) string)
          (run-hook-with-args 'comint-input-filter-functions string))
      (error "No MobDebug buffer, try `run-mobdebug'"))))

(defun mobdebug-setb ()
  "sets a breakpoint"
  (interactive)
  (mobdebug-send
   (format "setb %s %d" (buffer-file-name) (line-number-at-pos))))

(defun mobdebug-delb ()
  "removes a breakpoint"
  (interactive)
  (mobdebug-send
   (format "setb %s %d" (buffer-file-name) (line-number-at-pos))))

(defun mobdebug-delallb ()
  "removes all breakpoints"
  (interactive)
  (mobdebug-send "delallb"))


(defun mobdebug-setw ()
  "adds a new watch expression"
  (interactive)
  (mobdebug-send
   (concat "setw " (read-string "watch expression: "))))

(defun mobdebug-delw ()
  "removes the watch expression at index"
  (interactive)
  (mobdebug-send
   (concat "delw " (read-string "watch index: "))))

(defun mobdebug-delallw ()
  "removes all watch expressions"
  (interactive)
  (mobdebug-send "delallw"))


(defun mobdebug-run ()
  "runs until next breakpoint"
  (interactive)
  (mobdebug-send "run"))

(defun mobdebug-step ()
  "runs until next line, stepping into function calls"
  (interactive)
  (mobdebug-send "step"))

(defun mobdebug-over ()
  "runs until next line, stepping over function calls"
  (interactive)
  (mobdebug-send "over"))

(defun mobdebug-out ()
  "runs until line after returning from current function"
  (interactive)
  (mobdebug-send "out"))

(defun mobdebug-listb ()
  "lists breakpoints"
  (interactive)
  (mobdebug-send "listb"))

(defun mobdebug-listw ()
  "lists watch expressions"
  (interactive)
  (mobdebug-send "listw"))

(defun mobdebug-eval ()
  "evaluates expression on the current context and returns its value"
  (interactive)
  (mobdebug-send
   (concat "eval " (read-string "eval expression: "))))

(defun mobdebug-exec ()
  "executes statement on the current context"
  (interactive)
  (mobdebug-send
   (concat "exec " (read-string "exec statement: "))))

(defun mobdebug-load ()
  "loads a local file for debugging"
  (interactive)
  (let ((file-name (read-file-name "Load file: ")))
    (mobdebug-send
     (concat "load " file-name))))

(defun mobdebug-reload ()
  "restarts the current debugging session"
  (interactive)
  (mobdebug-send "reload"))

(defun mobdebug-stack ()
  "reports stack trace"
  (interactive)
  (mobdebug-send "stack"))

(defun mobdebug-output ()
  "capture and redirect io stream (default|copy|redirect)"
  (interactive)
  (mobdebug-send
   (concat "output stdout " (completing-read
                             "Method: "
                             '(default copy redirect)))))

(defun mobdebug-basedir ()
  "capture and redirect io stream (default|copy|redirect)"
  (interactive)
  (mobdebug-send
   (concat "basedir " (read-directory-name "new basedir: "))))

(defun mobdebug-exit ()
  "exits debugger"
  (interactive)
  (mobdebug-send "exit"))

(defun mobdebug-get-process ()
  "Return the current MobDebug process or nil if none is running."
  (get-buffer-process mobdebug-buffer))

(defun mobdebug-proc ()
  (or (mobdebug-get-process)
      (error "No current process.  See variable `mobdebug-buffer'")))

;;; evil support
(eval-after-load "evil"
  '(progn
     (define-minor-mode evil-mobdebug-minor-mode
       "minor mode to add evil keymappings to MobDebug mode."
       :keymap (make-sparse-keymap)
       (evil-local-mode t))

     (evil-define-key 'normal evil-mobdebug-minor-mode-map "ba" 'mobdebug-setb)
     (evil-define-key 'normal evil-mobdebug-minor-mode-map "bd" 'mobdebug-delb)
     (evil-define-key 'normal evil-mobdebug-minor-mode-map "bc" 'mobdebug-delallb)
     (evil-define-key 'normal evil-mobdebug-minor-mode-map "wa" 'mobdebug-setw)
     (evil-define-key 'normal evil-mobdebug-minor-mode-map "wd" 'mobdebug-delw)
     (evil-define-key 'normal evil-mobdebug-minor-mode-map "wc" 'mobdebug-delallw)
     (evil-define-key 'normal evil-mobdebug-minor-mode-map "r" 'mobdebug-run)
     (evil-define-key 'normal evil-mobdebug-minor-mode-map "s" 'mobdebug-step)
     (evil-define-key 'normal evil-mobdebug-minor-mode-map "n" 'mobdebug-over)
     (evil-define-key 'normal evil-mobdebug-minor-mode-map "o" 'mobdebug-over)
     (evil-define-key 'normal evil-mobdebug-minor-mode-map "f" 'mobdebug-out)
     (evil-define-key 'normal evil-mobdebug-minor-mode-map "bl" 'mobdebug-listb)
     (evil-define-key 'normal evil-mobdebug-minor-mode-map "wl" 'mobdebug-listw)
     (evil-define-key 'normal evil-mobdebug-minor-mode-map "e" 'mobdebug-eval)
     (evil-define-key 'normal evil-mobdebug-minor-mode-map ";" 'mobdebug-exec)
     (evil-define-key 'normal evil-mobdebug-minor-mode-map "L" 'mobdebug-load)
     (evil-define-key 'normal evil-mobdebug-minor-mode-map "R" 'mobdebug-reload)
     (evil-define-key 'normal evil-mobdebug-minor-mode-map "bt" 'mobdebug-stack)
     (evil-define-key 'normal evil-mobdebug-minor-mode-map ">" 'mobdebug-output)
     (evil-define-key 'normal evil-mobdebug-minor-mode-map "p" 'mobdebug-basedir)
     (evil-define-key 'normal evil-mobdebug-minor-mode-map "X" 'mobdebug-exit)
     (evil-define-key 'normal evil-mobdebug-minor-mode-map " " 'run-mobdebug-persistent-window)
     (evil-define-key 'normal evil-mobdebug-minor-mode-map "z" 'run-mobdebug)))

(provide 'mobdebug-mode)
;;; mobdebug-mode.el ends here
