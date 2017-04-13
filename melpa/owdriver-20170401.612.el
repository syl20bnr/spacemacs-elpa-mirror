;;; owdriver.el --- Quickly perform various actions on other windows

;; Copyright (C) 2014  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: convenience
;; Package-Version: 20170401.612
;; URL: https://github.com/aki2o/owdriver
;; Version: 0.1.0
;; Package-Requires: ((smartrep "0.0.3") (log4e "0.2.0") (yaxception "0.2.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; This extension provides the function for doing various action to
;; other windows quickly in multi window situation.
;; In default, that's move, scroll and isearch.
;; Moreover, you can add the action what you want.

;;; Dependency:
;; 
;; - smartrep.el ( see <https://github.com/myuhe/smartrep.el> )
;; - yaxception.el ( see <https://github.com/aki2o/yaxception> )
;; - log4e.el ( see <https://github.com/aki2o/log4e> )

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;; 
;; (require 'owdriver)

;;; Configuration:
;; 
;; ;; Make config suit for you. About the config item, see Customization or eval the following sexp.
;; ;; (customize-group "owdriver")
;; 
;; ;; If you want to do the default config
;; (owdriver-config-default)
;; 
;; (owdriver-mode 1)

;;; Customization:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "owdriver-[^-]" :docstring t)
;; `owdriver-prefix-key'
;; String of the prefix keystroke for `owdriver-mode-map'.
;; `owdriver-next-window-prefer-pophint'
;; Whether to prefer to use `pophint:do' for `owdriver-next-window'.
;; 
;;  *** END auto-documentation

;;; API:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "owdriver-[^-]" :docstring t)
;; `owdriver-next-window'
;; Change the window of `owdriver--window'.
;; `owdriver-previous-window'
;; Change the window of `owdriver--window'.
;; `owdriver-focus-window'
;; Quit driving `owdriver--window' and move to `owdriver--window'.
;; `owdriver-quit'
;; Quit driving `owdriver--window'.
;; 
;;  *** END auto-documentation
;; [EVAL] (autodoc-document-lisp-buffer :type 'macro :prefix "owdriver-[^-]" :docstring t)
;; `owdriver-define-command'
;; Define the command for driving `owdriver--window' from COMMAND.
;; 
;;  *** END auto-documentation
;; [EVAL] (autodoc-document-lisp-buffer :type 'function :prefix "owdriver-[^-]" :docstring t)
;; `owdriver-add-keymap'
;; Add the keymap of `owdriver-mode-map'.
;; `owdriver-config-default'
;; Do the recommended configuration.
;; 
;;  *** END auto-documentation
;; [Note] Functions and variables other than listed above, Those specifications may be changed without notice.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 24.2.1 (i386-mingw-nt5.1.2600) of 2012-12-08 on GNUPACK
;; - smartrep.el ... Version 0.0.3
;; - yaxception.el ... Version 0.2.0
;; - log4e.el ... Version 0.2.0


;; Enjoy!!!


(eval-when-compile (require 'cl))
(require 'smartrep)
(require 'log4e)
(require 'yaxception)
(require 'inertial-scroll nil t)
(require 'pophint nil t)

(defgroup owdriver nil
  "Quickly perform various actions on other windows."
  :group 'convenience
  :prefix "owdriver-")

(defcustom owdriver-prefix-key "M-o"
  "String of the prefix keystroke for `owdriver-mode-map'."
  :type 'string
  :group 'owdriver)

(defcustom owdriver-next-window-prefer-pophint t
  "Whether to prefer to use `pophint:do' for `owdriver-next-window'."
  :type 'boolean
  :group 'owdriver)


(log4e:deflogger "owdriver" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                      (error . "error")
                                                      (warn  . "warn")
                                                      (info  . "info")
                                                      (debug . "debug")
                                                      (trace . "trace")))
(owdriver--log-set-level 'trace)


(defvar owdriver--window nil "Current window drived by the command of `owdriver-mode-map'.")
(defvar owdriver--move-window-amount nil)
(defvar owdriver--keymap-alist nil)


;;;;;;;;;;;;;
;; Utility

(defun* owdriver--show-message (msg &rest args)
  (apply 'message (concat "[OWDRIVER] " msg) args)
  nil)

(defmacro owdriver--awhen (test &rest body)
  (declare (indent 1))
  `(let ((it ,test)) (when it ,@body)))

(defmacro owdriver--with-selected-window (tasknm force-next-window &rest body)
  (declare (indent 2))
  `(yaxception:$
     (yaxception:try
       (owdriver--trace "start with select window : wnd[%s] force-next-window[%s]"
                        owdriver--window ,force-next-window)
       (when (or ,force-next-window
                 (not (window-live-p owdriver--window))
                 (eq owdriver--window (nth 0 (get-buffer-window-list))))
         (let ((owdriver--move-window-amount 1))
           (owdriver-next-window)))
       (with-selected-window owdriver--window
         ,@body))
     (yaxception:catch 'error e
       (owdriver--show-message "Failed %s : %s" ,tasknm (yaxception:get-text e))
       (owdriver--error "failed %s : %s\n%s"
                       ,tasknm
                       (yaxception:get-text e)
                       (yaxception:get-stack-trace-string e)))))

(defun owdriver--get-binding-keys (cmd)
  (owdriver--trace "start get binding keys : %s" cmd)
  (loop for b in (where-is-internal cmd global-map)
        for bindkey = (or (ignore-errors (key-description b))
                          "")
        if (and (not (string= bindkey ""))
                (not (string-match "\\`<menu-bar>" bindkey))
                (not (string-match "\\`<[^>]*mouse[^>]*>" bindkey)))
        collect (progn (owdriver--trace "found binding : %s" bindkey)
                       bindkey)))

(defun owdriver--get-keybind (cmd)
  (owdriver--trace "start get keybind : %s" cmd)
  (loop with ret = nil
        for k in (owdriver--get-binding-keys cmd)
        if (or (not ret)
               (< (length k) (length ret)))
        do (setq ret k)
        finally return (progn (owdriver--trace "got keybind : %s" ret)
                              ret)))


;;;;;;;;;;
;; Mode

;;;###autoload
(defvar owdriver-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode owdriver-mode
  "Quickly perform various actions on other windows."
  :init-value nil
  :lighter " OW"
  :keymap owdriver-mode-map
  :global t
  :group 'owdriver
  (smartrep-define-key owdriver-mode-map owdriver-prefix-key owdriver--keymap-alist))


;;;;;;;;;;;;;;;;;;
;; User Command

(defun owdriver-next-window (&optional reverse)
  "Change the window of `owdriver--window'."
  (interactive)
  (yaxception:$
    (yaxception:try
      (let* ((actwnd (get-buffer-window))
             (currwnd (if (window-live-p owdriver--window) owdriver--window actwnd))
             (move-amount (or owdriver--move-window-amount
                              (when (window-live-p owdriver--window) 1)
                              2))
             (is-nextable-window (lambda (w)
                                   (and (window-live-p w)
                                        (not (eq w actwnd))
                                        (not (eq w currwnd))
                                        (not (minibufferp (window-buffer w))))))
             nextwnd popwnd wndloc)
        (select-window currwnd)
        (owdriver--trace "start %s window. currwnd[%s] move-amount[%s]"
                         (if reverse "previous" "next") (selected-window) move-amount)
        ;; Move to next target window
        (if (and (and owdriver-next-window-prefer-pophint
                      (featurep 'pophint)
                      (boundp 'pophint--next-window-source)
                      (>= (loop for w in (window-list) count (funcall is-nextable-window w)) 2)))
            (setq nextwnd (pophint:do :source pophint--next-window-source :allwindow t))
          (while (and (> move-amount 0)
                      (not (eq nextwnd currwnd)))
            (other-window (if reverse -1 1))
            (setq nextwnd (get-buffer-window))
            (owdriver--trace "selected next window : %s" nextwnd)
            (when (funcall is-nextable-window nextwnd)
              (decf move-amount)
              (owdriver--trace "decremented move-amount[%s]" move-amount))))
        ;; Blink target window after move
        (when (not (eq nextwnd currwnd))
          (owdriver--trace "start blink window : %s" nextwnd)
          (let ((ov (make-overlay (window-start) (window-end))))
            (yaxception:$
              (yaxception:try
                (overlay-put ov 'face 'highlight)
                (select-window actwnd)
                (sit-for 0.1)
                (select-window nextwnd))
              (yaxception:catch 'error e
                (yaxception:throw e))
              (yaxception:finally
                (delete-overlay ov)))))
        ;; Return to working window at last
        (select-window actwnd)
        (setq owdriver--window nextwnd)
        (owdriver--show-message "Drived window is '%s'" owdriver--window)))
    (yaxception:catch 'error e
      (owdriver--show-message "Failed next window : %s" (yaxception:get-text e))
      (owdriver--error "failed next window : %s\n%s"
                      (yaxception:get-text e)
                      (yaxception:get-stack-trace-string e)))))

(defun owdriver-previous-window ()
  "Change the window of `owdriver--window'."
  (interactive)
  (owdriver-next-window t))

(defun owdriver-focus-window ()
  "Quit driving `owdriver--window' and move to `owdriver--window'."
  (interactive)
  (when (window-live-p owdriver--window)
    (select-window owdriver--window)
    (keyboard-quit)))

(defun owdriver-quit ()
  "Quit driving `owdriver--window'."
  (interactive)
  (keyboard-quit))


;;;;;;;;;;;;;;;
;; For Setup

;;;###autoload
(defun owdriver-add-keymap (keystroke command)
  "Add the keymap of `owdriver-mode-map'."
  (owdriver--trace "start add keymap. keystroke[%s] command[%s]" keystroke command)
  (when (and (stringp keystroke)
             (not (string= keystroke ""))
             (commandp command))
    (define-key owdriver-mode-map
      (read-kbd-macro (concat owdriver-prefix-key " " keystroke))
      command)
    (owdriver--awhen (assoc keystroke owdriver--keymap-alist)
      (setq owdriver--keymap-alist (delq it owdriver--keymap-alist)))
    (add-to-list 'owdriver--keymap-alist `(,keystroke . ,command))))

;;;###autoload
(defmacro owdriver-define-command (command add-keymap &rest body)
  "Define the command for driving `owdriver--window' from COMMAND.

The command named `owdriver-do-COMMAND' is defined by this function.
ADD-KEYMAP is boolean. If non-nil, do `owdriver-add-keymap' using the key bound to COMMAND in `global-map'.
BODY is sexp. If COMMAND is used in `owdriver--window' actually, this value is no need."
  (declare (indent 2))
  (let* ((body (or body `((call-interactively ',command))))
         (cmdnm (symbol-name command))
         (ncommand (intern (concat "owdriver-do-" cmdnm)))
         (tasknm (replace-regexp-in-string "-" " " cmdnm)))
    `(progn
       (owdriver--trace "start define command[%s]. add-keymap[%s]" ,cmdnm ,add-keymap)
       ;;;###autoload
       (defun ,ncommand (&optional arg)
         ,(format "Do `%s' in `owdriver--window'.\n\nIf prefix argument is given, do `owdriver-next-window' before that." cmdnm)
         (interactive "p")
         (let ((force-next-window (and arg (> arg 1))))
           (owdriver--with-selected-window ,tasknm force-next-window
             ,@body)))
       (when ,add-keymap
         (dolist (k (owdriver--get-binding-keys ',command))
           (owdriver-add-keymap k ',ncommand))))))

;;;###autoload
(defun owdriver-config-default ()
  "Do the recommended configuration."
  ;; Own command
  (owdriver-add-keymap "C-o"        'owdriver-next-window)
  (owdriver-add-keymap "C-S-o"      'owdriver-previous-window)
  (owdriver-add-keymap "<C-return>" 'owdriver-focus-window)
  (owdriver-add-keymap "<return>"   'owdriver-quit)
  (owdriver-add-keymap "C-q"        'owdriver-quit)
  ;; Basic command
  (owdriver-define-command newline                 t (owdriver-quit))
  (owdriver-define-command scroll-up               t)
  (owdriver-define-command scroll-up-command       t)
  (owdriver-define-command scroll-down             t)
  (owdriver-define-command scroll-down-command     t)
  (owdriver-define-command scroll-left             t (scroll-left 10 t))
  (owdriver-define-command scroll-right            t (scroll-right 10 t))
  (owdriver-define-command next-line               t)
  (owdriver-define-command previous-line           t)
  (owdriver-define-command forward-char            t)
  (owdriver-define-command forward-word            t)
  (owdriver-define-command backward-char           t)
  (owdriver-define-command backward-word           t)
  (owdriver-define-command move-beginning-of-line  t)
  (owdriver-define-command move-end-of-line        t)
  (owdriver-define-command beginning-of-buffer     t)
  (owdriver-define-command end-of-buffer           t)
  (owdriver-define-command isearch-forward         t (isearch-forward))
  (owdriver-define-command isearch-backward        t (isearch-backward))
  (owdriver-define-command set-mark-command        t)
  (owdriver-define-command kill-ring-save          t (call-interactively 'kill-ring-save) (deactivate-mark))
  ;; Third party command
  (owdriver-define-command pophint:do t (pophint:do :not-switch-window t))
  (owdriver-define-command inertias-up t)
  (owdriver-define-command inertias-down t)
  )


(provide 'owdriver)
;;; owdriver.el ends here
