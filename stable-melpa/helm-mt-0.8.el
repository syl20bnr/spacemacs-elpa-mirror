;;; helm-mt.el --- helm multi-term management -*- lexical-binding: t -*-

;; Copyright (C) 2015, 2016 Didier Deshommes <dfdeshom@gmail.com>

;; Author: Didier Deshommes <dfdeshom@gmail.com>
;; URL: https://github.com/dfdeshom/helm-mt
;; Package-Version: 0.8
;; Version: 0.6
;; Package-Requires: ((emacs "24") (helm "0.0") (multi-term "0.0") (cl-lib "0.5"))
;; Keywords: helm multi-term

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

;; Create and delete multi-term terminals easily with Helm.  A call to
;; `helm-mt` will show a list of running terminal sessions by
;; examining buffers with major mode `term-mode` or `shell-mode`.
;; From there, you should be able to create, delete or switch over to
;; existing terminal buffers.

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-lib)
(require 'helm-source)
(require 'helm-utils)
(require 'multi-term)

(defgroup helm-mt nil
  "Open helm-mt."
  :prefix "helm-mt/" :group 'helm)

(defvar helm-mt/all-terminal-modes '(term-mode shell-mode)
  "If a buffer has a major mode in this list, helm-mt will list it as an option.
The order of the modes controls which is the default action in the helm-mt UI.")

(defun helm-mt/terminal-buffers ()
  "Filter for buffers that are terminals only."
  (cl-loop for buf in (buffer-list)
           if (member (buffer-local-value 'major-mode buf) helm-mt/all-terminal-modes)
           collect (buffer-name buf)))

(defun helm-mt/unique-buffer-name (name mode)
  "Unique buffer from NAME and MODE."
  (cl-case mode
    ('term-mode
     (generate-new-buffer-name (format "*terminal<%s>*" name)))
    ('shell-mode
     (generate-new-buffer-name (format "*shell<%s>*" name)))))

(defun helm-mt/new-term (name)
  "Create terminal NAME."
  (multi-term)
  (rename-buffer name))

(defun helm-mt/launch-term (name mode)
  "Create new terminal in a buffer called NAME using optional MODE."
  (message (format "Launching term \"%s\" with mode \"%s\" " name mode))
  (cl-case mode
    ('term-mode
     (helm-mt/new-term (helm-mt/unique-buffer-name name 'term-mode)))
    ('shell-mode
     (shell (helm-mt/unique-buffer-name name 'shell-mode)))))

(defun helm-mt/delete-marked-terms (_ignored)
  "Delete marked terminals.
The _IGNORED argument is not used."
  (let* ((bufs (helm-marked-candidates))
         (killed-bufs (cl-count-if 'helm-mt/delete-term bufs)))
    (with-helm-buffer
      (setq helm-marked-candidates nil
            helm-visible-mark-overlays nil))
    (message "Deleted %s terminal(s)" killed-bufs)))

(defun helm-mt/delete-term (name)
  "Delete terminal NAME."
  (if (get-buffer-process name)
      (delete-process name))
  (kill-buffer name))

(defun helm-mt/helper-auto-terminal ()
  "Launch a term with the current directory as the name."
  (let* ((name (replace-regexp-in-string  (regexp-quote "Directory ") "" (pwd)))
         (terminal_name (helm-mt/unique-buffer-name name 'term-mode)))
    (helm-mt/new-term terminal_name)))

(defun helm-mt/auto-terminal ()
  "Launch a term with the current directory as the name."
  (interactive)
  (helm-mt/helper-auto-terminal)
  ;;(helm-keyboard-quit)
  ;;(helm-exit-minibuffer)
  ;;(exit-minibuffer)
  ;;(helm-quit-and-execute-action 'helm-mt/helper-launch-term-with-named-dir)
  )

(defvar helm-mt/keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c n") 'helm-mt/auto-terminal)
    (delq nil map))
  "Keymap for helm-mt.")

(defun helm-mt/term-source-terminals ()
  "Helm source with candidates for all terminal buffers."
  (helm-build-sync-source
      "Terminals"
    :candidates (lambda () (or
                            (helm-mt/terminal-buffers)
                            (list "")))
    :action (helm-make-actions
             "Switch to terminal buffer"
             (lambda (candidate)
               (switch-to-buffer candidate))
             "Exit marked terminal(s)"
             (lambda (_ignored)
               (helm-mt/delete-marked-terms _ignored)))))

(defun helm-mt/term-source-terminal-not-found ()
  "Dummy helm source to launch a new terminal."
  (helm-build-dummy-source
      "Launch a new terminal"
    :action (apply 'helm-make-actions
                   (apply 'append
                          (mapcar (lambda (mode)
                                    (list (format "Launch new %s" mode)
                                          `(lambda (candidate)
                                             (helm-mt/launch-term candidate (quote ,mode)))))
                                  helm-mt/all-terminal-modes)))))

(defun helm-mt/shell-advice (orig-fun &rest args)
  "Advice that has helm-mt run when invoking `M-x shell` or `M-x term`.
Argument ORIG-FUN is the original function, ARGS are its arguments."
  (message "wrapping shell with helm-mt")
  (if (called-interactively-p 'interactive)
      (call-interactively 'helm-mt)
    (apply orig-fun args)))

;;;###autoload
(defun helm-mt/wrap-shells (onoff)
  "Put advice around shell functions when called interactively.
This routes to helm-mt UI instead of launching a new shell/terminal.
If ONOFF is t, activate the advice and if nil, remove it."
  (interactive)
  (dolist (mode helm-mt/all-terminal-modes)
    (let ((fun (intern (replace-regexp-in-string (regexp-quote "-mode") "" (symbol-name mode)))))
      (if onoff
          (eval
           `(add-function :around (symbol-function (quote ,fun)) #'helm-mt/shell-advice))
        (eval `(advice-remove (quote ,fun) #'helm-mt/shell-advice))))))

;;;###autoload
(defun helm-mt ()
  "Custom helm buffer for terminals only."
  (interactive)
  (helm :sources `(,(helm-mt/term-source-terminals)
                   ,(helm-mt/term-source-terminal-not-found))
        :keymap helm-mt/keymap
        :buffer "*helm mt*"))

(provide 'helm-mt)
;;; helm-mt.el ends here
