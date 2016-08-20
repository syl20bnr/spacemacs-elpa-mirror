;;; kerl.el --- Emacs integration for kerl

;; Copyright (c) 2015 Correl Roush

;; Author: Correl Roush <correl@gmail.com>
;; URL: http://github.com/correl/kerl.el/
;; Package-Version: 20150424.1305
;; Version: 0.1
;; Created: 2015-04-21
;; Keywords: tools

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
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

;; Based on and borrows from rvm.el

;; M-x kerl-use allows you to switch between erlang environments that
;; have been installed using kerl.

;; M-x kerl-deactivate will stop using any kerl erlang environments,
;; allowing you to go back to using the system-installed erlang
;; environment.

;;; Code:

(defvar eshell-path-env)

(defcustom kerl-executable
  (executable-find "kerl")
  "Location of kerl executable."
  :group 'kerl
  :type 'file)

(defcustom kerl-interactive-completion-function
  (if ido-mode 'ido-completing-read 'completing-read)
  "The function which is used by kerl.el to interactively complete user input."
  :group 'kerl
  :type 'function)

(defvar kerl--current-installation nil
  "Current active kerl installation.")

(defvar kerl--current-bin-path nil
  "Path to the currently active kerl installation binaries.")

;;;###autoload
(defun kerl-use (new-erlang)
  "Switch the current version of Erlang to a version installed using kerl as NEW-ERLANG."
  (interactive
   (let ((picked-erlang (kerl--completing-read "Erlang Version: "
                                               (mapcar #'car (kerl-installations)))))
     (list picked-erlang)))
  (kerl--switch kerl--current-installation
                new-erlang))

(defun kerl-deactivate ()
  "Deactivate kerl and revert to the system-installed Erlang."
  (interactive)
  (kerl-use nil))

(defun kerl--switch (old-installation new-installation)
  "Switch environment variables from OLD-INSTALLATION to NEW-INSTALLATION."
  (setq kerl--current-installation new-installation)
  (let ((old-bin kerl--current-bin-path)
        (new-bin (kerl--path new-installation "bin")))
    (kerl--update-path-env "PATH" old-bin new-bin)
    (if old-bin (setq exec-path (remove old-bin exec-path)))
    (if new-bin (add-to-list 'exec-path new-bin))
    (setq eshell-path-env (getenv "PATH"))
    (setq kerl--current-bin-path new-bin))
  new-installation)

(defun kerl--path (installation &optional dir)
  "Return the path to the kerl installation INSTALLATION.
Optionally append the directory DIR."
  (let ((pair (assoc installation (kerl-installations))))
    (if pair
        (let ((path (cdr pair)))
          (if dir
              (concat (file-name-as-directory path) dir)
            path)))))

(defun kerl--update-path-env (env-var old-path new-path)
  "Update a path in the environment variable ENV-VAR from OLD-PATH to NEW-PATH."
  (let ((path-regexp (if old-path
                         (regexp-quote (concat old-path ":"))
                       "^"))
        (replacement-path (if new-path
                      (concat new-path ":")
                      ""))
        (old-value (getenv env-var)))
    (setenv env-var (replace-regexp-in-string
                    path-regexp
                    replacement-path
                    (if old-value old-value "")))))

(defun kerl-installations ()
  "Return an alist of available kerl installations."
  (mapcar (lambda (s)
                 (let ((pair (split-string s)))
                   (cons (car pair) (cadr pair))))
               (split-string (kerl--call-process "list" "installations") "\n" t)))

(defun kerl--completing-read (prompt options)
  "Call the interactive completion function using PROMPT and OPTIONS and return the result trimmed."
  (let ((selected (funcall kerl-interactive-completion-function prompt options)))
    (kerl--string-trim selected)))

(defun kerl--string-trim (string)
  "Trim unwanted whitespace from the ends of STRING."
  (replace-regexp-in-string "^\\s-*\\|\\s-*$" "" string))

(defun kerl--call-process (&rest args)
  "Call the kerl process with arguments ARGS and return the output."
  (with-temp-buffer
    (let* ((success (apply 'call-process kerl-executable nil t nil
                           (delete nil args)))
           (output (buffer-substring-no-properties
                    (point-min) (point-max))))
      (if (= 0 success)
          output))))

(provide 'kerl)
;;; kerl.el ends here
