;;; composer.el --- Utillities for PHP composer -*- lexical-binding: t -*-

;; Copyright (C) 2015 USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 5 Dec 2015
;; Version: 0.0.1
;; Package-Version: 0.0.2
;; Keywords: php dependency manager
;; Package-Requires: ((emacs "24") (s "1.9.0") (f "0.17") (request "0.2.0"))

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

;;; Commentary:

;;; Code:
(require 's)
(require 'f)
(require 'request)

(defvar composer-executable-bin nil
  "Path to `composer.phar' exec file.")

(defvar composer--async-use-compilation t)

(defcustom composer-use-ansi-color nil
  "Use ansi color code on execute `composer' command.")

(defun composer--find-executable ()
  "Return `composer' command name."
  (if (and composer-executable-bin (file-exists-p composer-executable-bin))
      composer-executable-bin
    (catch 'found
      (dolist (cmd '("composer.phar" "composer"))
        (when (executable-find cmd)
          (throw 'found cmd)))
      ;; ToDo: Returns project local binary file
      )))

(defun composer--find-composer-root (directory)
  "Return directory path which include composer.json by `DIRECTORY'."
  (let ((composer-json (f-join directory "composer.json")) parent)
    (if (file-exists-p composer-json)
        (concat (f-dirname composer-json) "/")
      (setq parent (f-dirname directory))
      (if (null parent)
          nil
        (composer--find-composer-root (f-dirname directory))))))

(defun composer--make-command-string (sub-command args)
  "Return command string by `SUB-COMMAND' and `ARGS'."
  (s-join " " (cons (composer--find-executable)
                    (cons sub-command (composer--args-with-global-options args)))))

(defun composer--args-with-global-options (args)
  "Set global options to `ARGS'."
  (unless composer-use-ansi-color
    (setq args (push "--no-ansi" args)))
  args)

(defun composer--command-async-execute (sub-command &rest args)
  "Asynchronous execute `composer.phar' command SUB-COMMAND by ARGS."
  (let ((default-directory (or (composer--find-composer-root default-directory)
                               default-directory)))
    (if composer--async-use-compilation
        (compile (composer--make-command-string sub-command args))
      (async-shell-command (composer--make-command-string sub-command args)))))

(defun composer--command-execute (sub-command &rest args)
  "Execute `composer.phar' command SUB-COMMAND by ARGS."
  ;; You are running composer with xdebug enabled. This has a major impact on runtime performance. See https://getcomposer.org/xdebug
  (let ((default-directory (or (composer--find-composer-root default-directory)
                               default-directory)))
    (replace-regexp-in-string
     "^.+getcomposer.org/xdebug\n" ""
     (s-chomp
      (shell-command-to-string (composer--make-command-string sub-command args))))))


;; (composer--command-async-execute "require" "--dev" "phpunit/phpunit:^4.8")
;; (composer--command-async-execute "update")
;; (let ((composer--async-use-compilation nil)) (composer--command-execute "update"))
;; (composer--command-execute "update")

;;;###autoload
(defun composer-install ()
  "Execute `composer.phar install' command."
  (interactive)
  (composer--command-async-execute "install"))

;;;###autoload
(defun composer-require (is-dev &optional package)
  "Execute `composer.phar require (--dev)' command.  Add --dev option if `IS-DEV' is t.  Require `PACKAGE' is package name."
  (interactive "p")
  (when (called-interactively-p 'interactive)
    (setq is-dev (not (eq is-dev 1)))
    (setq package (read-string
                   (if is-dev "Input package name(dev): " "Input package name: "))))
  (unless package
    (error "A argument `PACKAGE' is required"))
  (let ((args (list package)))
    (when is-dev (push "--dev" args))
    (apply 'composer--command-async-execute "require" args)))

;;;###autoload
(defun composer-find-json-file ()
  "Open composer.json of the project."
  (interactive)
  (find-file (f-join (composer--find-composer-root default-directory) "composer.json")))

;;;###autoload
(defun composer-view-lock-file ()
  "Open composer.lock of the project."
  (interactive)
  (find-file (f-join (composer--find-composer-root default-directory) "composer.lock"))
  (read-only-mode))

;;;###autoload
(defun composer-self-update ()
  "Execute `composer.phar self-update' command."
  (interactive)
  (when (yes-or-no-p "Do composer self-update? ")
    (composer--command-async-execute "self-update")))

(provide 'composer)
;;; composer.el ends here
