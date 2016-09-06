;;; composer.el --- Interface to PHP Composer -*- lexical-binding: t -*-

;; Copyright (C) 2015 USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 5 Dec 2015
;; Version: 0.0.6
;; Package-Version: 0.0.6
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

;; `composer.el' is PHP Composer interface for Emacs.
;;
;; ## Commands
;;
;;  - M-x composer  - Run composer sub command (with completing read)
;;  - C-u M-x composer  - Run composer (global) sub command (with completing read)
;;  - M-x composer-install  - Run composer install command
;;  - M-x composer-require  - Run composer require command
;;  - C-u M-x composer-require  - Run composer require --dev command
;;  - M-x composer-dump-autoload - Run composer dump-autoload command
;;  - M-x composer-find-json-file  - Open composer.json of the project
;;  - M-x composer-view-lock-file  - Open composer.lock of the project (as read-only)

;;; Code:
(require 's)
(require 'f)
(require 'request)

(defvar composer-executable-bin nil
  "Path to `composer.phar' exec file.")

(defvar composer--async-use-compilation t)

(defvar composer--execute-interactive nil)

(defvar composer--quote-shell-argument t)

(defvar composer-global-command nil
  "Execute composer global command when `composer-global-command' is t.")

(defgroup composer nil
  "Interface to PHP Composer."
  :group 'external
  :tag "PHP Composer"
  :prefix "composer-")

(defcustom composer-use-ansi-color nil
  "Use ansi color code on execute `composer' command."
  :type 'boolean)

(defcustom composer-interactive-sub-commands
  '("remove" "search")
  "List of sub commands of interactive execution."
  :type '(repeat string))

(defun composer--find-executable ()
  "Return `composer' command name."
  (if (and composer-executable-bin (file-exists-p composer-executable-bin))
      composer-executable-bin
    (catch 'found
      (dolist (cmd '("composer" "composer.phar"))
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
  (s-join " "
          (mapcar
           (if composer--quote-shell-argument 'shell-quote-argument 'identity)
           (cons (composer--find-executable)
                 (append (if composer-global-command '("global") nil)
                         (list sub-command)
                         (if composer--execute-interactive nil '("--no-interaction"))
                         (composer--args-with-global-options args))))))

(defun composer--args-with-global-options (args)
  "Set global options to `ARGS'."
  (unless composer-use-ansi-color
    (setq args (push "--no-ansi" args)))
  args)

(defun composer--parse-json ()
  "Parse `composer.json'."
  (json-read-file (f-join (composer--find-composer-root default-directory) "composer.json")))

(defun composer--get-vendor-bin-dir ()
  "Return path to project bin dir."
  (let ((config (composer--parse-json)))
    (or
     (cdr-safe (assq 'bin-dir (cdr-safe (assq 'config config))))
     "vendor/bin")))

(defun composer--get-vendor-bin-files ()
  "Return executable file names of `vendor/bin' dir."
  (let* ((default-directory (or (composer--find-composer-root default-directory)
                                default-directory))
         (bin-dir (composer--get-vendor-bin-dir)))
    (if (null bin-dir)
        nil
      (directory-files (f-join default-directory bin-dir) nil "\\`[^.]"))))

(defun composer--get-vendor-bin-path (command)
  "Return executable file path by `COMMAND'."
  (let* ((default-directory (or (composer--find-composer-root default-directory)
                                default-directory))
         (bin-dir (composer--get-vendor-bin-dir))
         (command-path (if (and bin-dir command) (f-join bin-dir command) nil)))
    (if (not (and command-path (file-executable-p command-path)))
        (error "%s command is not exists" command)
      command-path)))

(defun composer--command-async-execute (sub-command &rest args)
  "Asynchronous execute `composer.phar' command SUB-COMMAND by ARGS."
  (let ((default-directory (or (composer--find-composer-root default-directory)
                               default-directory)))
    (if composer--async-use-compilation
        (compile (composer--make-command-string sub-command args))
      (async-shell-command (composer--make-command-string sub-command args) nil nil))))

(defun composer--command-execute (sub-command &rest args)
  "Execute `composer.phar' command SUB-COMMAND by ARGS."
  ;; You are running composer with xdebug enabled. This has a major impact on runtime performance. See https://getcomposer.org/xdebug
  (let ((default-directory (or (composer--find-composer-root default-directory)
                               default-directory)))
    (if composer--execute-interactive
        (compile (composer--make-command-string sub-command args) t)
      (replace-regexp-in-string
       "^.+getcomposer.org/xdebug\n" ""
       (s-chomp
        (shell-command-to-string (composer--make-command-string sub-command args)))))))

(defun composer--list-sub-commands ()
  "List `composer' sub commands."
  (let ((output (composer--command-execute "list")))
    (mapcar (lambda (line) (car (s-split-words line)))
            (s-split "\n" (cadr (s-split "Available commands:\n" output))))))

;;;###autoload
(defun composer-get-config (name)
  "Return config value by `NAME'."
  (let ((output (s-lines (composer--command-execute "config" name))))
    (if (eq 1 (length output)) (car output) nil)))

;; (composer--command-async-execute "require" "--dev" "phpunit/phpunit:^4.8")
;; (composer--command-async-execute "update")
;; (let ((composer--async-use-compilation nil)) (composer--command-execute "update"))
;; (composer--command-execute "update")
;; (composer-get-config "bin-dir")
;; (let ((composer-global-command t)) (composer-get-config "bin-dir"))
;; (composer--make-command-string "hoge" '("fuga"))

;;;###autoload
(defun composer-install ()
  "Execute `composer.phar install' command."
  (interactive)
  (composer--command-async-execute "install"))

;;;###autoload
(defun composer-dump-autoload ()
  "Execute `composer.phar install' command."
  (interactive)
  (let ((composer--async-use-compilation nil))
    (composer--command-async-execute "dump-autoload")))

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
  (view-mode))

;;;###autoload
(defun composer-run-vendor-bin-command (command)
  "Run command `COMMAND' in `vendor/bin' of the composer project."
  (interactive (list (completing-read "Run command in vendor/bin: " (composer--get-vendor-bin-files))))
  (let ((default-directory (or (composer--find-composer-root default-directory)
                               default-directory))
        (command-path (composer--get-vendor-bin-path command)))
    (if command-path
        (compile command-path)
      (error "`%s' is not executable file" command))))

;;;###autoload
(defun composer-self-update ()
  "Execute `composer.phar self-update' command."
  (interactive)
  (when (yes-or-no-p "Do composer self-update? ")
    (composer--command-async-execute "self-update")))

(make-obsolete 'composer-self-update 'composer)

;;;###autoload
(defun composer (global &optional sub-command option)
  "Execute `composer.phar'.  Execute `global' sub command If GLOBAL is t.  Require SUB-COMMAND is composer sub command.  OPTION is optional commandline arguments."
  (interactive "p")
  (when (called-interactively-p 'interactive)
    (setq global (not (eq global 1)))
    (setq sub-command (completing-read
                       (if global "Composer (global) sub command: " "Composer sub command: ")
                       (composer--list-sub-commands)))
    (setq option (read-string (format "Input `composer %s' argument: " sub-command))))
  (unless sub-command
    (error "A argument `SUB-COMMAND' is required"))
  (let ((composer--quote-shell-argument nil)
        (composer-global-command global)
        (composer--execute-interactive (member sub-command composer-interactive-sub-commands)))
    (apply (if composer--execute-interactive 'composer--command-execute 'composer--command-async-execute)
           sub-command (list option))))

(provide 'composer)
;;; composer.el ends here
