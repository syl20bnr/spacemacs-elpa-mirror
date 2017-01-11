;;; pcmpl-homebrew.el --- pcomplete for homebrew

;; Copyright (C) 2014, 2015, 2016 hiddenlotus
;; Author: hiddenlotus <kaihaosw@gmail.com>
;; Git: https://github.com/hiddenlotus/pcmpl-homebrew.git
;; Version: 0.97.3
;; Package-Version: 20170110.1609
;; Created: 2014-08-11
;; Keywords: pcomplete, homebrew, tools, cask, services

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

;; Pcomplete for homebrew.
;; Completion for homebrew when using shell or eshell in emacs.

;;; Code:
(require 'pcomplete)

(defun pcmpl-homebrew-get-commands (executable-command shell-command search regex)
  (when (executable-find executable-command)
    (with-temp-buffer
      (call-process-shell-command shell-command nil (current-buffer) nil)
      (goto-char 0)
      (search-forward search)
      (let (commands)
        (while (re-search-forward regex nil t)
          (add-to-list 'commands (match-string 1) t))
        commands))))

(defun pcmpl-homebrew-get-formulas (&rest args)
  (with-temp-buffer
    (apply 'process-file "brew" nil (list t nil) nil args)
    (split-string (buffer-string) nil t)))

(defconst pcmpl-homebrew-commands
  (remove "External"
          (pcmpl-homebrew-get-commands "brew" "brew commands"
                                       "Built-in commands" "^\\([[:word:]-.]+\\)"))
  "List of homebrew commands.")

(defconst pcmpl-homebrew-local-formulas-commands
  '("cleanup" "link" "list" "pin" "reinstall" "unlink" "unpin" "uninstall"
    "upgrade" "test" "--prefix")
  "List of commands that use local formulas.")

(defconst pcmpl-homebrew-global-formulas-commands
  '("audit" "cat" "deps" "edit" "fetch" "home" "info" "install" "log"
    "missing" "reinstall" "search" "uses")
  "List of commands that use global formulas.")

(defmacro pcmpl-homebrew-set-formulas (var command)
  `(progn
     (when (null ,var)
       (setq ,var (funcall ,command)))
     ,var))

(defvar pcmpl-homebrew-installed-formulas '()
  "List of the installed formulas.")

(defun pcmpl-homebrew-installed-formulas ()
  (pcmpl-homebrew-set-formulas
   pcmpl-homebrew-installed-formulas
   (lambda ()
     (pcmpl-homebrew-get-formulas "list"))))

(defvar pcmpl-homebrew-all-formulas '()
  "List of all the formulas.")

(defun pcmpl-homebrew-all-formulas ()
  (pcmpl-homebrew-set-formulas
   pcmpl-homebrew-all-formulas
   (lambda ()
     (pcmpl-homebrew-get-formulas "search"))))

(defconst pcmpl-homebrew-options-hash-table
  (let (options-hash)
    (setq options-hash (make-hash-table :test 'equal))
    (puthash "cleanup" '("--force" "-n" "-s" "-ns") options-hash)
    (puthash "deps" '("--1" "-n" "--union" "--tree" "--all" "--installed") options-hash)
    (puthash "desc" '("-s" "-n" "-d") options-hash)
    (puthash "diy" '("--name=" "--version=") options-hash)
    (puthash "fetch" '("--force" "-v" "--devel" "--HEAD" "--deps"
                       "--build-from-source" "--force-bottle") options-hash)
    (puthash "info" '("--github" "--json=" "--all" "--installed") options-hash)
    (puthash "install" '("--debug" "--env=" "--ignore-dependencies" "--only-dependencies"
                         "--cc=" "--build-from-source" "--devel" "--HEAD"
                         "--interactive" "--git") options-hash)
    (puthash "link" '("--overwrite" "--dry-run" "--force") options-hash)
    (puthash "linkapps" '("--local") options-hash)
    (puthash "list" '("--unbrewed" "--versions" "--multiple" "--pinned") options-hash)
    (puthash "options" '("--compact" "--all" "--installed") options-hash)
    (puthash "outdated" '("--quiet") options-hash)
    (puthash "prune" '("--dry-run" "-d") options-hash)
    (puthash "uninstall" '("--force") options-hash)
    (puthash "search" '("--debian" "--fedora" "--fink" "--macports"
                        "--opensuse" "--ubuntu") options-hash)
    (puthash "sh" '("--env=std") options-hash)
    (puthash "tap" '("--repair") options-hash)
    (puthash "test" '("--devel" "--HEAD") options-hash)
    (puthash "unlinkapps" '("--local") options-hash)
    (puthash "unpack" '("--git" "--patch" "--destdir=") options-hash)
    (puthash "update" '("--rebase") options-hash)
    (puthash "upgrade" '("--debug" "--env=" "--ignore-dependencies" "--only-dependencies"
                         "--cc=" "--build-from-source" "--devel" "--HEAD"
                         "--interactive" "--git" "--all") options-hash)
    (puthash "uses" '("--installed" "--recursive" "--devel" "--HEAD") options-hash)
    options-hash))

(defun pcmpl-homebrew-get-command-options (command)
  (gethash command pcmpl-homebrew-options-hash-table))

;; external commands
;; homebrew/services
(defun pcmpl-external-commands-installed? (tap-name)
  (null (string-match
         "Not installed"
         (shell-command-to-string (format "brew tap-info %s" tap-name)))))

(defconst pcmpl-homebrew-services-commands
  '("cleanup" "list" "restart" "start" "stop")
  "List of homebrew services commands.")

;; caskroom/cask
(defun pcmpl-homebrew-cask-installed? ()
  (pcmpl-external-commands-installed? "caskroom/cask"))

(defvar pcmpl-homebrew-cask-installed? nil)

(when (pcmpl-homebrew-cask-installed?)
  (setq pcmpl-homebrew-cask-installed? t)

  (defconst pcmpl-homebrew-cask-commands
    '("audit" "cat" "cleanup" "create" "doctor" "edit" "fetch" "home" "info" "install"
      "list" "reinstall" "search" "style" "uninstall" "update" "zap")
    "List of homebrew cask commands.")

  (defvar pcmpl-homebrew-cask-all-casks '()
    "List of all casks.")

  (defun pcmpl-homebrew-cask-all-casks ()
    (pcmpl-homebrew-set-formulas
     pcmpl-homebrew-cask-all-casks
     (lambda ()
       (pcmpl-homebrew-get-formulas "cask" "search"))))

  (defvar pcmpl-homebrew-cask-local-casks '()
    "List of local casks.")

  (defun pcmpl-homebrew-cask-local-casks ()
    (pcmpl-homebrew-set-formulas
     pcmpl-homebrew-cask-local-casks
     (lambda ()
       (pcmpl-homebrew-get-formulas "cask" "list")))))


;;;###autoload
(defun pcomplete/brew ()
  (let ((command (nth 1 pcomplete-args)))
    (pcomplete-here* pcmpl-homebrew-commands)
    (while
        (cond
         ((pcomplete-match "^-" 0)
          (pcomplete-here (pcmpl-homebrew-get-command-options command)))
         ((member command pcmpl-homebrew-local-formulas-commands)
          (pcomplete-here (pcmpl-homebrew-installed-formulas)))
         ((member command pcmpl-homebrew-global-formulas-commands)
          (pcomplete-here (pcmpl-homebrew-all-formulas)))
         ((string= command "help")
          (pcomplete-here pcmpl-homebrew-commands))
         ((string= command "services")
          (pcomplete-here pcmpl-homebrew-services-commands))
         ((string= command "cask")
          (when pcmpl-homebrew-cask-installed?
            (let ((subcommand (nth 2 pcomplete-args)))
              (pcomplete-here pcmpl-homebrew-cask-commands)
              (cond ((member subcommand '("fetch" "home" "info"))
                     (pcomplete-here (pcmpl-homebrew-cask-all-casks)))
                    ((string= subcommand "install")
                     (and (pcomplete-match "^-" 0) (pcomplete-here '("--force")))
                     (while (pcomplete-here (pcmpl-homebrew-cask-all-casks))))
                    ((member subcommand '("uninstall" "reinstall"))
                     (and (pcomplete-match "^-" 0) (pcomplete-here '("--force")))
                     (while (pcomplete-here (pcmpl-homebrew-cask-local-casks))))))))))))

(provide 'pcmpl-homebrew)

;;; pcmpl-homebrew.el ends here
