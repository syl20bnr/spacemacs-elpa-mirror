;;; direnv.el --- Load environment variables from direnv

;; Copyright (C) 2016 Jonathan M. Lange
;;
;; ;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Jonathan M. Lange <jml@mumak.net>
;; Version: 0.1
;; Package-Version: 20161002.715

;;; Commentary:

;; direnv <http://direnv.net/> is an environment switcher for the shell.  Users
;; define a .envrc file in their directories, and when they change into those
;; directories,  direnv loads those environment variables.  When they change
;; out, direnv unloads them.
;;
;; This is particularly useful for defining project-specific environment
;; variables.
;;
;; This module integrates direnv with Emacs.
;;
;; The main function is 'direnv-load-environment', which exports the
;; environment difference from direnv and sets in globally within Emacs,
;; making the direnv environment settings available to any subprocess.  When
;; you change to a different buffer, the environment variable changes are
;; reverted, thanks to the magic of direnv.
;;
;; One way to use this is as a hook in find-file and buffer-list-update-hook:
;;
;;    (add-hook 'find-file-hook 'direnv-load-environment)
;;    (add-hook 'buffer-list-update-hook 'direnv-load-environment)

;;; Code:

(require 'json)

(defvar direnv--loading nil
  "True if direnv is in the middle of loading environment variables.")

(defun direnv--call-json-process (program &rest args)
  "Execute PROGRAM with ARGS, parsing stdout as JSON."
  (with-temp-buffer
    (let ((status (apply 'call-process program nil '(t nil) nil args)))
      (unless (eq status 0)
        (error "%s exited with status %s" program status))
      (if (= 0 (buffer-size))
          nil
        (progn
          (goto-char (point-min))
          (json-read))))))

(defun direnv-export (directory)
  "Export direnv settings in DIRECTORY as list of pairs.
Pairs are (var-name . var-value)"
  (let ((default-directory directory))
    (direnv--call-json-process "direnv" "export" "json")))

(defun direnv--update-environment (env-vars)
  "Set ENV-VARS on 'process-environment'.

ENV-VARS is a list of pairs of environment variables and their
values."
  (let ((new-vars (mapcar 'direnv--format-env-var env-vars)))
    (setq process-environment (nconc new-vars process-environment))))

(defun direnv--format-env-var (env-var)
  "Format ENV-VAR for 'process-environment'.

e.g.
  (direnv--format-env-var (\"foo\" . \"bar\")) ==> \"foo=bar\""
  (mapconcat 'identity (list (car env-var) (cdr env-var)) "="))

;;;###autoload
(defun direnv-load-environment (&optional file-name)
  "Load the direnv environment for FILE-NAME.
If FILE-NAME not provided, default to the current buffer."
  (interactive)
  (unless (not direnv--loading)
    (let ((direnv--loading t)
          (fn (if file-name file-name buffer-file-name)))
      (when fn
        (let ((direnv--loading t)
              (json-key-type 'string)
              (new-vars (direnv-export (file-name-directory fn))))
          (direnv--update-environment new-vars))))))

(provide 'direnv)

;;; direnv.el ends here
