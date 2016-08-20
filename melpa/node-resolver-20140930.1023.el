;;; node-resolver.el --- hook to install node modules in background

;; Copyright © 2014 Dave Justice

;; Author: Dave Justice
;; URL: https://github.com/meandavejustice/node-resolver.el
;; Package-Version: 20140930.1023
;; Version: 0.1.0
;; Created: 2014-09-29
;; Keywords: convenience, nodejs, javascript, npm
;; Package-Requires: ((cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package provides a way to start a background process from
;; emacs to install node_modules based on require statements found
;; on file save events.

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

;;; Code:

(require 'vc)
(require 'cl-lib)

(defvar *node-resolver-project-root* nil
  "Used internally to cache the project root.")
(make-variable-buffer-local '*node-resolver-project-root*)

(defvar node-resolver-active-projects ()
  "List of active projects.")

(defvar node-resolver-project-roots
  '("Rakefile" "Makefile" "README" "README.md" "build.xml" ".emacs-project"
    ".emacs-project" "node_modules" "package.json" "LICENSE" "bower.json")
  "The presence of any file/directory in this list indicates a project root.")

(defun node-resolver-find-project-root ()
  "Determines the current project root by recursively searching for an indicator."
  (when default-directory
    (locate-dominating-file default-directory
                                (lambda (dir)
                                  (cl-intersection (directory-files dir)
                                                   node-resolver-project-roots
                                                   :test 'string=)))))

(defun node-resolver-get-project-root ()
  "Returns the current project root."
  (or *node-resolver-project-root*
      (setq *node-resolver-project-root* (node-resolver-find-project-root))))

(defun node-resolver-start-process (dir)
  (start-process-shell-command "node-resolver-process" nil (format "cd %s && node-resolver ." dir)))

(defun node-resolver-start ()
  (if (not (member (node-resolver-get-project-root) node-resolver-active-projects))
      (and (node-resolver-start-process (node-resolver-get-project-root))
           (push (node-resolver-get-project-root) node-resolver-active-projects))))

(provide 'node-resolver)
;;; node-resolver.el ends here
