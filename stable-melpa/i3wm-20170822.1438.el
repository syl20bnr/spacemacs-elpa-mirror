;;; i3wm.el --- i3wm integration library

;; Copyright (C) 2016 Samuel Flint

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 0.5
;; Package-Version: 20170822.1438
;; Keywords: convenience, extensions
;; URL: https://git.flintfam.org/swf-projects/emacs-i3
;; License: GNU General Public License version 3, or (at your option) any later version

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This provides an interface to the i3 window manager, with a few
;; pre-built accessors to the various i3 commands.



;;; Code:

(require 'json)

;;; Primitive functions:

(defun i3wm-command (command &rest arguments)
  "Execute the given COMMAND with the given ARGUMENTS."
  (json-read-from-string
   (shell-command-to-string
    (format "i3-msg %s" (shell-quote-argument (apply #'format command arguments))))))

(defun i3wm-get-workspaces ()
  "List all workspaces."
  (json-read-from-string
   (shell-command-to-string "i3-msg -t get_workspaces")))

(defun i3wm-get-outputs ()
  "List all outputs."
  (json-read-from-string
   (shell-command-to-string "i3-msg -t get_outputs")))

(defun i3wm-get-version ()
  "Retrieve i3 version."
  (json-read-from-string
   (shell-command-to-string "i3-msg -t get_version")))

;;; i3 commands

(defun i3wm-exec (program)
  "Execute the given PROGRAM."
  (i3wm-command "exec %s" program))

(defun i3wm-exec-no-startup-id (program)
  "Execute the given PROGRAM with --no-startup-id."
  (i3wm-command "exec --no-startup-id %s" program))

(defun i3wm-workspace (workspace)
  "Switch to the given i3 WORKSPACE."
  (i3wm-command "workspace %s" workspace))

(defun i3wm-workspace-numbered (number)
  "Switch to the workspace with the given NUMBER."
  (i3wm-command "workspace number %d" number))

(defun i3wm-workspace-next ()
  "Switch to the next workspace."
  (i3wm-command "workspace next"))

(defun i3wm-workspace-prev ()
  "Switch to the previous workspace."
  (i3wm-command "workspace prev"))

(defun i3wm-split-horizontally ()
  "Split the current container horizontally."
  (i3wm-command "split h"))

(defun i3wm-split-vertically ()
  "Split the current container vertically."
  (i3wm-command "split v"))

(defun i3wm-stacking ()
  "Switch to a stacking layout."
  (i3wm-command "layout stacking"))

(defun i3wm-tabbed ()
  "Switch to a tabbed layout."
  (i3wm-command "layout tabbed"))

(defun i3wm-fullscreen ()
  "Switch to a fullscreen layout."
  (i3wm-command "fullscreen toggle"))

(defun i3wm-floating ()
  "Toggle container floating."
  (i3wm-command  "floating toggle"))

;; Interactive commands

;;;###autoload
(defun i3wm-switch-to-workspace (workspace)
  "Prompt for and switch to a WORKSPACE."
  (interactive (list (completing-read "Workspace Name: "
                                      (mapcar (lambda (workspace)
                                                (cdr (assoc 'name workspace)))
                                              (i3wm-get-workspaces)))))
  (i3wm-workspace workspace))

;;;###autoload
(defun i3wm-switch-to-workspace-number (number)
  "Prompt for and switch to the workspace numbered NUMBER."
  (interactive (list (read-number "Workspace Number: ")))
  (i3wm-workspace-numbered number))

(provide 'i3wm)

;;; i3wm.el ends here
