;;; helm-perspeen.el --- helm extension for perspeen.el

;; Copyright (C) 2017 jimo1001 <jimo1001@gmail.com>
;;
;; Author: Yoshinobu Fujimoto
;; Version: 0.1.0
;; Package-Version: 0.1.0
;; URL: https://github.com/jimo1001/helm-perspeen
;; Created: 2017-01-30
;; Package-Requires: ((perspeen "0.1.0") (helm-projectile "2.5.1"))
;; Keywords: projects, lisp

;; Dependencies:
;; - perspeen
;;    - https://github.com/seudut/perspeen/blob/master/perspeen
;; - helm:
;;   - https://emacs-helm.github.io/helm/
;; - (Optional) helm-projectile
;;   - https://github.com/bbatsov/helm-projectile
;;

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
;; Command:
;;   - M-x helm-perspeen
;; Helm Sources:
;;   - helm-source-perspeen-tabs
;;   - helm-source-perspeen-workspaces
;;   - helm-source-perspeen-create-workspace
;;

;;; Code:



(require 'helm)
(require 'perspeen)


(defvar helm-source-perspeen-tabs
  (helm-build-sync-source "Tabs (perspeen)"
    :candidates
    (lambda ()
      (if perspeen-tab-configurations
          (let ((index -1) (buffer nil))
            (mapcar (lambda (tab)
                      (setq index (+ index 1))
                      (setq buffer (get tab 'current-buffer))
                      (cons (format "%s" (buffer-name buffer)) index))
                    (perspeen-tab-get-tabs)))
        nil))
    :action
    (lambda (index) (perspeen-tab-switch-internal index) nil))
  "The helm source which are perspeen's tabs in the current workspace.")

(defun helm-perspeen--switch-workspace (ws)
  "Switch to another workspace.
Save the old windows configuration and restore the new configuration.
Argument WS the workspace to swith to."
  (perspeen-switch-ws-internal ws)
  (perspeen-update-mode-string)
  nil)

(defvar helm-source-perspeen-workspaces
  (helm-build-sync-source "WorkSpaces (perspeen)"
    :candidates
    (lambda ()
      (mapcar (lambda (ws)
                (let ((name (perspeen-ws-struct-name ws))
                      (root-dir (perspeen-ws-struct-root-dir ws)))
                  (cons (format "%s (%s)" name root-dir) ws)))
              perspeen-ws-list))
    :action
    'helm-perspeen--switch-workspace)
  "The workspaces helm source for perspeen.el.")

(defun helm-perspeen-create-projectile-workspace (dir)
  "Create new workspace with project directory.
DIR is project root directory."
  (interactive)
  (perspeen-create-ws)
  (perspeen-change-root-dir dir))

(defvar helm-source-perspeen-create-workspace
  (helm-build-dummy-source
      "Create perspeen workspace"
    :action (helm-make-actions
             "Create Workspace (perspeen)"
             (lambda (candidate)
               (perspeen-create-ws)
               (perspeen-rename-ws candidate) nil))))

;;;###autoload
(when (require 'helm-projectile nil t)
  (with-eval-after-load 'helm-projectile
    (setq helm-source-projectile-projects-actions
          (append helm-source-projectile-projects-actions
                  '(("Create WorkSpace (perspeen)" . helm-perspeen-create-projectile-workspace))))))

;;;###autoload
(defun helm-perspeen ()
  "Display workspaces (perspeen.el) with helm interface."
  (interactive
   (helm '(helm-source-perspeen-tabs helm-source-perspeen-workspaces helm-source-perspeen-create-workspace))))

(provide 'helm-perspeen)

;;; helm-perspeen.el ends here
