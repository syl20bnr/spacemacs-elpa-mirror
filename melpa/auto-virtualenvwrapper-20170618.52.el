;;; auto-virtualenvwrapper.el --- Lightweight auto activate python virtualenvs

;; Copyright (C) 2017 Marcwebbie, Robert Zaremba

;; Author: Marcwebbie <marcwebbie@gmail.com>
;;         Robert Zaremba <robert-zaremba@scale-it.pl>
;; Version: 1.0
;; Package-Version: 20170618.52
;; Package-X-Original-Version: 20170518
;; Keywords: Python, Virtualenv, Tools
;; Package-Requires: ((cl-lib "0.6") (s "1.10.0") (virtualenvwrapper "0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Auto virtualenvwrapper activates virtualenv automatically when called.
;; To use auto-virtualenvwrapper set hooks for `auto-virtualenvwrapper-activate'

;; For example:
;; (require 'auto-virtualenvwrapper)
;; (add-hook 'python-mode-hook #'auto-virtualenvwrapper-activate)
;; (add-hook 'projectile-after-switch-project-hook #'auto-virtualenvwrapper-activate)

;;; Code:

(require 'cl-lib)
(require 'vc)
(require 'python)
(require 'virtualenvwrapper)
(require 's)

(declare-function projectile-project-name "projectile")
(declare-function projectile-project-root "projectile")

(defun auto-virtualenvwrapper-first-file-exists-p (filelist)
  "Select first file from the FILELIST which exists."
  (cl-loop for filename in (mapcar #'expand-file-name filelist)
           when (file-exists-p filename)
           return filename))

(defcustom auto-virtualenvwrapper-dir (auto-virtualenvwrapper-first-file-exists-p '("~/.virtualenvs" "~/.pyenv/versions"))
  "The intended virtualenvs installation directory."
  :type 'directory
  :safe #'stringp
  :group 'auto-virtualenvwrapper)

(defvar auto-virtualenvwrapper-project-root-files
  '(".python-version" ".dir-locals.el" ".projectile" ".emacs-project" ".workon")
  "The presence of any file/directory in this list indicates a project root.")

(defvar auto-virtualenvwrapper-verbose t
  "Verbose output on activation.")

(defvar auto-virtualenvwrapper--path nil
  "Used internally to cache the current virtualenv path.")
(make-variable-buffer-local 'auto-virtualenvwrapper--path)

(defvar auto-virtualenvwrapper--project-root nil
  "Used internally to cache the project root.")
(make-variable-buffer-local 'auto-virtualenvwrapper--project-root)

(defun auto-virtualenvwrapper-message (msg &rest rest)
  "Prints the MSG REST in the message area."
  (when auto-virtualenvwrapper-verbose
    (apply #'message (concat "[auto-virtualenvwrapper] " msg) rest)))

(defun auto-virtualenvwrapper--project-root-projectile ()
  "Return projectile root if projectile is available."
  (when (and (boundp 'projectile-project-root) (not (equal (projectile-project-name) "-")))
    (projectile-project-root)))

(defun auto-virtualenvwrapper--project-root-vc ()
  "Return vc root if file is in version control."
  (when (or
         (vc-find-root (or (buffer-file-name) "") ".git")
         (vc-find-root (or (buffer-file-name) "") ".hg"))))


(defun auto-virtualenvwrapper--project-root-traverse ()
  "Tranvese parent directories looking for files in `auto-virtualenvwrapper-project-root-files' that indicates a root directory."
  (let ((dominating-file (locate-dominating-file default-directory
                           (lambda (dir)
                             (cl-intersection
                              auto-virtualenvwrapper-project-root-files
                              (directory-files dir)
                              :test 'string-equal)))))
    (when dominating-file
      (expand-file-name dominating-file))))

(defun auto-virtualenvwrapper--project-root ()
  "Return the current project root directory."
  (or auto-virtualenvwrapper--project-root
      (setq auto-virtualenvwrapper--project-root
            (or (auto-virtualenvwrapper--project-root-projectile)
                (auto-virtualenvwrapper--project-root-vc)
                (auto-virtualenvwrapper--project-root-traverse)
                "")))
  (when (eq auto-virtualenvwrapper--project-root "")
      (auto-virtualenvwrapper-message "Can't find project root"))
  auto-virtualenvwrapper--project-root)

(defun auto-virtualenvwrapper--project-name ()
  "Return the project project root name."
  (file-name-nondirectory
   (directory-file-name
    (or (file-name-directory (auto-virtualenvwrapper--project-root)) ""))))

(defun auto-virtualenvwrapper--versions ()
  "Get list of available virtualenv names."
  (if (and auto-virtualenvwrapper-dir (file-exists-p (expand-file-name auto-virtualenvwrapper-dir)))
      (directory-files (expand-file-name auto-virtualenvwrapper-dir))))

(defun auto-virtualenvwrapper-expandpath (path)
  (expand-file-name path auto-virtualenvwrapper-dir))

(defun auto-virtualenvwrapper-find-virtualenv-path ()
  "Get current buffer-file possible virtualenv name.
1. Try name from .python-version or .workon file if it exists
2. Try .venv dir in the root of project
3. Try venv dir in the root of project
4. Try find a virtualenv with the same name of Project Root.
Project root name is found using `auto-virtualenvwrapper--project-root'"
  (let ((python-version-file (expand-file-name ".python-version" (auto-virtualenvwrapper--project-root)))
        (workon-file (expand-file-name ".workon" (auto-virtualenvwrapper--project-root)))
        (dot-venv-dir (expand-file-name ".venv/" (auto-virtualenvwrapper--project-root)))
        (venv-dir (expand-file-name "venv/" (auto-virtualenvwrapper--project-root))))
    (cond
     ;; 1.1 Try name from .python-version file if it exists
     ((file-exists-p python-version-file)
      (auto-virtualenvwrapper-message "using virtualenv from .python-version")
      (auto-virtualenvwrapper-expandpath
       (with-temp-buffer
         (insert-file-contents python-version-file) (s-trim (buffer-string)))))
     ;; 1.2 Try name from .workon file if it exists
     ((file-exists-p workon-file)
      (auto-virtualenvwrapper-message "using virtualenv from .workon")
      (auto-virtualenvwrapper-expandpath
       (with-temp-buffer
         (insert-file-contents workon-file) (s-trim (buffer-string)))))
     ;; 2. Try .venv dir in the root of project
     ((file-exists-p dot-venv-dir)
      (auto-virtualenvwrapper-message "using virtualenv from .venv directory")
      dot-venv-dir)
     ;; 3. Try .venv dir in the root of project
     ((file-exists-p venv-dir)
      (auto-virtualenvwrapper-message "using virtualenv from venv directory")
      venv-dir)
     ;; 4. Try find a virtualenv with the same name of Project Root.
     ((and (auto-virtualenvwrapper--versions) (member (auto-virtualenvwrapper--project-name) (auto-virtualenvwrapper--versions)))
      (auto-virtualenvwrapper-message "using virtualenv based on the root directory name")
      (auto-virtualenvwrapper-expandpath (auto-virtualenvwrapper--project-name))))))

;;;###autoload
(defun auto-virtualenvwrapper-activate ()
  "Activate virtualenv for buffer-filename."
  (let ((path (auto-virtualenvwrapper-find-virtualenv-path)))
    (when (and path (not (equal path auto-virtualenvwrapper--path)))
      (setq auto-virtualenvwrapper--path path
            venv-current-name (file-name-base (file-truename path)))
      (venv--activate-dir auto-virtualenvwrapper--path)
      (auto-virtualenvwrapper-message "activated virtualenv: %s" path))))

(provide 'auto-virtualenvwrapper)

;;; auto-virtualenvwrapper.el ends here
