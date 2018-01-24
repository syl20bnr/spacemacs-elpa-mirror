;;; relative-buffers.el --- Emacs buffers naming convention

;; Copyright (C) 2014-2016 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/relative-buffers
;; Package-Version: 20160221.1123
;; Version: 0.0.1
;; Package-Requires: ((cl-lib "0.5") (dash "2.6.0") (s "1.9.0") (f "0.16.2"))

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

;; This is a tiny package that will rename your buffers according to project
;; structure.  For python buffers that will be whole module name.  For temporary
;; files and directories that will be relative path from project root.

;;; Usage:

;; See the README for more details.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'f)

(defgroup relative-buffers nil
  "Emacs buffers naming convention."
  :group 'convenience)

(defcustom relative-buffers-project-markers '(".git" ".hg")
  "List of files marked its directory as project root."
  :group 'relative-buffers
  :type '(repeat string))

;;;###autoload
(define-minor-mode relative-buffers-mode
  "Name your buffer relatively to project.
This mode support a lot of naming conventions.
Python buffer named as a module relative to package.
Project file and directories named relative to project root directory."
  :lighter ""
  (when relative-buffers-mode
    (let ((newname (relative-buffers-name)))
      (and newname (rename-buffer newname t)))))

;;;###autoload
(define-globalized-minor-mode global-relative-buffers-mode
  relative-buffers-mode
  relative-buffers-mode)

(defun relative-buffers-name ()
  "Give current buffer a relative name."
  (let ((path (or (buffer-file-name) dired-directory)))
    (cl-case major-mode
      (python-mode (relative-buffers-python-package path))
      (dired-mode (relative-buffers-directory path))
      (otherwise (relative-buffers-file-name path)))))

(defun relative-buffers-python-package (file)
  "Python module relative to package.
FILE must be absolute python module file name."
  (when file
    (let* ((file-path (f-full file))
           (init (f-join (f-dirname file-path) "__init__.py"))
           (module (f-base (f-filename file-path)))
           namespace parent)
      (and (f-exists? init)
           (not (s-equals? module "__init__"))
           (push module namespace))
      (while (file-exists-p init)
        (setq parent (f-filename (f-dirname init)))
        (setq init (f-join (f-dirname (f-dirname init)) "__init__.py"))
        (push parent namespace))
      (when namespace
        (s-join "." namespace)))))

(defun relative-buffers-directory (directory)
  "Directory relative to project root.
DIRECTORY must be specified as absolute path."
  (let ((root (relative-buffers-project-root directory))
        (directory-path (f-full directory)))
    (when (and root (f-ancestor-of? root directory-path))
      (s-chop-prefix root directory-path))))

(defun relative-buffers-file-name (file)
  "File name relative to project root.
FILE must be specified as absolute path."
  (when file
    (let ((file-path (f-full file)))
      (--when-let (relative-buffers-project-root file-path)
        (s-chop-prefix it file-path)))))

(defun relative-buffers-project-root (path)
  "Return project root for PATH in different VCS."
  (let* ((markers relative-buffers-project-markers)
         (projects (--map (locate-dominating-file path it) markers))
         (roots (-remove 'null projects))
         (dipper-roots (-sort 'f-descendant-of? roots))
         (root (car dipper-roots)))
    (and root (f-full root))))

(provide 'relative-buffers)

;;; relative-buffers.el ends here
