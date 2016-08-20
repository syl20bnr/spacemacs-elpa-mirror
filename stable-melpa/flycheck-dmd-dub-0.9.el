;;; flycheck-dmd-dub.el --- Sets flycheck-dmd-include-paths from dub package information

;; Copyright (C) 2014 Atila Neves

;; Author:  Atila Neves <atila.neves@gmail.com>
;; Version: 0.8
;; Package-Version: 0.9
;; Package-Requires: ((flycheck "0.24"))
;; Keywords: languages
;; URL: http://github.com/atilaneves/flycheck-dmd-dub

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

;; This package uses "dub describe" to obtain a list of all
;; dependencies of a D dub project and sets the variable flycheck-dmd-include-paths
;; so that flycheck syntax checking knows how to call the compiler
;; and pass it include flags to find the dependencies

;; If it's not clear what any one function does, consult the unit tests
;; in the tests directory.

;; Usage:
;;
;;      (add-hook 'd-mode-hook 'flycheck-dmd-dub-set-include-path)

;;; Code:

(require 'json)
(require 'flycheck)


(defun fldd--dub-pkg-version-to-suffix (version)
  "From dub dependency to suffix for the package directory.
VERSION is what follows the colon in a dub.json file such as
'~master' or '>=1.2.3' and returns the suffix to compose the
directory name with."
  (cond
   ((equal version "~master") "-master") ; e.g. "cerealed": "~master" -> cerealed-master
   ((equal (substring version 1 2) "=") (concat "-" (substring version 2))) ;>= or ==
   (t nil)))



(defun fldd--dub-pkgs-dir ()
  "Return the directory where dub packages are found."
  (if (eq system-type 'windows-nt)
      (concat (getenv "APPDATA") "\\dub\\packages\\")
    "~/.dub/packages/"))


(defun fldd--dub-pkg-to-dir-name (pkg)
  "Return the directory name for a dub package dependency.
PKG is a package name such as 'cerealed': '~master'."
  (let ((pkg-name (car pkg))
        (pkg-suffix (fldd--dub-pkg-version-to-suffix (cdr pkg))))
    (concat (fldd--dub-pkgs-dir) pkg-name pkg-suffix)))

(defun fldd--pkg-to-path-key (pkg key)
  "Take a PKG assoc list and return the value for KEY."
  (let ((import-paths (cdr (assq key pkg)))
        (path (cdr (assq 'path pkg))))
    (mapcar (lambda (p) (expand-file-name p path)) import-paths)))

(defun fldd--pkg-to-dir-names (pkg)
  "Return a directory name for the assoc list PKG."
  (fldd--pkg-to-path-key pkg 'importPaths))

(defun fldd--pkg-to-string-import-paths (pkg)
  "Return a directory name for the assoc list PKG."
  (fldd--pkg-to-path-key pkg 'stringImportPaths))


(defun fldd--flatten(x)
  (cond ((null x) nil)
        ((listp x) (append (fldd--flatten (car x)) (fldd--flatten (cdr x))))
        (t (list x))))


(defun fldd--pkgs-to-dir-names (pkgs)
  "Return a list of dir names for assoc list PKGS."
  (fldd--flatten (mapcar #'fldd--pkg-to-dir-names (cdr pkgs))))

(defun fldd--pkgs-to-string-import-paths (pkgs)
  "Return a list of dir names for assoc list PKGS."
  (fldd--flatten (mapcar #'fldd--pkg-to-string-import-paths (cdr pkgs))))


(defun fldd--get-dub-package-dirs-json (json)
  "Get package directories from dub output.
Return the directories where the packages are for the assoclist
in this JSON string.  Any characters before the first opening
brace are discarded before parsing."
  (let* ((data (json-read-from-string json))
         (packages (assq 'packages data)))
    (fldd--pkgs-to-dir-names packages)))

(defun fldd--get-dub-package-string-import-paths-json (json)
  "Get package directories from dub output.
Return the directories where the packages are for the assoclist
in this JSON string.  Any characters before the first opening
brace are discarded before parsing."
  (let* ((data (json-read-from-string json))
         (packages (assq 'packages data)))
    (fldd--pkgs-to-string-import-paths packages)))


(defun fldd--get-dub-package-dirs-output (output)
  "Get package directories from OUTPUT from dub describe.
Normally that output is json but sometimes it might contain
other lines besides the json object."
  (fldd--get-dub-package-dirs-json (substring output (string-match "{" output) (length output))))

(defun fldd--get-dub-package-string-import-paths-output (output)
  "Get package directories from OUTPUT from dub describe.
Normally that output is json but sometimes it might contain
other lines besides the json object."
  (fldd--get-dub-package-string-import-paths-json (substring output (string-match "{" output) (length output))))

(defun fldd--get-project-dir ()
  "Locates the project directory by searching up for either package.json or dub.json."
  (let ((package-json-dir (fldd--locate-topmost "dub.json"))
        (dub-json-dir (fldd--locate-topmost "package.json")))
    (or dub-json-dir package-json-dir)))

(defun fldd--locate-topmost (file-name)
  "Locate the topmost FILE-NAME."
  (fldd--locate-topmost-impl file-name default-directory nil))

(defun fldd--locate-topmost-impl (file-name dir last-found)
  "Locate the topmost FILE-NAME from DIR using LAST-FOUND as a 'plan B'."
  (let ((new-dir (locate-dominating-file dir file-name)))
    (if new-dir
        (fldd--locate-topmost-impl file-name (expand-file-name ".." new-dir) new-dir)
      last-found)))


(defun fldd--get-dub-package-dirs ()
  "Get package directories."
  (let ((default-directory (fldd--get-project-dir)))
    (fldd--get-dub-package-dirs-output (shell-command-to-string "dub describe"))))

(defun fldd--get-dub-string-import-paths ()
  "Get package directories."
  (let ((default-directory (fldd--get-project-dir)))
    (fldd--get-dub-package-string-import-paths-output (shell-command-to-string "dub describe"))))


;;;###autoload
(defun flycheck-dmd-dub-set-include-path ()
  "Set `flycheck-dmd-include-path' from dub info if available."
  (let* ((basedir (fldd--get-project-dir)))
    (when basedir
      (setq flycheck-dmd-include-path (fldd--get-dub-package-dirs)))))

;;;###autoload
(defun flycheck-dmd-dub-set-variables ()
  "Set all flycheck-dmd variables."
  (let* ((basedir (fldd--get-project-dir)))
    (when basedir
      (let* ((output (shell-command-to-string "dub describe"))
             (import-paths (fldd--get-dub-package-dirs-output output))
             (string-import-paths (fldd--get-dub-package-string-import-paths-output output)))
        (setq flycheck-dmd-include-path import-paths)
        (setq flycheck-dmd-flags (mapcar #'(lambda (x) (concat "-J" x)) string-import-paths))))))


(provide 'flycheck-dmd-dub)
;;; flycheck-dmd-dub ends here

;;; flycheck-dmd-dub.el ends here
