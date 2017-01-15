;;; js-import.el --- Import Javascript files from your current project or dependencies  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Jakob Lind

;; Author: Jakob Lind <karl.jakob.lind@gmail.com>
;; URL: https://github.com/jakoblind/js-import
;; Package-Version: 20170115.853
;; Package-Requires: ((emacs "24.4") (f "0.19.0") (projectile "0.14.0") (dash "2.13.0"))
;; Version: 1.0
;; Keywords: tools

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

;;; Code:

(require 'f)
(require 'json)
(require 'subr-x)
(require 'projectile)

(defcustom js-import-quote "\""
  "Quote type used."
  :group 'js-import
  :type '(choice (const :tag "Double" "\"")
                 (const :tag "Single" "'")))

(defun js-import-get-package-json ()
  "Return the path to package.json from projectile-project-root."
  (concat (projectile-project-root) "package.json"))

(defun js-import-get-project-dependencies (package-json-path section)
  "Return a list of strings with dependencies fetched from PACKAGE-JSON-PATH in SECTION.  If file not found, return nil."
  (let ((json-object-type 'hash-table))
    (when-let ((package-json-content (condition-case nil (f-read-text package-json-path 'utf-8) (error nil)))
               (dependencies-hash (condition-case nil (gethash section (json-read-from-string package-json-content)) (error nil))))
      (when dependencies-hash
        (hash-table-keys dependencies-hash)))))

(defun js-import-string-ends-with-p (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))

(defun js-import-is-js-file (filename)
  "Check if FILENAME ends with either .js or .jsx."
  (or (js-import-string-ends-with-p filename ".js") (js-import-string-ends-with-p filename ".jsx")))

(defun js-import-from-section (section)
  "Import Javascript files from your current project or dependencies from package.json in section SECTION."
  (save-excursion
    (let* ((filtered-project-files (-filter 'js-import-is-js-file (projectile-current-project-files)))
           (all (append (js-import-get-project-dependencies (js-import-get-package-json) section) filtered-project-files))
           (selected-file (completing-read "Select a file to import: " all))
           (selected-file-name (f-filename (f-no-ext selected-file)))
           (selected-file-relative-path
            (f-relative
             (concat (projectile-project-root) (f-no-ext selected-file))
             (file-name-directory (buffer-file-name))))
           (sap (symbol-at-point))
           (proposed-symbol (or (and sap (symbol-name sap)) selected-file-name))
           (read-symbols
            (read-string (format "Symbols (default: %s): " proposed-symbol) nil nil proposed-symbol))
           (symbols (if (string-match-p "^[^*]* " read-symbols)
                        (concat "{ " read-symbols " }")
                      read-symbols)))

      (if (re-search-backward "^import " nil t)
          (progn (end-of-line) (newline))
        (goto-char (point-min)) (split-line))

      (insert (concat
               "import "
               symbols
               " from "
               js-import-quote
               (if (js-import-is-js-file selected-file) (replace-regexp-in-string "^\\([^\\.]\\)" "./\\1" selected-file-relative-path) selected-file-name)
               js-import-quote
               ";")))))

;;;###autoload
(defun js-import ()
  "Import Javascript files from your current project or dependencies."
  (interactive)
  (js-import-from-section "dependencies"))

;;;###autoload
(defun js-import-dev ()
  "Import Javascript files from your current project or devDependencies."
  (interactive)
  (js-import-from-section "devDependencies"))

(provide 'js-import)
;;; js-import.el ends here
