;;; how-many-lines-in-project.el --- Calculate how many lines are there in your project.

;; Copyright (C) 2014 Wei Zhao
;; Author: Wei Zhao <kaihaosw@gmail.com>
;; Git: https://github.com/kaihaosw/how-many-lines-in-project.git
;; Version: 0.4
;; Package-Version: 20140806.2142
;; Created: 2014-07-24
;; Keywords: project, convenience

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

;; This library provides a method for quickly calculating how many
;; lines in a given project. It is inspired by `find-file-in-project'.

;; It depends on the command `find', `wc', `rev' and `sort'.
;; find . -type f \( -name "*.el" -or -name "*.elc" \) -not -regex ".*/elpa/.*"

;; Installation
;; It is recommended installed by the ELPA package system.
;; You could install it by M-x: with
;; package-install: how-many-lines-in-project.

;; Usage
;; M-x: how-many-lines-in-project

;; There are some variables you may need to config.
;; `hm-lines-file-extensions'
;; `hm-lines-sort-by-type'
;; `hm-lines-find-regex'

;;; Code:

(defvar hm-lines-buffer-name "*hm-lines*"
  "Buffer name to display the lines infomation.")

(defvar hm-lines-project-file ".git")

(defvar hm-lines-file-extensions
  '("*.el" "*.lisp" "*.scm" "*.ss" "*.rkt"
    "*.erl" "*.hs" "*.ml" "*.sml"
    "*.c" "*.cpp" "*.hpp" "*.cc" "*.mm"
    "*.java" "*.scala" "*.sbt" "*.groovy" "*.clj"
    "*.py" "*.rb" "*.js" "*.coffee" "*.pl" "*.php" "*.go" "*.lua" "*.rs"
    "*.pas" "*.sh" "*.sql" "*.fs" "*.st" "*.R" "*.swift")
  "List of file extensions that will be looked for.")

(defvar hm-lines-find-regex ""
  "Regex using in the find command. For example: -not -regex \".*/elpa/.*\"")

(defvar hm-lines-sort-by-type nil
  "Sort the result by type or not.")

(defun hm-lines-project-root ()
  "Find the root of a project."
  (let ((root (locate-dominating-file default-directory hm-lines-project-file)))
    (or root (error "no project root found."))))

(defun hm-lines-result (root)
  (let* ((find-patterns
          (mapconcat
           (lambda (e) (format "-name \"%s\"" e)) hm-lines-file-extensions " -or "))
         (find-command
          (format "find %s -type f \\( %s \\) %s %s"
                  (substring root 0 -1)
                  find-patterns
                  hm-lines-find-regex
                  (if hm-lines-sort-by-type
                      "| rev | sort | rev" "")))
         (find-result
          (replace-regexp-in-string "\n" " "
                                    (shell-command-to-string find-command))))
    (replace-regexp-in-string (file-truename root) "   "
                              (shell-command-to-string (concat "wc -l " find-result)))))

;;;###autoload
(defun hm-lines-in-project ()
  "Calculate how many lines are there in your project."
  (interactive)
  (let ((root (hm-lines-project-root)))
    (get-buffer-create hm-lines-buffer-name)
    (switch-to-buffer hm-lines-buffer-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (hm-lines-result root))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (switch-to-buffer hm-lines-buffer-name)))

;;;###autoload
(defalias 'how-many-lines-in-project 'hm-lines-in-project)

(provide 'how-many-lines-in-project)

;;; how-many-lines-in-project.el ends here
