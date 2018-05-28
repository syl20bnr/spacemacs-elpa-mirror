;;; helm-lines.el --- A helm interface for completing by lines -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2016 Free Software Foundation, Inc.

;; Author: @torgeir
;; Version: 1.2.0
;; Package-Version: 20180528.48
;; Keywords: files helm ag vc git lines complete tools languages
;; Package-Requires: ((emacs "24.4") (helm "1.9.8"))
;; URL: https://github.com/torgeir/helm-lines.el/

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

;; A helm interface for completing by lines in project.
;;
;; Run `helm-lines' to complete the current line by other lines that
;; exist in the current git project.
;;
;; Enable `helm-follow-mode' to replace in place as you jump around
;; the suggestions.
;;
;; Requires `ag' [1] to be installed.
;;
;;   [1]: https://github.com/ggreer/the_silver_searcher
;;
;; The project root is defined by the results of
;; `helm-lines-project-root-function', and by default it is the root
;; of the current version-control tree.  Projectile users might like
;; to set this variable to `projectile-root'.

;;; Code:

(require 'helm)
(require 'thingatpt)
(require 'subr-x)


(defgroup helm-lines nil
  "Completion by lines in project."
  :group 'helm)


(defcustom helm-lines-project-root-function 'vc-root-dir
  "Function called to find the root directory of the current project."
  :type 'function)


(defun helm-lines--action (line)
  "Insert the selected LINE at the beginning of the current line.
Indents the line after inserting it."
  (move-beginning-of-line 1)
  (unless (eolp)
    (kill-line))
  (insert line)
  (indent-for-tab-command))


(defvar helm-lines-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-n") 'helm-next-line)
    (define-key map (kbd "C-p") 'helm-previous-line)
    map)
  "Keymap used in helm project lines.")


(defun helm-lines--async-shell-command (cmd)
  "Execute shell CMD async. Puts the output in a *helm-lines* buffer."
  (let ((name "helm-lines"))
    (start-process-shell-command name (format "*%s*" name) cmd)))


(defun helm-lines--candidates (root)
  "Helm candidates by listing all lines under the current git ROOT."
  (let* ((query (if (string-empty-p helm-pattern) "^.*$" helm-pattern))
         (cmd (concat "ag"
                      " --nocolor"
                      " --nonumbers"
                      " --nofilename"
                      " --ignore .git"
                      " --ignore target"
                      " --ignore node_modules"
                      " -i \"" (shell-quote-argument query) "\"" ;; the pattern
                      " " (shell-quote-argument root)            ;; the folder
                      " | grep -Ev \"^$\""                       ;; remove empty lines
                      " | sed -E \"s/^[ \t]*//\""                ;; remove leading ws
                      " | sort -u"                               ;; unique
                      " | head -n 100")))
    (helm-lines--async-shell-command cmd)))


(defun helm-lines ()
  "Helm-lines entry point."
  (interactive)
  (unless (executable-find "ag")
    (user-error "Helm-lines: Could not find executable `ag', did you install it? https://github.com/ggreer/the_silver_searcher"))
  (let ((git-root (expand-file-name (or (funcall helm-lines-project-root-function)
                                        (error "Couldn't determine project root")))))
    (helm :input (string-trim (thing-at-point 'line t))
          :sources (helm-build-async-source "Complete line in project"
                     :candidates-process (lambda () (helm-lines--candidates git-root))
                     :action 'helm-lines--action)
          :keymap helm-lines-map)))


(provide 'helm-lines)

;;; helm-lines.el ends here
