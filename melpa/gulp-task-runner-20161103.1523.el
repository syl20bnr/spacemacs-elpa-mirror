;;; gulp-task-runner.el --- Gulp task runner                     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: convenience, javascript
;; Package-Version: 20161103.1523
;; Version: 1.0
;; Package: gulp-task-runner

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

;; Run `M-x gulp' to choose a task from the list of Gulp tasks and run it.

;;; Code:

(defvar gulp--task-cache nil
  "Map gulpfile path to the list of its tasks.")

;;;###autoload
(defun gulp (&optional prefix)
  "Prompt for a gulp task and run it.
With PREFIX or when called interactively with a prefix argument,
forces reload of the task list from gulpfile.js."
  (interactive "P")
  (when prefix
    (gulp--invalidate-cache))
  (let* ((gulpfile (or (gulp--current-gulpfile)
                       (gulp--gulpfile-from-cache))))
    (if gulpfile
        (gulp--run-gulpfile gulpfile)
      (warn "No gulpfile to get the tasks from"))))

(defun gulp--run-gulpfile (gulpfile)
  "Let the user choose a task from GULPFILE and run it."
  (let* ((tasks (gulp--get-tasks gulpfile))
         (task (completing-read "Gulp task: " tasks)))
    ;; Use a temporary buffer to change current directory
    (with-temp-buffer
      (cd (file-name-directory gulpfile))
      (let ((compilation-buffer-name-function #'gulp--get-buffer-name))
        (compilation-start (format "gulp %s" task))))))

(defun gulp--add-to-cache (gulpfile tasks)
  "Add (GULPFILE TASKS) to `gulp--task-cache'."
  ;; because we are using alist functions, there is no need to erase
  ;; pairs with same key
  (setq gulp--task-cache
        (cons (cons gulpfile tasks)
              gulp--task-cache)))

(defun gulp--invalidate-cache (&optional gulpfile)
  "Remove cached task list for GULPFILE.
If GULPFILE is nil, remove task list for `gulp--current-gulpfile'."
  (let ((gulpfile (or gulpfile (gulp--current-gulpfile))))
    (gulp--add-to-cache gulpfile nil)))

(defun gulp--current-gulpfile (&optional dir)
  "Return path of the gulpfile from DIR or `default-directory'."
  (let* ((dir (or dir default-directory))
         (gulpfile (locate-dominating-file (or dir default-directory)
                                           "gulpfile.js")))
    (when gulpfile
      (expand-file-name "gulpfile.js" gulpfile))))

(defun gulp--get-tasks-from-gulp ()
  "Ask gulp for a task list."
  (process-lines "gulp" "--tasks-simple"))

(defun gulp--get-tasks-from-cache (&optional gulpfile)
  "Lookup for a task list for GULPFILE in `gulp--task-cache'.
If GULPFILE is absent, its value is takend from
`gulp--current-gulpfile'."
  (let ((gulpfile (or gulpfile (gulp--current-gulpfile))))
    (cdr (assoc gulpfile gulp--task-cache))))

(defun gulp--get-gulpfiles-from-cache ()
  "Return a list of all gulpfiles in `gulp--task-cache'."
  (mapcar #'car gulp--task-cache))

(defun gulp--gulpfile-from-cache ()
  "Let the user choose a gulpfile from the cache."
  (let ((gulpfiles (gulp--get-gulpfiles-from-cache)))
    (when gulpfiles
      (completing-read "Choose a gulpfile: " gulpfiles))))

(defun gulp--get-tasks (gulpfile)
  "Return a list of gulp tasks for GULPFILE.
Either use `gulp--task-cache' or run gulp to get the tasks."
  (or (gulp--get-tasks-from-cache gulpfile)
      (let ((tasks (gulp--get-tasks-from-gulp)))
        (gulp--add-to-cache gulpfile tasks)
        tasks)))

(defun gulp--get-buffer-name (&rest _)
  "Return the name of a gulp task buffer."
  "*gulp*")

(provide 'gulp-task-runner)
;;; gulp-task-runner.el ends here
