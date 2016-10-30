;;; gulp-task-runner.el --- Gulp task runner                     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: convenience, javascript
;; Package-Version: 20161030.646
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
  (let* ((tasks (gulp--get-tasks))
         (task (completing-read "Gulp task: " tasks)))
    (let ((compilation-buffer-name-function #'gulp--get-buffer-name))
      (compilation-start (format "gulp %s" task)))))

(defun gulp--add-to-cache (gulpfile tasks)
  "Add (GULPFILE TASKS) to `gulp--task-cache'."
  ;; because we are using alist functions, there is no need to erase
  ;; pairs with same key
  (setq gulp--task-cache
        (cons (cons gulpfile tasks)
              gulp--task-cache)))

(defun gulp--invalidate-cache (&optional gulpfile)
  "Remove cached task list for GULPFILE.
If GULPFILE is nil, remove task list for `gulp--get-gulpfile'."
  (let ((gulpfile (or gulpfile (gulp--get-gulpfile))))
    (gulp--add-to-cache gulpfile nil)))

(defun gulp--get-gulpfile (&optional dir)
  "Return path of the gulpfile from DIR or `default-directory'."
  (let ((dir (or dir default-directory)))
    (expand-file-name
     "gulpfile.js"
     (locate-dominating-file (or dir default-directory)
                             "gulpfile.js"))))

(defun gulp--get-tasks-from-gulp ()
  "Ask gulp for a task list."
  (process-lines "gulp" "--tasks-simple"))

(defun gulp--get-tasks-from-cache (&optional gulpfile)
  "Lookup for a task list for GULPFILE in `gulp--task-cache'.
If GULPFILE is absent, its value is takend from
`gulp--get-gulpfile'."
  (let ((gulpfile (or gulpfile (gulp--get-gulpfile))))
    (cdr (assoc gulpfile gulp--task-cache))))

(defun gulp--get-tasks ()
  "Return a list of gulp tasks for the current project."
  (let* ((gulpfile (gulp--get-gulpfile)))
    (or (gulp--get-tasks-from-cache gulpfile)
        (let ((tasks (gulp--get-tasks-from-gulp)))
          (setq gulp--task-cache (cons (cons gulpfile tasks)
                                       gulp--task-cache))
          tasks))))

(defun gulp--get-buffer-name (&rest _)
  "Return the name of a gulp task buffer."
  "*gulp*")

(provide 'gulp-task-runner)
;;; gulp-task-runner.el ends here
