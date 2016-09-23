;;; gulp-task-runner.el --- Gulp task runner                     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: convenience, javascript
;; Package-Version: 1.0
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

;;;###autoload
(defun gulp ()
  "Prompt for a gulp task and run it."
  (interactive)
  (let* ((tasks (gulp--get-tasks))
         (task (completing-read "Gulp task: " tasks)))
    (let ((compilation-buffer-name-function #'gulp--get-buffer-name))
     (compilation-start (format "gulp %s" task)))))

(defun gulp--get-tasks ()
  "Return a list of gulp tasks for the current project."
  (process-lines "gulp" "--tasks-simple"))

(defun gulp--get-buffer-name (&rest _)
  "Return the name of a gulp task buffer."
  "*gulp*")

(provide 'gulp-task-runner)
;;; gulp-task-runner.el ends here
