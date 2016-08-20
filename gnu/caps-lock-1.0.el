;;; caps-lock.el --- Caps-lock as a minor mode       -*- lexical-binding: t -*-

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Version: 1.0

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

;;; Code:

(defvar caps-lock-commands
  '(self-insert-command isearch-printing-char)
  "List of commands that are subject to `caps-lock-mode'.")

;;;###autoload
(define-minor-mode caps-lock-mode
  "Make self-inserting keys invert the capitalization."
  :global t
  (if caps-lock-mode
      (add-hook 'pre-command-hook #'caps-lock--pch)
    (remove-hook 'pre-command-hook #'caps-lock--pch)))

(defun caps-lock--pch ()
  (when (and (characterp last-command-event)
             (or (memq this-command caps-lock-commands)
                 (eq this-command (key-binding [remap self-insert-command]))))
    (setq last-command-event
          (condition-case nil
              (let ((up (upcase last-command-event)))
                (if (eq up last-command-event)
                    (downcase last-command-event)
                  up))
            (error last-command-event)))))

;;;; ChangeLog:

;; 2014-07-07  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* caps-lock: New package.
;; 


(provide 'caps-lock)
;;; caps-lock.el ends here
