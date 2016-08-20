;;; narrowed-page-navigation.el --- A minor mode for showing one page at a time  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 David Raymond Christiansen

;; Author: David Raymond Christiansen <david@davidchristiansen.dk>
;; Keywords: outlines
;; Package-Version: 20150108.2119
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
;; Version: 0.1.0
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

;; `narrowed-page-navigation-mode' is a minor mode that provides
;; keybindings for showing only one page at a time. This is intended
;; to be useful primarily for presentations and live coding, where it
;; might be convenient to avoid distractions for the audience.

;;; Code:

(require 'cl-lib)

(defun narrowed-page-navigation-next ()
  "Narrow to the next page."
  (interactive)
  (narrow-to-page 1)
  (goto-char (point-min)))

(defun narrowed-page-navigation-previous ()
  "Narrow to the previous page."
  (interactive)
  (narrow-to-page -1)
  (goto-char (point-min)))

(defun narrowed-page-navigation-count ()
  "Return a cons cell whose car is the current page and whose cdr is the number of pages in the buffer."
  (save-restriction
    (widen)
    (save-excursion
      (let ((page 1)
            total
            (opoint (point)))
        (goto-char (point-min))
        (while (re-search-forward page-delimiter opoint t)
          (when (= (match-beginning 0) (match-end 0))
            (forward-char 1))
          (cl-incf page))
        (setq total page)
        (while (re-search-forward page-delimiter nil t)
          (when (= (match-beginning 0) (match-end 0))
            (forward-char 1))
          (cl-incf total))
        (cons page total)))))

;; Based on helm-current-buffer-narrowed-p
(defun narrowed-page-navigation-current-buffer-narrowed-p (&optional buffer)
  "Check if BUFFER is narrowed.  If BUFFER is nil, the current buffer is used."
  (with-current-buffer (or buffer (current-buffer))
    (let ((beg (point-min))
          (end (point-max))
          (total (buffer-size)))
      (or (/= beg 1) (/= end (1+ total))))))

;;;###autoload
(define-minor-mode narrowed-page-navigation-mode
  "A minor mode with convenient bindings for moving between narrowed pages."
  nil (:eval (let ((where (narrowed-page-navigation-count)))
               (format " NPNav(%s/%s)" (car where) (cdr where))))
  `((,(kbd "<S-down>") . narrowed-page-navigation-next)
    (,(kbd "<S-up>") . narrowed-page-navigation-previous)))

(easy-menu-define narrowed-page-navigation-menu narrowed-page-navigation-mode-map
  "Menu for narrowed page navigation mode."
  '("NPNav"
    ["Narrow to current page" narrow-to-page t]
    ["Narrow to next page" narrowed-page-navigation-next t]
    ["Narrow to previous page" narrowed-page-navigation-previous t]
    ["Widen" widen t]
    "---------------"
    ["Turn off minor mode" #'(lambda () (interactive) (narrowed-page-navigation-mode -1)) t]
    ))

(provide 'narrowed-page-navigation)
;;; narrowed-page-navigation.el ends here
