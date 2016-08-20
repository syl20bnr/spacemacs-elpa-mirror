;;; ac-anaconda.el --- Anaconda sources for auto-complete-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2016 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/ac-anaconda
;; Package-Version: 20160221.1123
;; Version: 0.1.0
;; Package-Requires: ((auto-complete "1.4.0") (anaconda-mode "0.1.1") (dash "2.6.0"))

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

;; See the README for more details.

;;; Code:

(require 'auto-complete)
(require 'anaconda-mode)

(defun ac-anaconda-candidates ()
  "Send `anaconda-mode' complete request."
  (anaconda-mode-call "complete" 'ac-anaconda-candidates-callback)
  nil)

(defun ac-anaconda-candidates-callback (result)
  "Obtain candidates list from RESULT."
  (let ((ac-sources `(((candidates . ,(lambda ()
                                        (--map (popup-make-item (cdr (assoc 'name it))
                                                :document (cdr (assoc 'docstring it))
                                                :summary (if (equal "statement" (cdr (assoc 'type it)))
                                                             "statement"
                                                           (cdr (assoc 'description it))))
                                               result)))
                       (symbol . "a"))
                      ,@(cdr ac-sources))))
    (ac-start)))

(ac-define-source anaconda
  '((candidates . ac-anaconda-candidates)))

;;;###autoload
(defun ac-anaconda-setup ()
  "Set up `ac-sources' for `anaconda-mode'."
  (interactive)
  (push 'ac-source-anaconda ac-sources)
  (unless auto-complete-mode
    (auto-complete-mode)))

(provide 'ac-anaconda)

;;; ac-anaconda.el ends here
