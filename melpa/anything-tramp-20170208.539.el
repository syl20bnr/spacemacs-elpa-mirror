;;; anything-tramp.el --- Tramp with anything interface -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by masasam

;; Author: masasam
;; URL: https://github.com/masasam/emacs-anything-tramp
;; Package-Version: 20170208.539
;; Version: 0.01
;; Package-Requires: ((anything "1.0") (emacs "24"))

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

;; tramp with anything interface

;;; Code:

(require 'anything)
(require 'tramp)

(defgroup anything-tramp nil
  "tramp with anything interface"
  :group 'anything)

(defun anything-tramp--candidates ()
  (let ((source (split-string
                 (with-temp-buffer
                   (insert-file-contents "~/.ssh/config")
                   (buffer-string))
                 "\n"))
        (hosts (list)))
    (dolist (host source)
      (when (string-match "[H\\|h]ost +\\(.+?\\)$" host)
	(setq host (match-string 1 host))
	(if (string-match "[ \t\n\r]+\\'" host)
	    (replace-match "" t t host))
	(if (string-match "\\`[ \t\n\r]+" host)
	    (replace-match "" t t host))
        (unless (string= host "*")
          (push
	   (concat "/" tramp-default-method ":" host ":/")
	   hosts)
	  (push
	   (concat "/" tramp-default-method ":" host "|sudo:" host ":/")
	   hosts))))
    (reverse hosts)))

(defun anything-tramp-open (path)
  "Tramp open with PATH."
  (find-file path))

(defvar anything-tramp-hosts
  '((name . "Tramp")
    (candidates . (lambda () (anything-tramp--candidates)))
    (type . file)
    (action . (("Tramp" . anything-tramp-open)))))

;;;###autoload
(defun anything-tramp ()
  "Open your ~/.ssh/config with anything interface.
You can connect your server with tramp"
  (interactive)
  (unless (file-exists-p "~/.ssh/config")
    (error "There is no ~/.ssh/config"))
  (anything-other-buffer
   '(anything-tramp-hosts)
   "*anything-tramp*"))

(provide 'anything-tramp)

;;; anything-tramp.el ends here
