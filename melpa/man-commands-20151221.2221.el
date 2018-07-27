;;; man-commands.el --- Add interactive commands for every manpages installed in your computer.

;; Copyright (C) 2013 Nathaniel Flath <nflath@gmail.com>

;; Author: Nathaniel Flath <nflath@gmail.com>
;; URL: http://github.com/nflath/man-commands
;; Package-Version: 20151221.2221
;; Version: 1.1
;; Package-Requires: ((cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; I usually try to do M-x man-mail, or whatever command I'm trying, before
;; going to M-x max and entering the command.  The following actually installs
;; all of the commands you can man as interactive command.

;;; Installation:

;; To install, put this file somewhere in your load-path and add the following
;; to your .emacs file:
;;(require 'man-commands)

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
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

;;; Code:

(require 'cl-lib)

(defvar man-commands-man-dir "/usr/share/man/" "Location of man files on your system")

(defun man-commands-before-first (regexp string)
  "Returns the prefix of string that occurs directly before the start of the first match of 'regexp'."
  (let ((index (string-match regexp string)))
    (if index
        (substring string 0 (match-beginning 0))
      string)))

(defun man-commands-after-last (regexp string)
  "Returns the part of the string after the last occurrence of regexp."
  (let ((index (string-match regexp string)))
    (if index
        (man-commands-after-last regexp (substring string (match-end 0) (length string)))
      string)))

(defun man-commands-directory-files-recursive (dir )
  "Returns a list of files in the directory specified and all subdirectories."
  (apply #'append
         (mapcar
          (lambda (file)
            (when (not (string-match ".*\\.$" file))
              (if (file-directory-p file)
                  (man-commands-directory-files-recursive file)
                (list file))))
          (directory-files dir t))))

(defun man-commands-update-commands ()
  (interactive)
  (let ((man-page-list nil))
    (mapc (lambda (dir)
            (mapcar
             (lambda (file) (add-to-list 'man-page-list
                                         (man-commands-before-first "\\." (man-commands-after-last "/" file))))
             (man-commands-directory-files-recursive (concat man-commands-man-dir dir))))
          (cl-remove-if-not (lambda (elt) (string-match "man" elt))
                            (directory-files man-commands-man-dir)))
    (mapcar (lambda (elt)
              (eval
               `(defun ,(intern (concat "man-" elt)) ()
                  (interactive)
                  (man ,elt))))
            man-page-list)))

(man-commands-update-commands)

(provide 'man-commands)
;;; man-commands.el ends here
