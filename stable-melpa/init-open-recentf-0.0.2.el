;;; init-open-recentf.el --- Open recentf immediately after Emacs is started -*- coding: utf-8 ; lexical-binding: t -*-

;; Copyright (C) 2015 USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 26 Oct 2015
;; Version: 0.0.1
;; Package-Version: 0.0.2
;; Keywords: file recentf after-init-hook
;; Package-Requires: ((emacs "24.4"))

;; This file is NOT part of GNU Emacs.

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
;;
;; Open recentf immediately after Emacs is started.
;; If files are opend, does nothing.  Open recentf otherwise.
;; (For example, it is when execute by specifying the file from command line.)
;; This script uses only advice function for startup.  Not privede interactive functions.
;; (This approach's dirty hack, but the hook to be the alternative does not exist.)
;;
;; put into your own .emacs file (~/.emacs.d/init.el)
;;
;;   (init-open-recentf)
;;
;; `init-open-recentf' Support helm, ido, anything (or nothing).
;; Determine from your environment, but it is also possible that you explicitly.
;;
;;   (setq init-open-recentf-interface 'ido)
;;   (init-open-recentf)
;;
;; If you want to do another thing, you can specify an arbitrary function.
;;
;;   (setq init-open-recentf-function #'awesome-open-recentf)
;;   (init-open-recentf)
;;

;;; Code:
(require 'cl-lib)
(require 'recentf)

(defgroup init-open-recentf nil
  "init-open-recentf"
  :group 'emacs)

(defcustom init-open-recentf-function
  nil
  "Function to open recentf files (or other)."
  :type '(function :tag "Invoke recentf (or other) function")
  :group 'init-open-recentf)

(defcustom init-open-recentf-interface
  nil
  "Interface to open recentf files."
  :type '(radio (const :tag "Use ido interface" 'ido)
                (const :tag "Use helm interface" 'helm)
                (const :tag "Use anything intereface" 'anything)
                (const :tag "Use Emacs default (recentf-open-files)" 'default)
                (const :tag "Select automatically" 'nil))
  :group 'init-open-recentf)

(defun init-open-recentf-buffer-files ()
  "Return list of opened file names."
  (let ((found-files '()))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when buffer-file-name
          (cl-pushnew buffer-file-name found-files))))
    found-files))

(defun init-open-recentf-interface ()
  ""
  (or init-open-recentf-interface
      (cond
       ((and (boundp 'helm-mode) helm-mode) 'helm)
       ((and (boundp 'ido-mode) ido-mode) 'ido)
       ((fboundp 'anything-for-files) 'anything)
       (:else 'default))))

(defun init-open-recentf-dwim ()
  "Open recent file command you want (Do What I Mean)."
  (if init-open-recentf-function
      (call-interactively init-open-recentf-function)
    (cl-case (init-open-recentf-interface)
      ((helm) (helm-recentf))
      ((ido) (find-file (ido-completing-read "Find recent file: " recentf-list)))
      ((anything) (anything-for-files))
      ((default) (recentf-open-files)))))

(defun init-open-recentf-open (&rest dummy-args)
  "If files are opend, does nothing.  Open recentf otherwise.
`DUMMY-ARGS' is ignored."
  (prog1
      (cond
       ((init-open-recentf-buffer-files) t)
       ((recentf-enabled-p) (init-open-recentf-dwim))
       (:else
        (error "recentf-mode is not enabled."))))
  (advice-remove 'display-startup-screen #'init-open-recentf-open))

;;;###autoload
(defun init-open-recentf ()
  "Set 'after-init-hook ."
  (advice-add 'command-line-1 :after #'init-open-recentf-open)
  t)

(provide 'init-open-recentf)
;;; init-open-recentf.el ends here
