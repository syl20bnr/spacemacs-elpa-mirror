;;; helm-elscreen.el --- Elscreen with helm interface -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2017 Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; Author: Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; Version: 1.0
;; Package-Version: 20170709.914
;; Keywords: files, convenience
;; URL: https://github.com/emacs-helm/helm-elscreen
;; Package-Requires: ((helm "2.8.0") (elscreen "0") (cl-lib "0.5") (emacs "24.1"))

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
;; helm-elscreen is a Helm interface for Elscreen.

;;; Code:
(require 'cl-lib)
(require 'helm)
(require 'helm-utils)

(declare-function elscreen-find-screen-by-buffer "ext:elscreen.el" (buffer &optional create))
(declare-function elscreen-find-file "ext:elscreen.el" (filename))
(declare-function elscreen-goto "ext:elscreen.el" (screen))
(declare-function elscreen-get-conf-list "ext:elscreen.el" (type))

(defun helm-elscreen-find-buffer (candidate)
  "Open CANDIDATE buffer in new elscreen.
If marked buffers, open all in elscreens."
  (helm-require-or-error 'elscreen 'helm-find-buffer-on-elscreen)
  (helm-aif (helm-marked-candidates)
      (cl-dolist (i it)
        (let ((target-screen (elscreen-find-screen-by-buffer
                              (get-buffer i) 'create)))
          (elscreen-goto target-screen)))
    (let ((target-screen (elscreen-find-screen-by-buffer
                          (get-buffer candidate) 'create)))
      (elscreen-goto target-screen))))

;; For compatibility
(defalias 'helm-find-buffer-on-elscreen 'helm-elscreen-find-buffer)

(defun helm-elscreen-find-file (file)
  "Switch to a elscreen visiting FILE.
If none already exists, creating one."
  (helm-require-or-error 'elscreen 'helm-elscreen-find-file)
  (elscreen-find-file file))

(defclass helm-elscreen-source (helm-source-sync)
  ((candidates
    :initform
    (lambda ()
      (when (cdr (elscreen-get-screen-to-name-alist))
        (cl-sort (cl-loop for (screen . name) in (elscreen-get-screen-to-name-alist)
                       collect (cons (format "[%d] %s" screen name) screen))
                 #'< :key #'cdr))))
   (action :initform
           '(("Change Screen" .
              (lambda (candidate)
                (elscreen-goto candidate)))
             ("Kill Screen(s)" .
              (lambda (_)
                (cl-dolist (i (helm-marked-candidates))
                  (elscreen-goto i)
                  (elscreen-kill))))
             ("Only Screen" .
              (lambda (candidate)
                (elscreen-goto candidate)
                (elscreen-kill-others)))))
   (migemo :initform t)))

(defclass helm-elscreen-source-history (helm-elscreen-source)
  ((candidates
    :initform
    (lambda ()
      (let ((sname (elscreen-get-screen-to-name-alist)))
        (when (cdr sname)
          (cl-loop for screen in (cdr (elscreen-get-conf-list 'screen-history))
                collect (cons (format "[%d] %s" screen (cdr (assq screen sname)))
                              screen))))))))

(defvar helm-elscreen-source-list
  (helm-make-source "ElScreen" 'helm-elscreen-source))

(defvar helm-elscreen-source-history-list
  (helm-make-source "ElScreen History" 'helm-elscreen-source-history))

;;;###autoload
(defun helm-elscreen ()
  "Preconfigured helm to list elscreen."
  (interactive)
  (helm-other-buffer 'helm-elscreen-source-list "*Helm ElScreen*"))

;;;###autoload
(defun helm-elscreen-history ()
  "Preconfigured helm to list elscreen in history order."
  (interactive)
  (helm-other-buffer 'helm-elscreen-source-history-list "*Helm ElScreen*"))

(provide 'helm-elscreen)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-elscreen.el ends here
