;;; anything-prosjekt.el --- Anything integration for prosjekt.

;; Copyright (C) 2012 Austin Bingham
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 0.1
;; Package-Version: 20140129.4
;; URL: https://github.com/abingham/prosjekt
;; Package-Requires: ((prosjekt "0.3") (anything "0"))

;; Originally based on anything-eproject by Daniel Hackney
;; http://www.emacswiki.org/emacs/anything-eproject.el

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; Allows opening and closing prosjekt projects through anything, as well as
;; selection of files within a project.
;;
;; To activate, add anything-prosjekt.el to your load path and
;;
;;   (require 'anything-prosjekt)
;;
;; to your .emacs. You can then use the following sources
;;
;;  `anything-c-source-prosjekt-files'
;;    Search for files in the current prosjekt.
;;  `anything-c-source-prosjekt-projects'
;;    Open or close prosjekt projects.
;;
;; like this
;;
;;   (add-to-list 'anything-sources 'anything-c-source-prosjekt-files t)
;;   (add-to-list 'anything-sources 'anything-c-source-prosjekt-projects t)
;;
;; Anything: http://www.emacswiki.org/emacs/Anything
;;
;; License:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(require 'prosjekt)
(require 'anything)

(defun prosjekt-temp-file-name ()
  (mapcar
   (lambda (item)
     (expand-file-name (concat prosjekt-proj-dir item)))
   (prosjekt-proj-files)))

(defvar anything-c-source-prosjekt-files
  '((name . "Files in prosjekt")
    (init . anything-c-source-prosjekt-files-init)
    ;(candidates-in-buffer)
    ; Grab candidates from all project files.
    (candidates . prosjekt-temp-file-name)
    (type . file)
    ; TODO: Understand this next one better.
    (real-to-display . (lambda (real)
                         (file-relative-name real prosjekt-proj-dir)))
    )
  "Search for files in the current prosjekt.")

(defun anything-c-source-prosjekt-files-init ()
  "Build `anything-candidate-buffer' of prosjekt files."
  (with-current-buffer (anything-candidate-buffer 'local)
    (mapcar
     (lambda (item)
       (insert (format "%s\n" item)))
     (prosjekt-proj-files))))

(defvar anything-c-source-prosjekt-projects
  '((name . "Prosjekt projects")
    (candidates . (lambda ()
                    (prosjekt-cfg-project-list (prosjekt-cfg-load))))
    (action ("Open Project" . (lambda (cand)
                                (prosjekt-open cand)))
            ("Close project" . (lambda (cand)
                                 (prosjekt-close))))
    "Open or close prosjekt projects."))

;;;###autoload
(defun anything-prosjekt ()
  (interactive)
  (anything-other-buffer '(anything-c-source-prosjekt-files
                           anything-c-source-prosjekt-projects)
                          "*anything-prosjekt buffer*"))

(provide 'anything-prosjekt)

;;; anything-prosjekt.el ends here
