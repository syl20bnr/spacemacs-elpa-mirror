;;; sly-company.el --- sly completion backend for company mode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2009-2014  Ole Arndt, João Távora
;;
;; Author: Ole Arndt <anwyn@sugarshark.com>
;; Keywords: convenience, lisp, abbrev
;; Package-Version: 20180518.1007
;; Version: 0.8
;; Package-Requires: ((sly "1.0.0-alpha") (company "0.7") (emacs "24.3"))
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;; 
;; A company-mode Emacs completion backend for
;; SLY, a Common Lisp IDE.
;;
;; SLY lives at https://github.com/capitaomorte/sly. company-mode
;; lives at http://company-mode.github.io.  slime-company, whereupon
;; sly-company is based, lives at https://github.com/anwyn/slime-company.
;; 
;;; Installation:
;;
;; Install via MELPA, or alternatively do the following in your
;; `~/.emacs' or `~/.emacs.d/init/el' init file.
;;  
;;     (add-to-list 'load-path "/path/to/sly-company")
;;     (require 'sly-company)
;;  
;; In either case add this to the init file as well:
;;  
;;     (add-hook 'sly-mode-hook 'sly-company-mode)
;;     (add-to-list 'company-backends 'sly-company)
;;
;; The following bindings for `company-active-map' may be useful:
;;
;;   (define-key company-active-map (kbd "\C-n") 'company-select-next)
;;   (define-key company-active-map (kbd "\C-p") 'company-select-previous)
;;   (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
;;   (define-key company-active-map (kbd "M-.") 'company-show-location)
;;
;;
;;; Code:

(require 'company)
(require 'cl-lib)

(defgroup sly-company nil
  "Interaction between sly and the company completion mode."
  :group 'company
  :group 'sly)

(defcustom sly-company-completion 'simple
  "Which Sly completion to use: `simple' or `fuzzy'.

`simple' just displays the completion candidate,
`fuzzy' also displays the classification flags as an annotation,
alignment of annotations via `company-tooltip-align-annotations'
is recommended."
  :group 'sly-company
  :type '(choice
          (const simple)
          (const fuzzy)))

(defcustom sly-company-complete-in-comments-and-strings nil
  "Should sly-company also complete in comments and strings."
  :group 'sly-company
  :type 'boolean)

;;;###autoload
(define-minor-mode sly-company-mode
  "Minor mode for using `company-mode' with SLY."
  nil nil nil
  (company-mode 1))

(defun sly-company-fetch-candidates-simple (prefix)
  (let ((package (sly-current-package)))
    (lambda (callback)
      (sly-eval-async
          `(slynk-completion:simple-completions ,prefix ',package)
        (lambda (result)
          (funcall callback (cl-first result)))
        package))))

(defun sly-company-fetch-candidates-fuzzy (prefix)
  (let ((package (sly-current-package)))
    (lambda (callback)
      (sly-eval-async
          `(slynk-completion:flex-completions ,prefix ',package)
        (lambda (result)
          (funcall callback
                   (mapcar (lambda (completion)
                             (propertize (cl-first completion)
                                         'score (cl-second completion)
                                         'flags (cl-fourth completion)))
                           (cl-first result))))
        package))))

(defun sly-company-fetch-candidates-async (prefix)
  (cons :async
        (cl-ecase sly-company-completion
          (simple (sly-company-fetch-candidates-simple prefix))
          (fuzzy (sly-company-fetch-candidates-fuzzy prefix)))))

(defun sly-company-meta (candidate)
  (let ((arglist (sly-eval `(slynk:operator-arglist ,candidate ,(sly-current-package)))))
    (if arglist (sly-autodoc--fontify arglist)
      :not-available)))

(defun sly-company-doc-buffer (candidate)
  (let ((doc (sly-eval `(slynk:describe-symbol ,candidate))))
    (with-current-buffer (company-doc-buffer)
      (insert doc)
      (goto-char (point-min))
      (current-buffer))))

(defun sly-company-location (candidate)
  (let ((source-buffer (current-buffer)))
    (save-window-excursion
      (sly-edit-definition candidate)
      (let ((buffer (if (eq source-buffer (current-buffer))
                        sly-xref-last-buffer
                      (current-buffer))))
        (when (buffer-live-p buffer)
          (cons buffer (with-current-buffer buffer
                         (point))))))))

;;;###autoload
(defun sly-company (command &optional arg &rest ignored)
  "Company mode backend for sly."
  (cl-case command
    ('init nil)
    ('prefix
     (if (and sly-company-mode
              (sly-connected-p)
              (or sly-company-complete-in-comments-and-strings
                  (null (company-in-string-or-comment))))
         (or (company-grab-symbol) 'stop)
       'stop))
    ('candidates
     (when (sly-connected-p)
       (sly-company-fetch-candidates-async (substring-no-properties arg))))
    ('meta
     (sly-company-meta (substring-no-properties arg)))
    ('annotation
     (concat " " (get-text-property 0 'flags arg)))
    ('doc-buffer
     (sly-company-doc-buffer (substring-no-properties arg)))
    ('location
     (sly-company-location (substring-no-properties arg)))
    ('post-completion)
    ('sorted
     (eq sly-company-completion 'fuzzy))))

(provide 'sly-company)

;;; sly-company.el ends here
