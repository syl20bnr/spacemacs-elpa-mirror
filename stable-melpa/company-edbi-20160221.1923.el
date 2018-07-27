;;; company-edbi.el --- Edbi backend for company-mode

;; Copyright (C) 2014-2016 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/company-edbi
;; Package-Version: 20160221.1923
;; Version: 0.1.0
;; Package-Requires: ((company "0.8.5") (edbi "0.1.3") (cl-lib "0.5.0") (s "1.9.0"))

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

(require 'company)
(require 'cl-lib)
(require 'edbi)
(require 's)

(defun company-edbi-prefix ()
  "Grab prefix for `company-edbi' backend."
  (and (eq major-mode 'edbi:sql-mode)
       (edbi:connection-ac edbi:connection)
       (not (company-in-string-or-comment))
       (or (company-grab-symbol-cons "\\." 1)
           'stop)))

(defun company-edbi-candidates (prefix)
  "Candidates list for `edbi' query editor.
PREFIX is a candidates prefix supplied by `company'."
  (cl-remove-if-not
   (lambda (x) (s-prefix? prefix x t))
   (mapcar
    (lambda (x)
      (if (consp x)
          (car x)
        x))
    (append
     (edbi:ac-editor-table-candidates)
     (edbi:ac-editor-column-candidates)
     (edbi:ac-editor-type-candidates)
     (edbi:ac-editor-keyword-candidates)))))

(defun company-edbi-meta (candidate)
  "Get CANDIDATE meta information."
  (let* ((summary (get-text-property 0 'summary candidate))
         (document  (get-text-property 0 'document candidate)))
    (setq summary (or summary ""))
    (setq document (s-trim-right (or document "")))
    (concat summary
            (unless (string= summary document)
              (concat " " document)))))

;;;###autoload
(defun company-edbi (command &optional arg &rest _args)
  "Edbi backend for company-mode.
See `company-backends' for more info about COMMAND and ARG."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-edbi))
    (prefix (company-edbi-prefix))
    (candidates (company-edbi-candidates arg))
    (meta (company-edbi-meta arg))
    (ignore-case t)))

(add-hook 'edbi:dbview-update-hook 'edbi:ac-editor-word-candidate-update)

(provide 'company-edbi)

;;; company-edbi.el ends here
