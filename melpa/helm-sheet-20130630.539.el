;;; helm-sheet.el --- helm sources for sheet

;; Copyright (C) 2013 Yasuyuki Oka

;; Author: Yasuyuki Oka <yasuyk@gmail.com>
;; Version: DEV
;; Package-Version: 20130630.539
;; URL: https://github.com/yasuyk/helm-sheet
;; Package-Requires: ((helm "1.0"))
;; Keywords: helm sheet

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

;; Add the following to your Emacs init file:
;;
;; (require 'helm-sheet)  ;; Not necessary if using ELPA package
;; (global-set-key (kbd "C-c s") 'helm-sheet)

;; That's all.

;;; Code:

(require 'helm)

(defgroup helm-sheet nil
  "Sheet related Applications and libraries for Helm."
  :prefix "helm-sheet-"
  :group 'http)

(defcustom helm-sheet-command "sheet"
  "Name of sheet command."
  :type 'string
  :group 'helm-sheet)

(defconst helm-sheets-directory "~/.sheets"
  "Path of sheets directory.")

(defun helm-sheet-file (sheet)
  "Return path of a SHEET file."
  (format "%s/%s" helm-sheets-directory sheet))

(defvar helm-source-sheet-create
  '((name . "Creates a new sheet")
    (dummy)
    (no-delay-on-input)
    (action . (lambda (candidate)
                (find-file (helm-sheet-file candidate)))))
  "Create a new sheet.")

(defun helm-sheet-candidates ()
  "Return a list of all sheets."
  (let ((cmd (format "%s list" helm-sheet-command)))
    (with-current-buffer  (helm-candidate-buffer 'global)
      (let ((ret (call-process-shell-command cmd nil t)))
        (unless (zerop ret)
          (error "Failed: %s (retval=%d)" cmd ret))))))

(defun helm-sheet-copy (candidate)
  "Copies content of the sheet to the clipboard by sheet copy subcommand.
Argument CANDIDATE candidate of `helm-source-sheet'."
  (let ((cmd (format "%s copy %s" helm-sheet-command candidate)))
    (let ((ret (call-process-shell-command cmd nil t)))
        (unless (zerop ret)
          (error "Failed: %s (retval=%d)" cmd ret)))))

(defvar helm-source-sheet
  '((name . "sheet")
    (init . helm-sheet-candidates)
    (candidates-in-buffer)
    (action . (("Open" . (lambda (candidate)
                           (find-file (helm-sheet-file candidate))))
               ("Copies content of the sheet" . helm-sheet-copy))))
  "Show sheet list.")

;;;###autoload
(defun helm-sheet ()
  "Helm to list sheets and to create a new sheet."
  (interactive)
  (helm-other-buffer '(helm-source-sheet
                       helm-source-sheet-create)
                     "*helm sheet*"))

(provide 'helm-sheet)

;; Local Variables:
;; coding: utf-8
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; helm-sheet.el ends here
