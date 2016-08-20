;;; gplusify.el --- Add Google Plus markup to a piece of code

;; Copyright (C) 2012  Jorgen Schaefer <forcer@forcix.cx>

;; Version: 1.0
;; Package-Version: 20150312.1244
;; Author: Jorgen Schaefer <forcer@forcix.cx>
;; URL: https://github.com/jorgenschaefer/gplusify

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Google+ does not have any markup for program code, which is highly
;; annoying. This simple command will add some markup to share code
;; more easily on Google+, based on fontification.

;; Sadly, it will break if the code itself includes Google+ markup. As
;; there is no escape mechanism included in Google+, there's not much
;; we can do there.

;;; Code:

(defvar gplusify-face-highlights
  '((font-lock-keyword-face "*")
    (font-lock-function-name-face "_"))
  "Alist mapping face names to Google Plus characters.")

;;;###autoload
(defun gplusify-region (beg end)
  "Format region for Google Plus."
  (interactive "r")
  (save-excursion
    (setq beg (copy-marker beg)
          end (copy-marker end))
    (gplusify--faces beg end)
    (gplusify--spaces beg end)))

;;;###autoload
(defun gplusify-region-as-kill (beg end)
  "Save the gplusified version of the selected region to
the kill ring."
  (interactive "r")
  (let ((sel-text (buffer-substring beg end)))
    (with-temp-buffer
      (insert sel-text)
      (gplusify-region (point-min) (point-max))
      (kill-region (point-min) (point-max)))
    (deactivate-mark)))

(defun gplusify--spaces (beg end)
  "Replace indentation with NO-BREAK SPACE in the region."
  (untabify beg end)
  (goto-char beg)
  (while (re-search-forward "^ +" end t)
    (replace-match (make-string (length (match-string 0))
                                160 ; NO-BREAK SPACE
                                ))))

(defun gplusify--faces (beg end)
  "Add Google Plus highlighting to faces."
  (while (< beg end)
    (let* ((face (get-text-property beg 'face))
           (highlight (cadr (assq face gplusify-face-highlights)))
           this-end)
      (if (not highlight)
          (setq beg (next-single-property-change beg 'face nil end))
        (goto-char beg)
        (insert highlight)
        (goto-char (next-single-property-change (point) 'face nil end))
        (insert highlight)
        (setq beg (point))))))

(provide 'gplusify)
;;; gplusify.el ends here
