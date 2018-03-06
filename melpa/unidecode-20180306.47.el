;;; unidecode.el --- Transliterate Unicode to ASCII -*- lexical-binding: t -*-
;;
;; Copyright (C) 2013 sindikat
;; Copyright (C) 2018 sindikat, John Mastro
;;
;; Author: sindikat <sindikat at mail36 dot net>
;; Maintainer: John Mastro <john.b.mastro@gmail.com>
;; Version: 0.2
;; Package-Version: 20180306.47
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, version 2.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Transliterate Unicode characters into one of 128 ASCII characters.
;;
;; Two functions are provided that operate on strings (`unidecode' and
;; `unidecode-sanitize'), plus two that operate on buffer regions
;; (`unidecode-region' and `unidecode-sanitize-region')
;;
;; This package is an Emacs Lisp port of the Unidecode package for Python:
;; <http://pypi.python.org/pypi/Unidecode/>.
;;
;; See README.org for additional information.

;;; Code:

(defconst unidecode--data-directory
  (let ((file (or load-file-name buffer-file-name)))
    (expand-file-name "data" (file-name-directory (file-chase-links file)))))

(defvar unidecode--cache (make-hash-table :test 'eq :size 185))

(defun unidecode--read-file (file)
  (let ((read-circle t))
    (with-temp-buffer
      (insert-file-contents file)
      (read (current-buffer)))))

(defun unidecode--section-file (section)
  (expand-file-name (format "unidecode-x%03x.eld" section)
                    unidecode--data-directory))

(defun unidecode-region (beg end)
  "Transliterate Unicode chars between BEG and END to ASCII."
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (let (chr new)
      (while (setq chr (char-after))
        (setq new (cond ((< chr #x80)    ; ASCII
                         chr)
                        ((> chr #xEFFFF) ; PUA and above
                         nil)
                        (t
                         (let* ((section (lsh chr -8))
                                (position (mod chr 256))
                                (table (gethash section unidecode--cache)))
                           (unless (vectorp table)
                             (let ((file (unidecode--section-file section)))
                               (when (file-readable-p file)
                                 (setq table (unidecode--read-file file))
                                 (puthash section table unidecode--cache))))
                           (and (vectorp table)
                                (< position (length table))
                                (aref table position))))))
        (if (eq chr new)
            (forward-char 1)
          (delete-char 1)
          (when new (insert new)))))))

(defun unidecode (string)
  "Transliterate Unicode chars in STRING and return the result."
  (with-temp-buffer
    (insert string)
    (unidecode-region (point-min) (point-max))
    (buffer-string)))

;; Alias for backwards compatibility
(defalias 'unidecode-unidecode #'unidecode
  "Transliterate Unicode chars in STRING and return the result.")

(defun unidecode-sanitize-region (beg end)
  "Sanitize region between BEG and END.
Strip all characters that are not alphanumeric or hyphen, and
convert space to hyphen."
  (save-restriction
    (narrow-to-region beg end)
    (downcase-region (point-min) (point-max))
    (unidecode-region (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "[[:blank:]]" nil t)
      (replace-match "-"))
    (goto-char (point-min))
    (while (re-search-forward "[^a-z0-9-]+" nil t)
      (replace-match ""))))

(defun unidecode-sanitize (string)
  "Sanitize STRING and return the result.
Strip all characters that are not alphanumeric or hyphen, and
convert space to hyphen."
  (with-temp-buffer
    (insert string)
    (unidecode-sanitize-region (point-min) (point-max))
    (buffer-string)))

(provide 'unidecode)
;;; unidecode.el ends here
