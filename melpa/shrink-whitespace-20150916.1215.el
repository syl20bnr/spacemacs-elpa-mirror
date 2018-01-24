;;; shrink-whitespace.el --- Whitespace removal DWIM key -*- coding: utf-8-unix -*-

;; Copyright (C) 2012 Jean-Christophe Petkovich <jcpetkovich@gmail.com>

;; Author   : Jean-Christophe Petkovich <jcpetkovich@gmail.com>
;; Created  : 6 June 2015
;; URL      : https://github.com/jcpetkovich/shrink-whitespace.el
;; Package-Version: 20150916.1215
;; Version  : 0.0.1
;; Keywords : editing

;; Please see README.md for documentation, or read it online at
;; https://github.com/jcpetkovich/shrink-whitespace.el

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Bind shrink-whitespace to a key, and start spamming it in places where you
;; want to remove whitespace. M-\ is not a bad key for this, as it does
;; something analagous, but isn't quite as smart.
;;
;; (global-set-key (kbd "M-\\") 'shrink-whitespace)
;; ----------------------------------------------------------

;;; Code:

;;;###autoload
(defun shrink-whitespace ()
  "Remove whitespace around cursor to just one or none.
If current line contains non-white space chars, then shrink any
whitespace char surrounding cursor to just one space.  If current
line does not contain non-white space chars, then remove blank
lines to just one."
  (interactive)
  (cond ((shrink-whitespace--just-one-space-p)
         (delete-horizontal-space))
        ((not (shrink-whitespace--line-has-meat-p))
         (delete-blank-lines))
        ((and (shrink-whitespace--line-has-meat-p)
              (or
               (looking-at " \\|\t")
               (looking-back " \\|\t")))
         (just-one-space))
        ((and (shrink-whitespace--line-has-meat-p)
              (looking-back "\n"))
         (delete-char -1))
        ((and (shrink-whitespace--line-has-meat-p)
              (looking-at "\n"))
         (delete-char 1))))

(defun shrink-whitespace--just-one-space-p ()
  "Return a truthy value if there is only one space at point."
  (save-excursion
    (let (beginning end)
      (skip-chars-backward " \t")
      (setf beginning (point))
      (skip-chars-forward " \t")
      (setf end (point))
      (= 1 (- end beginning)))))

(defun shrink-whitespace--line-has-meat-p ()
  "Return truthy if line at point has any characters, nil otherwise."
  (save-excursion
    (move-beginning-of-line 1)
    (let ((line-begin-pos (point))
          line-end-pos)
      (move-end-of-line 1)
      (setq line-end-pos (point))
      (< 0 (count-matches "[[:graph:]]" line-begin-pos line-end-pos)))))


(defun shrink-whitespace--open-line-above ()
  "Put a blank line above point."
  (beginning-of-line)
  (newline)
  (forward-line -1))

(defun shrink-whitespace--open-line-below ()
  "Put a blank line after point."
  (end-of-line)
  (newline)
  (indent-for-tab-command))

;;;###autoload
(defun shrink-whitespace-grow-whitespace-around ()
  "Counterpart to shrink-whitespace, grow whitespace in a smartish way."
  (interactive)
  (let ((content-above nil)
        (content-below nil))

    (save-excursion
      ;; move up a line and to the beginning
      (beginning-of-line 0)

      (when (shrink-whitespace--line-has-meat-p)
        (setq content-above t)))

    (save-excursion
      ;; move down a line and to the beginning
      (beginning-of-line 2)
      (when (shrink-whitespace--line-has-meat-p)
        (setq content-below t)))

    (save-excursion
      (if content-above
          (shrink-whitespace--open-line-above)
        (if content-below
            (shrink-whitespace--open-line-below))))
    (if (and (= (line-beginning-position) (point))
             content-above)
        (forward-line))))

;;;###autoload
(defun shrink-whitespace-shrink-whitespace-around ()
  "Shrink whitespace surrounding point."
  (interactive)
  (let ((content-above nil)
        (content-below nil))

    (save-excursion
      (beginning-of-line 0)
      (when (shrink-whitespace--line-has-meat-p)
        (setq content-above t)))

    (save-excursion
      (beginning-of-line 2)
      (when (shrink-whitespace--line-has-meat-p)
        (setq content-below t)))

    (save-excursion
      (if (not content-above)
          (progn
            (beginning-of-line 0)
            (kill-line))
        (if (not content-below)
            (progn
              (beginning-of-line 2)
              (kill-line)))))))

(defalias 'grow-whitespace-around 'shrink-whitespace-grow-whitespace-around)
(defalias 'shrink-whitespace-around 'shrink-whitespace-shrink-whitespace-around)

(provide 'shrink-whitespace)
;;; shrink-whitespace.el ends here
