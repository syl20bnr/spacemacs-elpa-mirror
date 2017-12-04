;;; total-lines.el --- Keep track of a buffer's total number of lines  -*- lexical-binding:t -*-

;; Copyright (C) 2017 Hinrik Örn Sigurðsson

;; Author: Hinrik Örn Sigurðsson
;; URL: https://github.com/hinrik/total-lines
;; Package-Version: 20171203.1649
;; Version: 0.1-git
;; Keywords: convenience mode-line
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
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

;;; Commentary:
;;
;; total-lines-mode provides a variable, `total-lines' which holds
;; the total number of lines in the current buffer at any given time.
;;
;; You could display it in your modeline, for instance.
;;
;;; Code:

(defvar-local total-lines nil
  "The total number of lines in the current buffer.")

(defun total-lines-init ()
  "Reset `total-lines' by scanning to the end of the buffer."
  (setq total-lines (line-number-at-pos (point-max) t)))

(defun total-lines-before-change-function (beg end)
  "Decrement `total-lines' in response to a text deletion.

BEG, END come from `after-change-functions'"
  (unless (= beg end)
    (let ((deleted-lines (1- (count-lines beg end))))
      (save-excursion
        (goto-char end)
        (beginning-of-line)
        (when (= (point) end)
          (setq deleted-lines (1+ deleted-lines))))
      (setq total-lines (- total-lines deleted-lines)))))

(defun total-lines-after-change-function (beg end old-length)
  "Increment `total-lines-count' in response to a text addition.

BEG and END, and OLD-LENGTH come from `before-change-functions'"
  (when (= old-length 0)
    (let ((added-lines (1- (count-lines beg end))))
      (save-excursion
        (goto-char end)
        (beginning-of-line)
        (when (= (point) end)
          (setq added-lines (1+ added-lines))))
      (setq total-lines (+ total-lines added-lines)))))

;;;###autoload
(define-minor-mode total-lines-mode
  "A minor mode that keeps track of the total number of lines in a buffer."
  :group 'total-lines
  (if total-lines-mode
      (progn
        (total-lines-init)
        (add-hook 'before-change-functions 'total-lines-before-change-function nil t)
        (add-hook 'after-change-functions 'total-lines-after-change-function nil t))
    (setq total-lines nil)
    (remove-hook 'before-change-functions 'total-lines-before-change-function t)
    (remove-hook 'after-change-functions 'total-lines-after-change-function t)))

;;;###autoload
(define-globalized-minor-mode global-total-lines-mode
  total-lines-mode
  total-lines-on
  :require 'total-lines)

(defun total-lines-on ()
  "Turn the mode on if we're in an appropriate buffer."
  (unless (minibufferp)
    (total-lines-mode 1)))

(provide 'total-lines)
;;; total-lines.el ends here
