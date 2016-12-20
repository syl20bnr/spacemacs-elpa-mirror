;;; python-switch-quotes.el --- cycle between ' and " quotes in python strings
;;; Copyright (C) 2016 Vladimir Lagunov

;;; Author: Vladimir Lagunov <lagunov.vladimir@gmail.com>
;;; Maintainer: Vladimir Lagunov <lagunov.vladimir@gmail.com>
;;; URL: https://github.com/werehuman/python-switch-quotes
;; Package-Version: 20161219.622
;;; Created: 2016-12-18
;;; Version: 0.1
;;; Keywords: python tools convenience
;;; Package-Requires: ((emacs "24.3"))

;;; This file is not part of GNU Emacs.

;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program. If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;   Converts strings like 'this' to strings like "this".
;;;   Supports raw strings, docstrings and strings with escaped quotes.
;;;
;;;   Assigns key ``C-c '`` to convert string at point.


;;; Code:
(require 'python)

(defun python-switch-quotes--simple (string-start string-end old-quote new-quote)
  "Private: Convert simple strings like \"hello world\" => 'hello world'.
Expected that string is between STRING-START and STRING-END.
OLD-QUOTE and NEW-QUOTE may be ?' or ?\"."
  (goto-char (1- string-end))
  (save-excursion
    (delete-char 1)
    (insert new-quote))
  (while (re-search-backward "[\"']" string-start t)
    (let ((return-back (point)) (found-char (char-after (point))))
      (delete-char 1)
      (cond ((eq found-char new-quote)
             ;; "hello >>'<<world" => 'hello \'world'
             (insert ?\\ new-quote))
            ((eq (char-before return-back) ?\\)
             ;; "hello >>\"<<world" => 'hello "world'
             (delete-char -1)
             (insert old-quote)
             (setq return-back (1- return-back)))
            (t
             ;; >>"<<hello world" => 'hello world
             (insert new-quote)))
      (goto-char return-back))))


(defun python-switch-quotes--raw-simple (string-start string-end new-quote)
  "Private: Convert simple strings like r\"hello world\" => r'hello world'.
Expected that string is between STRING-START and STRING-END.
NEW-QUOTE may be ?' or ?\"."
  (goto-char (1- string-end))
  (delete-char 1)
  (insert new-quote)
  (goto-char string-start)
  (delete-char 1)
  (insert new-quote))


(defun python-switch-quotes--docstring (string-start string-end old-quote new-quote)
  "Private: Convert docstrings like \"\"\"hello\"\"\" => '''hello'''.
Expected that string is between STRING-START and STRING-END.
OLD-QUOTE and NEW-QUOTE may be ?' or ?\"."
  (goto-char string-start)
  (delete-char 3)
  (insert new-quote new-quote new-quote)
  (goto-char string-end)
  (delete-char -3)
  (cond
   ((eq (char-before (point)) new-quote)
    ;;; """hello world'""" => '''hello world\''''
    (delete-char -1)
    (insert ?\\ new-quote new-quote new-quote new-quote)
    (goto-char (- (point) 2)))
   ((and (eq (char-before (point)) old-quote)
         (eq (char-before (1- (point))) ?\\))
    ;;; """hello world\"""" => '''hello world"'''
    (delete-char -2)
    (insert old-quote new-quote new-quote new-quote)
    (goto-char (1- (point))))
   (t
    ;;; """hello world""" => '''hello world'''
    (insert new-quote new-quote new-quote)))
  (goto-char (- (point) 3))
  (let ((re (regexp-opt '("'''" "\"\"\"")))
        (bound (+ 3 string-start)))
    (while (re-search-backward re bound t)
      (when (and (eq new-quote (char-after (point)))
                 (not (eq ?\\ (char-before (point)))))
        (save-excursion (insert ?\\))))))


(defun python-switch-quotes--raw-docstring (string-start string-end new-quote)
  "Private: Convert raw docstrings like r\"\"\"hello\"\"\" => r'''hello'''.
Expected that string is between STRING-START and STRING-END.
OLD-QUOTE and NEW-QUOTE may be ?' or ?\"."
  (goto-char (- string-end 3))
  (delete-char 3)
  (insert new-quote new-quote new-quote)
  (goto-char string-start)
  (delete-char 3)
  (insert new-quote new-quote new-quote))


;;;###autoload
(defun python-switch-quotes (&optional pos)
  "Convert apostrophe quoted string to quoted and vice versa.
POS - point inside of string, using current position if omitted."
  (interactive)
  (save-excursion
    (goto-char (or pos (point)))
    (let ((string-start (python-syntax-context 'string)))
      (if (not string-start) (error "Not a python string")
        (let* ((string-end (scan-sexps string-start 1))
               (old-quote (char-after string-start))
               (new-quote (if (equal old-quote ?\") ?' ?\"))
               (is-raw (memq (char-before string-start) '(?r ?R)))
               (is-docstring (equal (buffer-substring string-start (+ 3 string-start))
                                    (string old-quote old-quote old-quote))))
          (cond
           ((and is-raw is-docstring)
            (python-switch-quotes--raw-docstring string-start string-end new-quote))
           (is-docstring
            (python-switch-quotes--docstring string-start string-end old-quote new-quote))
           (is-raw
            (python-switch-quotes--raw-simple string-start string-end new-quote))
           (t
            (python-switch-quotes--simple string-start string-end old-quote new-quote))))))))

(provide 'python-switch-quotes)
;;; python-switch-quotes.el ends here
