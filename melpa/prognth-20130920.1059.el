;;; prognth.el --- Extend prog1 to arbitrary index

;; Copyright (C) 2013 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Keywords: lisp
;; Package-Version: 20130920.1059
;; Version: 1.0
;; Created: 20th Sep 2013

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

;; Emacs provides `prog1', `prog2' and `progn' to group statements and
;; return the value of first, second or last form.  This package
;; extends this notion to allow arbitrary index.  The macro `prognth'
;; takes as a first argument the index of the form whose value is to
;; be returned.  If the index is 1, 2 or greater than length of the
;; body, the standard emacs forms are used.  Otherwise, this
;; translates to (prog1 (progn ... first INDEX forms) rest)

;; Additionally, `progX' for X from 3 to 9 are generated for easier
;; usage.

;;; Code:

(defmacro prognth (index &rest body)
  "Eval first INDEX arguments sequentially, then eval the BODY.
The value of INDEX-th form is returned."
  (declare (indent 1)
           (debug (numberp body)))
  (let ((len (length body))
        (rest body)
        first-n)
    (cond
     ((>= index len)
      `(progn ,@body))
     ((= index 1)
      `(prog1 ,@body))
     ((= index 2)
      `(prog2 ,@body))
     (t
      (while (and (> index 0) rest)
        (push (car rest) first-n)
        (setq rest (cdr rest))
        (setq index (1- index)))
      `(prog1 (progn ,@(nreverse first-n)) ,@rest)))))

(let ((i 3))
  (while (< i 9)
    (eval `(defmacro ,(intern (format "prog%d" i)) (&rest body)
             ,(format "Eval first %d arguments sequentially, then eval the rest of the arguments.
The value of %dth argument is returned." i i)
             (declare (indent ,i)
                      (debug (body)))
             `(prognth ,,i ,@body)))
    (setq i (1+ i))))

(font-lock-add-keywords 'emacs-lisp-mode `((,(concat "("
                                                     (regexp-opt '("prognth"
                                                                   "prog3"
                                                                   "prog4"
                                                                   "prog5"
                                                                   "prog6"
                                                                   "prog7"
                                                                   "prog8"
                                                                   "prog9") t)
                                                     "\\>")
                                            (1 font-lock-keyword-face))))

(provide 'prognth)

;;; prognth.el ends here
