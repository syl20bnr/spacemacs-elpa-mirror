;;; map-progress.el --- mapping macros that report progress

;; Copyright (C) 2010-2014  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/tarsius/map-progress/
;; Keywords: convenience
;; Package-Version: 20140310.2132

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package defines mapping macros that report progress.

;; For many of the standard and CL mapping functions like `mapc' macros
;; like `mapc-with-progress-reporter' are defined here.  The arguments
;; have the same meaning as the respective arguments of the mapping
;; function or of `make-progress-reporter', which ever has an argument by
;; the same name.

;; Even when the original mapping function supports multiple sequences the
;; macros defined here only support one.  All of `make-progress-reporter's
;; arguments except for MESSAGE are optional.  This includes the starting
;; and final state arguments.

;; All standard mapping function with exactly two mandatory arguments that
;; call the function applied to each element with exactly one argument are
;; supported by `map-with-progress-reporter', which can be used when no
;; progress reporting variant of that function has been defined here.  But
;; any optional arguments the original might have are not supported.

;;; TODO:

;; * support all builtin and cl mapping functions
;; * support FN that take any number of arguments
;; * support multiple sequences

;;; Code:

(require 'cl-lib)

(defmacro map-with-progress-reporter (msg map fn seq &optional min max &rest rest)
  "Apply FUNCTION to each element of SEQUENCE using mapping function MAP.
Report progress in the echo area.  Also see `make-progress-reporter'.

\(fn MESSAGE MAP FUNCTION SEQUENCE [MIN-VALUE MAX-VALUE CURRENT-VALUE MIN-CHANGE MIN-TIME])"
  (let ((idx (make-symbol "--map-with-progress-idx--"))
        (msm (make-symbol "--map-with-progress-msm--"))
        (lst (make-symbol "--map-with-progress-lst--"))
        (prg (make-symbol "--map-with-progress-prg--"))
        (elt (make-symbol "--map-with-progress-elt--")))
    `(let* ((,idx 0)
            (,msm ,msg)
            (,lst ,seq)
            (,prg (make-progress-reporter
                   ,msm (or ,min 0) (or ,max (length ,lst)) ,@rest)))
       (prog1 (funcall ,map (lambda (,elt)
                              (prog1 (funcall ,fn ,elt)
                                (progress-reporter-update ,prg (incf ,idx))))
                       ,lst)
         (progress-reporter-done ,prg)))))

(defmacro mapc-with-progress-reporter (msg fn seq &optional min max &rest rest)
  "Like `mapc' but report progress in the echo area.
Also see `make-progress-reporter'.

\(fn MESSAGE FUNCTION SEQUENCE [MIN-VALUE MAX-VALUE CURRENT-VALUE MIN-CHANGE MIN-TIME])"
  `(map-with-progress-reporter ,msg 'mapc ,fn ,seq ,min ,max ,@rest))

(defmacro mapcar-with-progress-reporter (msg fn seq &optional min max &rest rest)
  "Like `mapcar' but report progress in the echo area.
Also see `make-progress-reporter'.

\(fn MESSAGE FUNCTION SEQUENCE [MIN-VALUE MAX-VALUE CURRENT-VALUE MIN-CHANGE MIN-TIME])"
  `(map-with-progress-reporter ,msg 'mapcar ,fn ,seq ,min ,max ,@rest))

(defmacro mapatoms-with-progress-reporter (msg fn seq &optional min max &rest rest)
  "Like `mapatoms' but report progress in the echo area.
Also see `make-progress-reporter'.

\(fn MESSAGE FUNCTION [OBARRAY MIN-VALUE MAX-VALUE CURRENT-VALUE MIN-CHANGE MIN-TIME])"
  `(map-with-progress-reporter ,msg 'mapatoms ,fn ,seq ,min ,max ,@rest))

(defmacro mapcan-with-progress-reporter (msg fn seq &optional min max &rest rest)
  "Like `cl-mapcan' but report progress in the echo area.
There may be only one SEQUENCE.  Also see `make-progress-reporter'.

\(fn MESSAGE FUNCTION SEQUENCE [MIN-VALUE MAX-VALUE CURRENT-VALUE MIN-CHANGE MIN-TIME])"
  `(map-with-progress-reporter ,msg 'cl-mapcan ,fn ,seq ,min ,max ,@rest))

(defmacro mapcon-with-progress-reporter (msg fn seq &optional min max &rest rest)
  "Like `cl-mapcon' but report progress in the echo area.
There may be only one SEQUENCE.  Also see `make-progress-reporter'.

\(fn MESSAGE FUNCTION SEQUENCE [MIN-VALUE MAX-VALUE CURRENT-VALUE MIN-CHANGE MIN-TIME])"
  `(map-with-progress-reporter ,msg 'cl-mapcon ,fn ,seq ,min ,max ,@rest))

(defmacro mapl-with-progress-reporter (msg fn seq &optional min max &rest rest)
  "Like `cl-mapl' but report progress in the echo area.
There may be only one SEQUENCE.  Also see `make-progress-reporter'.

\(fn MESSAGE FUNCTION SEQUENCE [MIN-VALUE MAX-VALUE CURRENT-VALUE MIN-CHANGE MIN-TIME])"
  `(map-with-progress-reporter ,msg 'cl-mapl ,fn ,seq ,min ,max ,@rest))

(defmacro maplist-with-progress-reporter (msg fn seq &optional min max &rest rest)
  "Like `cl-maplist' but report progress in the echo area.
There may be only one SEQUENCE.  Also see `make-progress-reporter'.

\(fn MESSAGE FUNCTION SEQUENCE [MIN-VALUE MAX-VALUE CURRENT-VALUE MIN-CHANGE MIN-TIME])"
  `(map-with-progress-reporter ,msg 'cl-maplist ,fn ,seq ,min ,max ,@rest))

(defmacro mprg-with-message (message &rest body)
  "Display MESSAGE before and after executing the forms in BODY.

Display MESSAGE, with \"...\" respectively \"...done\" appended, before and
after evaluationg BODY using function `message' .  MESSAGE can also have
the form (SYMBOL MESSAGE) in which case SYMBOL is lexically bound to
\"MESSAGE...\".  The value of the last form in BODY is returned."
  (declare (indent 1))
  (let ((sym (if (listp message)
                 (prog1 (car message)
                   (setq message (cadr message)))
               (make-symbol "--with-message--"))))
    `(lexical-let ((,sym (concat ,message "...")))
       (message ,sym)
       (prog1 (progn ,@body)
         (message (concat ,sym "done"))))))

(provide 'map-progress)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; map-progress.el ends here
