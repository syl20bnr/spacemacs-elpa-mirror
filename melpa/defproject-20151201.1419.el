;;; defproject.el --- Manager dir-locals and project specific variables

;; Copyright (C) 2015 Chris Kotfila

;; Author:  <kotfic@gmail.com>
;; Version: 0.1.0
;; Package-Version: 20151201.1419
;; Package-Requires: ((emacs "24"))
;; Keywords: convenience
;; URL: https://github.com/kotfic/defproject


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

;;; Code:


(defun defproject--ismode? (symbol)
  "Predicate for determining if a SYMBOL is a mode symbol."
  (or (string-match-p ".*-mode$" (symbol-name symbol))
      (equal ":nil" (symbol-name symbol))))

(defun defproject--filter-plist (fn plist)
  "Apply FN, a lambda function with signatures (key value), to PLIST.
Remove elements that return nil."
  (let ((pl plist)
        (vals ()))
    (while pl
      (push (funcall fn (car pl) (cadr pl)) vals)
      (setq pl (cddr pl)))
    (delq nil (nreverse vals))))

(defun defproject-get-dir-locals (args)
  "Filter ARGS, returning cons cels of (PROPERTY . VALUE) for mode properties."
  (defproject--filter-plist (lambda (key val)
                  (when (defproject--ismode? key)
                    (cons key val))) args))

(defun defproject-eval-dir-locals (args)
  "Map over each (MODE . VARS) in dir-locals like list ARGS.
Evaluate each cdr in VARS list unless the car is `eval'."
  ;; for each mode and its var/value list
  (mapcar (lambda (mode_args)
          (let ((mode (car mode_args))
                (body (cdr mode_args)))
            ;; for each var and its value
            (cons (intern (replace-regexp-in-string "^:" "" (symbol-name mode)))
                  (mapcar (lambda (var_val)
                          (let ((var (car var_val))
                                (val (cdr var_val)))
                            ;; if first element is 'eval pass through
                            ;; otherwise evaluate the cdr
                            (if (eq var 'eval)
                                var_val
                              (cons var (eval val))))) body)))) args))

(defmacro defproject(project-name &rest args)
  "Define a project of type PROJECT-NAME.  ARGS is a plist which
requires a :path symbol and value as well as mode symbols (e.g.,
:python-mode). Also accepts :vars property and :init property. Code
found in the :vars property should be a list of variables for use in
the bodies of the mode properties. :init allows for additional code
to be executed conditional on the existence of the path defined in
the :path property."
  (declare (indent 1))
  `(let* ((project-path ,(plist-get args :path))
          ,@(plist-get args :vars)
          (dir-locals (defproject-eval-dir-locals
                        (quote ,(defproject-get-dir-locals args)))))
     (when dir-locals
       (mapcar (lambda(class-vars-list)
                 (mapcar (lambda(class-var)
                           (add-to-list 'safe-local-variable-values class-var))
                         (cdr class-vars-list))) dir-locals))

       (dir-locals-set-class-variables (quote ,project-name)
                                       dir-locals)
     (when (file-exists-p project-path)
       (dir-locals-set-directory-class project-path (quote ,project-name))
       ,@(plist-get args :init))))

(provide 'defproject)

;;; defproject.el ends here
