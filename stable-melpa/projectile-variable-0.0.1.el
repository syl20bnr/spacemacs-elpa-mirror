;;; projectile-variable.el --- Store project local variables. -*- mode: lexical-binding: t -*-

;; Copyright (C) 2016 USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 11 Sep 2016
;; Version: 0.0.1
;; Package-Version: 0.0.1
;; Keywords: project, convenience
;; Homepage: https://github.com/zonuexe/projectile-variable
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (projectile "0.14.0"))

;; This file is NOT part of GNU Emacs.

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

;; Store project local variables (property) using Projectile and Symbol Plists.
;;
;; - https://github.com/bbatsov/projectile
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Symbol-Plists.html

;; (projectile-variable-put 'foo-value 2) ;; Store property
;; (projectile-variable-get 'foo-value)   ;;=> 2
;;
;; (projectile-variable-plist)        ;; Return all project local property list
;; (projectile-variable-plist "foo-") ;; Return project local property list filterd by prefix "foo-"
;; (projectile-variable-alist)        ;; Return all project local properties as association list (alist)
;; (projectile-variable-alist "foo-") ;; Return project local properties alist filterd by prefix "foo-"

;;; Code:
(require 'cl-lib)
(require 'projectile)

(defconst projectile-variable--prefix "projectile-variable--")

(defun projectile-variable--make-symbol ()
  "Make symbol for save project local variable."
  (intern (concat projectile-variable--prefix (projectile-project-root))))

(defun projectile-variable-plist (&optional prefix)
  "Return project local property list.  Fiter properties by prefix if PREFIX is not nil."
  (let ((plist (symbol-plist (projectile-variable--make-symbol)))
        filtered-plist)
    (if (null prefix)
        plist
      (cl-loop for (prop value) on plist by 'cddr
               if (string-prefix-p prefix (symbol-name prop))
               do (setq filtered-plist (plist-put filtered-plist prop value)))
      filtered-plist)))

(defun projectile-variable-alist (&optional prefix)
  "Return project local property list as alist.  Fiter properties by prefix if PREFIX is not nil."
  (let ((plist (symbol-plist (projectile-variable--make-symbol))))
    (cl-loop for (prop value) on plist by 'cddr
             if (or (null prefix) (string-prefix-p prefix (symbol-name prop)))
             collect (cons prop value))))

(defun projectile-variable-put (propname value)
  "Store the project local PROPNAME property with value VALUE."
  (put (projectile-variable--make-symbol) propname value))

(defun projectile-variable-get (propname)
  "Return the value of the project local PROPNAME property."
  (get (projectile-variable--make-symbol) propname))

(provide 'projectile-variable)
;;; projectile-variable.el ends here
