;;; helm-lib-babel.el --- helm insertion of babel function references

;; Author: Derek Feichtinger <dfeich@gmail.com>
;; Keywords: convenience
;; Package-Version: 20180225.1322
;; Package-Requires: ((cl-lib "0.5") (helm "1.9.2") (emacs "24.4"))
;; Homepage: https://github.com/dfeich/helm-lib-babel.el
;; Version: 1.0

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; Helm extension for inserting the reference to an org source block
;; function from the library of babel.
;;
;; The available functions consist of all named source
;; blocks in the current org file and of the functions defined in the
;; user's library of babel.

;;; Code:
(require 'helm)
(require 'org)
(require 'cl-lib)

;; Note: I cannot use lexical binding here since seemingly the lambdas which
;; I define below are handed over to helm and helm does not use lexical binding,
;; so I would then end with a "
;;;###autoload
(defun helm-lib-babel-insert ()
  "Helm function to insert a reference to an org source block function.

The available functions consist of all functions defined in the
library of babel (q.v. `org-babel-library-of-babel',
`org-babel-lob-ingest') as well as all the named source blocks
found in the current file.  The available actions include:

Insert a #+CALL: function.  The CALL function arguments are pre-filled
with the function's default arguments.

Insert a :post header argument for a source block

Insert an `org-sbe' form usually used in table functions.  Again,
the function default arguments are added as arguments to the
`org-sbe' call."
  (interactive)
  (helm
   :sources
   (let ((getname (lambda (x) (nth 5 (car x))))
	 (getvars (lambda (x) (cl-remove
			       :var (nth 3 (car x))
			       :test-not #'eql
			       :key #'car))))
     (helm-build-sync-source "available source block functions"
       :candidates (mapcar
		    (lambda (x) (cons (nth 5 x) `(,x)))
		    ;; we use the org-babel-lob-ingest function to also add the
		    ;; named src blocks in the present file to the existing
		    ;; library of babel functions
		    (cl-concatenate 'list org-babel-library-of-babel
				    (let ((org-babel-library-of-babel nil))
				      (org-babel-lob-ingest)
				      org-babel-library-of-babel)))
       :action `(("#+CALL statement" . (lambda (x)
					 (insert
					  (format "#+CALL: %s(%s)"
						  (funcall ,getname x)
						  ;; extract :var fields
						  (mapconcat
						   #'cdr
						   (funcall ,getvars x)
						   ", ")))))
		 (":post header directive" . (lambda (x)
					       (insert
						(format ":post %s(*this*)"
							(funcall ,getname x)))))
		 ("org-sbe form" . (lambda (x)
				     (insert
				      (format
				       "org-sbe %s %s"
				       (funcall ,getname x)
				       (mapconcat
					(lambda (var)
					  (apply #'format "(%s $%s)"
						 (split-string (cdr var) "=")))
					(funcall ,getvars x)
					" ")))))
		 ("function name" . (lambda (x) (insert
						 (funcall ,getname x)))))))
   :buffer "*helm insert library of babel call*"))

(provide 'helm-lib-babel)
;;; helm-lib-babel.el ends here
