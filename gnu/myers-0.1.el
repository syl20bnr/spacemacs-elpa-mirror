;;; myers.el --- Random-access singly-linked lists     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: list, containers
;; Package-Requires: ((emacs "25"))
;; Version: 0.1

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

;; This package implements Eugene W. Myers's "stacks" which are like
;; standard singly-linked lists, except that they also provide efficient
;; lookup.  More specifically:
;;
;; cons/car/cdr are O(1), while (nthcdr N L) is O(min (N, log L))
;;
;; For details, see "An applicative random-access stack", Eugene W. Myers,
;; 1983, Information Processing Letters
;; http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.188.9344&rep=rep1&type=pdf

;;; Code:

(require 'cl-lib)
(require 'seq)

(cl-defstruct (myers
               (:copier nil)
               (:constructor nil)
               (:conc-name myers--)
               (:constructor myers--cons (car cdr skip-distance skip)))
  (car nil :read-only t)
  (cdr nil :read-only t :type (or null myers))
  ;; Contrary to Myers's presentation, we index from the top of the stack,
  ;; and we don't store the total length but the "skip distance" instead.
  ;; This makes `cons' slightly faster, and better matches our use for
  ;; debruijn environments.
  (skip-distance nil :read-only t :type integer)
  (skip nil :read-only t :type (or null myers)))

(defun myers-cons (car cdr)
  "Create a new Myers cons, give it CAR and CDR as components, and return it.
This like `cons' but for Myers's lists."
  (if (null cdr)
      (myers--cons car cdr 1 cdr)
    (let ((s1 (myers--skip-distance cdr))
          (cddr (myers--skip cdr)))
      (if (null cddr)
          (myers--cons car cdr 1 cdr)
        (let ((s2 (myers--skip-distance cddr))
              (cdddr (myers--skip cddr)))
          (if (<= s2 s1)
              (myers--cons car cdr (+ 1 s1 s2) cdddr)
            (myers--cons car cdr 1 cdr)))))))

(defun myers-list (&rest objects)
  "Return a newly created list with specified arguments as elements."
  (let ((list nil))
    (dolist (x (nreverse objects))
      (setq list (myers-cons x list)))
    list))

;; FIXME: Should myers-car/cdr just defer to myers--car/cdr, or should they
;; reproduce car/cdr's behavior more faithfully and return nil when the arg
;; is nil?
(defalias 'myers-car #'myers--car)
(defalias 'myers-cdr #'myers--cdr)

(pcase-defmacro myers-cons (car cdr)
  `(cl-struct myers (car ,car) (cdr ,cdr)))

(defun myers-nthcdr (n list)
  "Take `myers-cdr' N times on LIST, return the result."
  (while (and (> n 0) list)
    (let ((s (myers--skip-distance list)))
      (if (<= s n)
          (setq n (- n s) list (myers--skip list))
        (setq n (- n 1) list (myers--cdr list)))))
  list)

;; This operation would be more efficient using Myers's choice of keeping
;; the length (instead of the skip-distance) in each node.
(cl-defmethod seq-length ((seq myers))
  (let ((n 0))
    (while seq
      (cl-incf n (myers--skip-distance seq))
      (setq seq (myers--skip seq)))
    n))

(cl-defmethod seq-elt ((seq myers) n)
  (let ((l (myers-nthcdr n seq)))
    (when l (myers--car l))))


(cl-defmethod seq-do (fun (seq myers))
  (while seq
    (funcall fun (myers--car seq))
    (setq seq (myers--cdr seq))))

(cl-defmethod seqp ((_seq myers)) t)

(cl-defmethod seq-copy ((seq myers))
  (let ((elts ()))
    (while seq
      (push (myers--car seq) elts)
      (setq seq (myers--cdr seq)))
    (dolist (elt elts)
      (setq seq (myers-cons elt seq)))
    seq))

(cl-defmethod seq-subseq ((seq myers) start &optional end)
  (when (< start 0)
    (let ((nstart (+ (seq-length seq) start)))
      (if (< nstart 0)
          (signal 'args-out-of-range (list seq start))
        (setq start nstart))))
  (setq seq (myers-nthcdr start seq))
  (if (null end)
      (seq-copy seq)
    (let ((nend (if (>= end 0)
                    (- end start)
                  (+ end (seq-length seq)))))
      (if (< nend 0)
          (signal 'args-out-of-range (list seq end))
        (setq end nend)))
    (let ((elts ())
          (res ()))
      (dotimes (_ end)
        (push (myers--car seq) elts)
        (setq seq (myers--cdr seq)))
      (dolist (elt elts)
        (setq res (myers-cons elt res)))
      res)))

(cl-defmethod seq-empty-p ((_seq myers)) nil)

(cl-defmethod seq-reverse ((seq myers))
  (let ((res ()))
    (while seq
      (setq res (myers-cons (myers--car seq) res))
      (setq seq (myers--cdr seq)))
    res))

(defun myers-find (pred list)
  "Find the first element of LIST for which PRED returns non-nil.
\"Binary\" search, assuming the list is \"sorted\" (i.e. all elements after
this one also return true).
Return the node holding that element (or nil, if none found)."
  (while
      (when list
        (if (funcall pred (myers--car list))
            nil
          (let ((l2 (myers--skip list)))
            (setq list (myers--cdr list))
            (if (eq l2 list)
                t
              (while
                  (and l2 (not (funcall pred (myers--car l2)))
                       (progn
                         (setq list (myers--cdr l2))
                         (setq l2 (myers--skip l2))
                         t))))
            t))))
  list)

;; (* Find the last node for which the predicate `p' is false.
;;  * "Binary" search, assuming the list is "sorted" (i.e. all elements after
;;  * this one also return true).  *)
;; let rec findcdr p l =
;;   let rec findcdr2 last l1 l2 =
;;     match l1,l2 with
;;     | _, (Mcons (x, l1, _, l2) as l) when not (p x) -> findcdr2 (Some l) l1 l2
;;     | l, _ -> findcdr1 last l
;;   and findcdr1 last l =
;;     match l with
;;      | Mnil -> last
;;      | Mcons (x, _, _, _) when p x -> last
;;      | Mcons (_, l1, _, l2) -> findcdr2 (Some l) l1 l2
;;   in findcdr1 None l

;;;; ChangeLog:

;; 2016-06-10  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* myers.el: New package
;; 



(provide 'myers)
;;; myers.el ends here
