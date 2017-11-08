;;; trie.el --- Trie data structure  -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2010, 2012, 2014, 2017  Free Software Foundation, Inc

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.4
;; Keywords: extensions, matching, data structures
;;           trie, ternary search tree, tree, completion, regexp
;; Package-Requires: ((tNFA "0.1.1") (heap "0.3"))
;; URL: http://www.dr-qubit.org/emacs.php
;; Repository: http://www.dr-qubit.org/git/predictive.git

;; This file is part of Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; Quick Overview
;; --------------
;; A trie is a data structure used to store keys that are ordered sequences of
;; elements (vectors, lists or strings in Elisp; strings are by far the most
;; common), in such a way that both storage and retrieval are space- and
;; time-efficient. But, more importantly, a variety of more advanced queries
;; can also be performed efficiently: for example, returning all strings with
;; a given prefix, searching for keys matching a given wildcard pattern or
;; regular expression, or searching for all keys that match any of the above
;; to within a given Lewenstein distance.
;;
;; You create a trie using `make-trie', create an association using
;; `trie-insert', retrieve an association using `trie-lookup', and map over a
;; trie using `trie-map', `trie-mapc', `trie-mapcar', or `trie-mapf'. You can
;; find completions of a prefix sequence using `trie-complete', search for
;; keys matching a regular expression using `trie-regexp-search', find fuzzy
;; matches within a given Lewenstein distance (edit distance) of a string
;; using `trie-fuzzy-match', and find completions of prefixes within a given
;; distance using `trie-fuzzy-complete'.
;;
;; Using `trie-stack', you can create an object that allows the contents of
;; the trie to be used like a stack, useful for building other algorithms on
;; top of tries; `trie-stack-pop' pops elements off the stack one-by-one, in
;; "lexicographic" order, whilst `trie-stack-push' pushes things onto the
;; stack. Similarly, `trie-complete-stack', `trie-regexp-stack',
;; `trie-fuzzy-match-stack' and `trie-fuzzy-complete-stack' create
;; "lexicographicly-ordered" stacks of query results.
;;
;; Very similar to trie-stacks, `trie-iter', `trie-complete-iter',
;; `trie-regexp-iter', `trie-fuzzy-match-iter' and `trie-fuzzy-complete-iter'
;; generate iterator objects, which can be used to retrieve successive
;; elements by calling `iter-next' on them.
;;
;; Note that there are two uses for a trie: as a lookup table, in which case
;; only the presence or absence of a key in the trie is significant, or as an
;; associative array, in which case each key carries some associated
;; data. Libraries for other data structure often only implement lookup
;; tables, leaving it up to you to implement an associative array on top of
;; this (by storing key+data pairs in the data structure's keys, then defining
;; a comparison function that only compares the key part). For a trie,
;; however, the underlying data structures naturally support associative
;; arrays at no extra cost, so this package does the opposite: it implements
;; associative arrays, and leaves it up to you to use them as lookup tables if
;; you so desire, by ignoring the associated data.
;;
;;
;; Different Types of Trie
;; -----------------------
;; There are numerous ways to implement trie data structures internally, each
;; with its own time- and space-efficiency trade-offs. By viewing a trie as a
;; tree whose nodes are themselves lookup tables for key elements, this
;; package is able to support all types of trie in a uniform manner. This
;; relies on there existing (or you writing!) an Elisp implementation of the
;; corresponding type of lookup table. The best type of trie to use will
;; depend on what trade-offs are appropriate for your particular
;; application. The following gives an overview of the advantages and
;; disadvantages of various types of trie. (Not all of the underlying lookup
;; tables have been implemented in Elisp yet, so using some of the trie types
;; described below would require writing the missing Elisp package!)
;;
;;
;; One of the most effective all-round implementations of a trie is a ternary
;; search tree, which can be viewed as a tree of binary trees. If basic binary
;; search trees are used for the nodes of the trie, we get a standard ternary
;; search tree. If self-balancing binary trees are used (e.g. AVL or red-black
;; trees), we get a self-balancing ternary search tree. If splay trees are
;; used, we get yet another self-organising variant of a ternary search
;; tree. All ternary search trees have, in common, good space-efficiency. The
;; time-efficiency of the various trie operations is also good, assuming the
;; underlying binary trees are balanced. Under that assumption, all variants
;; of ternary search trees described below have the same asymptotic
;; time-complexity for all trie operations.
;;
;; Self-balancing trees ensure the underlying binary trees are always close to
;; perfectly balanced, with the usual trade-offs between the different the
;; types of self-balancing binary tree: AVL trees are slightly more efficient
;; for lookup operations than red-black trees, at a cost of slightly less
;; efficienct insertion operations, and less efficient deletion
;; operations. Splay trees give good average-case complexity and are simpler
;; to implement than AVL or red-black trees (which can mean they're faster in
;; practice), at the expense of poor worst-case complexity.
;;
;; If your tries are going to be static (i.e. created once and rarely
;; modified), then using perfectly balanced binary search trees might be
;; appropriate. Perfectly balancing the binary trees is very inefficient, but
;; it only has to be done when the trie is first created or modified. Lookup
;; operations will then be as efficient as possible for ternary search trees,
;; and the implementation will also be simpler (so probably faster) than a
;; self-balancing tree, without the space and time overhead required to keep
;; track of rebalancing.
;;
;; On the other hand, adding data to a binary search tree in a random order
;; usually results in a reasonably balanced tree. If this is the likely
;; scenario, using a basic binary tree without bothering to balance it at all
;; might be quite efficient, and, being even simpler to implement, could be
;; quite fast overall.
;;
;;
;; A digital trie is a different implementation of a trie, which can be viewed
;; as a tree of arrays, and has different space- and time-complexities than a
;; ternary search tree. Roughly speaking, a digital trie has worse
;; space-complexity, but better time-complexity. Using hash tables instead of
;; arrays for the nodes gives something similar to a digital trie, potentially
;; with better space-complexity and the same amortised time-complexity, but at
;; the expense of occasional significant inefficiency when inserting and
;; deleting (whenever a hash table has to be resized). Indeed, an array can be
;; viewed as a perfect hash table, but as such it requires the number of
;; possible values to be known in advance.
;;
;; Finally, if you really need optimal efficiency from your trie, you could
;; even write a custom type of underlying lookup table, optimised for your
;; specific needs.
;;
;; This package uses the AVL tree package avl-tree.el, the tagged NFA package
;; tNFA.el, and the heap package heap.el.


;;; Code:

(eval-when-compile (require 'cl))
(require 'avl-tree)
(require 'heap)
(require 'tNFA)




;;; ================================================================
;;;                   Pre-defined trie types

(defconst trie--types '(avl))


;; --- avl-tree ---
(put 'avl :trie-createfun
     (lambda (cmpfun _seq) (avl-tree-create cmpfun)))
(put 'avl :trie-insertfun 'avl-tree-enter)
(put 'avl :trie-deletefun 'avl-tree-delete)
(put 'avl :trie-lookupfun 'avl-tree-member)
(put 'avl :trie-mapfun 'avl-tree-mapc)
(put 'avl :trie-emptyfun 'avl-tree-empty)
(put 'avl :trie-stack-createfun 'avl-tree-stack)
(put 'avl :trie-stack-popfun 'avl-tree-stack-pop)
(put 'avl :trie-stack-emptyfun 'avl-tree-stack-empty-p)
(put 'avl :trie-transform-for-print 'trie--avl-transform-for-print)
(put 'avl :trie-transform-from-read 'trie--avl-transform-from-read)




;;; ================================================================
;;;           Internal utility functions and macros

;; symbol used to denote a trie leaf node
(defconst trie--terminator '--trie--terminator)


(defmacro trie--if-lexical-binding (then else)
  "If lexical binding is in effect, evaluate THEN, otherwise ELSE."
  (declare (indent 1) (debug t))
  (if (let ((tempvar nil)
	    (f (let ((tempvar t)) (lambda () tempvar))))
	tempvar  ;; shut up "unused lexical variable" byte-compiler warning
	(funcall f))
      then else))


;; wrap CMPFUN for use in a subtree
(trie--if-lexical-binding
    (defun trie--wrap-cmpfun (cmpfun)
      (lambda (a b)
	(setq a (trie--node-split a)
	      b (trie--node-split b))
	(cond ((eq a trie--terminator)
	       (if (eq b trie--terminator) nil t))
	      ((eq b trie--terminator) nil)
	      (t (funcall cmpfun a b)))))
  (defun trie--wrap-cmpfun (cmpfun)
    `(lambda (a b)
       (setq a (trie--node-split a)
	     b (trie--node-split b))
       (cond ((eq a trie--terminator)
	      (if (eq b trie--terminator) nil t))
	     ((eq b trie--terminator) nil)
	     (t (,cmpfun a b))))))


;; create equality function from trie comparison function
(trie--if-lexical-binding
    (defun trie--construct-equality-function (comparison-function)
      (lambda (a b)
	 (not (or (funcall comparison-function a b)
		  (funcall comparison-function b a)))))
  (defun trie--construct-equality-function (comparison-function)
    `(lambda (a b)
       (not (or (,comparison-function a b)
		(,comparison-function b a))))))


;; create Lewenstein rank function from trie comparison function
(trie--if-lexical-binding
    (defun trie--construct-Lewenstein-rankfun (comparison-function)
      (let ((compfun (trie-construct-sortfun comparison-function)))
	(lambda (a b)
	  (cond
	   ((< (nth 1 (car a)) (nth 1 (car b))) t)
	   ((> (nth 1 (car a)) (nth 1 (car b))) nil)
	   (t (funcall compfun (nth 0 (car a)) (nth 0 (car b))))))))
  (defun trie--construct-Lewenstein-rankfun (comparison-function)
    `(lambda (a b)
       (cond
	((< (nth 1 (car a)) (nth 1 (car b))) t)
	((> (nth 1 (car a)) (nth 1 (car b))) nil)
	(t ,(trie-construct-sortfun comparison-function)
	   (nth 0 (car a)) (nth 0 (car b)))))))




;;; ----------------------------------------------------------------
;;;           Functions and macros for handling a trie.

(defstruct
  (trie-
   :named
   (:constructor nil)
   (:constructor trie--create
		 (comparison-function &optional (type 'avl)
		  &aux
		  (_dummy
		   (or (memq type trie--types)
		       (error "trie--create: unknown trie TYPE, %s" type)))
		  (createfun (get type :trie-createfun))
		  (insertfun (get type :trie-insertfun))
		  (deletefun (get type :trie-deletefun))
		  (lookupfun (get type :trie-lookupfun))
		  (mapfun (get type :trie-mapfun))
		  (emptyfun (get type :trie-emptyfun))
		  (stack-createfun (get type :trie-stack-createfun))
		  (stack-popfun (get type :trie-stack-popfun))
		  (stack-emptyfun (get type :trie-stack-emptyfun))
		  (transform-for-print (get type :trie-transform-for-print))
		  (transform-from-read (get type :trie-transform-from-read))
		  (cmpfun (trie--wrap-cmpfun comparison-function))
		  (root (trie--node-create-root createfun cmpfun))
		  ))
   (:constructor trie--create-custom
		 (comparison-function
		  &key
		  (createfun #'avl-tree-create-bare)
		  (insertfun #'avl-tree-enter)
		  (deletefun #'avl-tree-delete)
		  (lookupfun #'avl-tree-member)
		  (mapfun #'avl-tree-mapc)
		  (emptyfun #'avl-tree-empty)
		  (stack-createfun #'avl-tree-stack)
		  (stack-popfun #'avl-tree-stack-pop)
		  (stack-emptyfun #'avl-tree-stack-empty-p)
		  (transform-for-print #'trie--avl-transform-for-print)
		  (transform-from-read #'trie--avl-transform-from-read)
		  &aux
		  (cmpfun (trie--wrap-cmpfun comparison-function))
		  (root (trie--node-create-root createfun cmpfun))
		  ))
   (:copier nil))
  root comparison-function cmpfun
  createfun insertfun deletefun lookupfun mapfun emptyfun
  stack-createfun stack-popfun stack-emptyfun
  transform-for-print transform-from-read print-form)




;;; ----------------------------------------------------------------
;;;          Functions and macros for handling a trie node.

(defstruct
  (trie--node
   (:type vector)
   (:constructor nil)
   (:constructor trie--node-create
		 (split seq trie
		  &aux (subtree (funcall (trie--createfun trie)
					 (trie--cmpfun trie) seq))))
   (:constructor trie--node-create-data
		 (data &aux (split trie--terminator) (subtree data)))
   (:constructor trie--node-create-dummy
		 (split &aux (subtree nil)))
   (:constructor trie--node-create-root
		 (createfun cmpfun
		  &aux
		  (split nil)
		  (subtree (funcall createfun cmpfun []))))
   (:copier nil))
   split subtree)

;; data is stored in the subtree cell of a terminal node
(defalias 'trie--node-data 'trie--node-subtree)

(defsetf trie--node-data (node) (data)
  `(setf (trie--node-subtree ,node) ,data))

(defsubst trie--node-data-p (node)
  ;; Return t if NODE is a data node, nil otherwise.
  (eq (trie--node-split node) trie--terminator))

(defsubst trie--node-p (node)
  ;; Return t if NODE is a TRIE trie--node, nil otherwise.  Have to
  ;; define this ourselves, because we created a defstruct without any
  ;; identifying tags (i.e. (:type vector)) for efficiency, but this
  ;; means we can only perform a rudimentary and very unreliable test.
  (and (vectorp node) (= (length node) 2)))


(defun trie--node-find (node seq lookupfun)
  ;; Returns the node below NODE corresponding to SEQ, or nil if none
  ;; found.
  (let ((i -1))
    ;; descend trie until we find SEQ or run out of trie
    (while (and node (< (incf i) (length seq)))
      (setq node
	    (funcall lookupfun
		     (trie--node-subtree node)
		     (trie--node-create-dummy (elt seq i))
		     nil)))
    node))


(defsubst trie--find-data-node (node lookupfun)
  ;; Return data node from NODE's subtree, or nil if NODE has no data
  ;; node in its subtree.
  (funcall lookupfun
	   (trie--node-subtree node)
	   (trie--node-create-dummy trie--terminator)
	   nil))


(defsubst trie--find-data (node lookupfun)
  ;; Return data associated with sequence corresponding to NODE, or nil
  ;; if sequence has no associated data.
  (let ((node (trie--find-data-node node lookupfun)))
    (when node (trie--node-data node))))




;;; ----------------------------------------------------------------
;;;              print/read transformation functions

(defun trie-transform-for-print (trie)
  "Transform TRIE to print form."
  (when (trie--transform-for-print trie)
    (if (trie--print-form trie)
	(warn "Trie has already been transformed to print-form")
      (funcall (trie--transform-for-print trie) trie)
      (setf (trie--print-form trie) t))))


(defun trie-transform-from-read (trie)
  "Transform TRIE from print form."
  (when (trie--transform-from-read trie)
    (if (not (trie--print-form trie))
	(warn "Trie is not in print-form")
      (funcall (trie--transform-from-read trie) trie)
      (setf (trie--print-form trie) nil))))


(defsubst trie-transform-from-read-warn (trie)
  "Transform TRIE from print form, with warning."
  (when (trie--print-form trie)
    (warn (concat "Attempt to operate on trie in print-form;\
 converting to normal form"))
    (trie-transform-from-read trie)))


(defun trie--avl-transform-for-print (trie)
  ;; transform avl-tree based TRIE to print form.
  (trie-mapc-internal
   (lambda (avl _seq) (setf (avl-tree--cmpfun avl) nil))
   trie))


(defun trie--avl-transform-from-read (trie)
  ;; transform avl-tree based TRIE from print form."
  (let ((--trie-avl-transform--cmpfun (trie--cmpfun trie)))
    (trie-mapc-internal
     (lambda (avl _seq)
       (setf (avl-tree--cmpfun avl) --trie-avl-transform--cmpfun))
     trie)))




;;; ----------------------------------------------------------------
;;;                Replacements for CL functions

;; copied from cl-extra.el
(defun trie--subseq (seq start &optional end)
  "Return the subsequence of SEQ from START to END.
If END is omitted, it defaults to the length of the sequence.
If START or END is negative, it counts from the end."
  (if (stringp seq) (substring seq start end)
    (let (len)
      (and end (< end 0) (setq end (+ end (setq len (length seq)))))
      (when (< start 0)
	(setq start (+ start (or len (setq len (length seq))))))
      (cond ((listp seq)
	     (if (> start 0) (setq seq (nthcdr start seq)))
	     (if end
		 (let ((res nil))
		   (while (>= (setq end (1- end)) start)
		     (push (pop seq) res))
		   (nreverse res))
	       (copy-sequence seq)))
	    (t
	     (or end (setq end (or len (length seq))))
	     (let ((res (make-vector (max (- end start) 0) nil))
		   (i 0))
	       (while (< start end)
		 (aset res i (aref seq start))
		 (setq i (1+ i) start (1+ start)))
	       res))))))


(defun trie--position (item list)
  "Find the first occurrence of ITEM in LIST.
Return the index of the matching item, or nil of not found.
Comparison is done with `equal'."
  (let ((i 0))
    (catch 'found
      (while (progn
	       (when (equal item (car list)) (throw 'found i))
	       (setq i (1+ i))
	       (setq list (cdr list))))
      nil)))


(defsubst trie--seq-append (seq el)
  "Append EL to the end of sequence SEQ."
  (cond
   ((stringp seq) (concat seq (string el)))
   ((vectorp seq) (vconcat seq (vector el)))
   ((listp seq)	  (append seq (list el)))))


(defsubst trie--seq-concat (seq &rest sequences)
  "Concatenate SEQ and SEQUENCES, and make the result the same
type of sequence as SEQ."
  (cond
   ((stringp seq) (apply #'concat  seq sequences))
   ((vectorp seq) (apply #'vconcat seq sequences))
   ((listp seq)	  (apply #'append  seq sequences))))




;;; ================================================================
;;;                     Basic trie operations

;;;###autoload
(defalias 'make-trie 'trie--create
  "Return a new trie that uses comparison function COMPARISON-FUNCTION.

A trie stores sequences (strings, vectors or lists) along with
associated data. COMPARISON-FUNCTEION should accept two
arguments, each being an element of such a sequence, and return t
if the first is strictly smaller than the second.

The optional argument TYPE specifies the type of trie to
create. However, the only one that is currently implemented is
the default, so this argument is useless for now.

\(See also `make-trie-custom'.\)")


;;;###autoload
(defalias 'trie-create 'make-trie)


;;;###autoload
(defalias 'make-trie-custom 'trie--create-custom
  "Return a new trie that uses comparison function COMPARISON-FUNCTION.

A trie stores sequences (strings, vectors or lists) along with
associated data. COMPARISON-FUNCTION should accept two arguments,
each being an element of such a sequence, and return t if the
first is strictly smaller than the second.

The remaining keyword arguments: :CREATEFUN, :INSERTFUN, :DELETEFUN,
:LOOKUPFUN, :MAPFUN, :EMPTYFUN, :STACK-CREATEFUN, :STACK-POPFUN,
:STACK-EMPTYFUN, :TRANSFORM-FOR-PRINT and :TRANSFORM-FROM-READ
determine the type of trie that is created.

CREATEFUN is called as follows:

  (CREATEFUN COMPARISON-FUNCTION SEQ)

and should return a data structure (\"ARRAY\") that can be used
as an associative array, where two elements A and B are equal if
the following is non-nil:

  (and (COMPARISON-FUNCTION b a)
       (COMPARISON-FUNCTION b a))

The SEQ argument is a vector containing the sequence that will
correspond to the newly created array in the trie. For most types
of trie, this value is ignored. It is passed to CREATEFUN only in
order to allow the creation of \"hybrid\" trie structures, in
which different types of associative array are used in different
parts of the trie. For example, the type of associative array
could be chosen based on the depth in the trie, given by \(length
SEQ\). (Note that all the other functions described below must be
able to correctly handle *any* of the types of associate array
that might be created by CREATEFUN.)

INSERTFUN, DELETEFUN, LOOKUPFUN, MAPFUN and EMPTYFUN should
insert, delete, lookup, map over, and check-if-there-exist-any
elements in an associative array. They are called as follows:

  (INSERTFUN array element &optional updatefun)
  (DELETEFUN array element &optional predicate nilflag)
  (LOOKUPFUN array element &optional nilflag)
  (MAPFUN function array &optional reverse)
  (EMPTYFUN array)

INSERTFUN should insert ELEMENT into ARRAY and return the new
element, which will be ELEMENT itself unless UPDATEFUN is
specified. In that case, if and only if an element matching
ELEMENT already exists in the associative array, INSERTFUN should
instead pass ELEMENT and the matching element as arguments to
UPDATEFUN, replace the matching element with the return value,
and return that return value.

DELETEFUN should delete the element in the associative array that
matches ELEMENT, and return the deleted element. However, if
PREDICATE is specified and a matching element exists in ARRAY,
DELETEFUN should first pass the matching element as an argument
to PREDICATE before deleting, and should only delete the element
if PREDICATE returns non-nil. DELETEFUN should return NILFLAG if
no element was deleted (either becuase no matching element was
found, or because TESTFUN returned nil).

LOOKUPFUN should return the element from the associative array
that matches ELEMENT, or NILFLAG if no matching element exists.

MAPFUN should map FUNCTION over all elements in the order defined by
COMPARISON-FUNCTION, or in reverse order if REVERSE is non-nil.


STACK-CREATEFUN, STACK-POPFUN and STACK-EMPTYFUN should allow the
associative array to be used as a stack. STACK-CREATEFUN is
called as follows:

  (STACK-CREATEFUN array)

and should return a data structure (\"STACK\") that behaves like
a sorted stack of all elements in the associative array. I.e.
successive calls to

  (STACK-POPFUN stack)

should return elements from the associative array in the order
defined by COMPARISON-FUNCTION, and

  (STACK-EMPTYFUN stack)

should return non-nil if the stack is empty, nil otherwise.

The stack functions are optional, in that all trie operations
other than the stack-related ones will work correctly. However,
any code that makes use of trie-stacks will complain if supplied
with this type of trie.


The :TRANSFORM-FOR-PRINT and :TRANSFORM-FROM-READ arguments are
optional. If supplied, they can be used to transform the trie
into a format suitable for passing to Elisp's `print'
functions (typically used to persistently store the trie by
writing it to file), and transform from that format back to the
original usable form.


Warning: to avoid nasty dynamic scoping bugs, the supplied
functions must *never* bind any variables with names commencing
\"--\".")


;;;###autoload
(defalias 'trie-create-custom 'make-trie-custom)



(defalias 'trie-comparison-function 'trie--comparison-function
  "Return the comparison function for TRIE.")


(defalias 'trie-p 'trie--p
  "Return t if argument is a trie, nil otherwise.")


(defun trie-empty (trie)
  "Return t if the TRIE is empty, nil otherwise."
  (trie-transform-from-read-warn trie)
  (funcall (trie--emptyfun trie)
	   (trie--node-subtree (trie--root trie))))


(trie--if-lexical-binding
    (defun trie-construct-sortfun (cmpfun &optional reverse)
      "Construct function to compare key sequences, based on a CMPFUN
that compares individual elements of the sequence. Order is
reversed if REVERSE is non-nil."
      (if reverse
	  (lambda (a b)
	    (catch 'compared
	      (dotimes (i (min (length a) (length b)))
		(cond ((funcall cmpfun (elt b i) (elt a i))
		       (throw 'compared t))
		      ((funcall cmpfun (elt a i) (elt b i))
		       (throw 'compared nil))))
	      (< (length a) (length b))))
	(lambda (a b)
	  (catch 'compared
	    (dotimes (i (min (length a) (length b)))
	      (cond ((funcall cmpfun (elt a i) (elt b i))
		     (throw 'compared t))
		    ((funcall cmpfun (elt b i) (elt a i))
		     (throw 'compared nil))))
	    (< (length a) (length b))))))

  (defun trie-construct-sortfun (cmpfun &optional reverse)
    "Construct function to compare key sequences, based on a CMPFUN
that compares individual elements of the sequence. Order is
reversed if REVERSE is non-nil."
    (if reverse
	`(lambda (a b)
	   (catch 'compared
	     (dotimes (i (min (length a) (length b)))
	       (cond ((,cmpfun (elt b i) (elt a i))
		      (throw 'compared t))
		     ((,cmpfun (elt a i) (elt b i))
		      (throw 'compared nil))))
	     (< (length a) (length b))))
      `(lambda (a b)
	 (catch 'compared
	   (dotimes (i (min (length a) (length b)))
	     (cond ((,cmpfun (elt a i) (elt b i))
		    (throw 'compared t))
		   ((,cmpfun (elt b i) (elt a i))
		    (throw 'compared nil))))
	   (< (length a) (length b))))))
)




;; ----------------------------------------------------------------
;;                        Inserting data

(defun trie-insert (trie key &optional data updatefun)
  "Associate DATA with KEY in TRIE.

If KEY already exists in TRIE, then DATA replaces the existing
association, unless UPDATEFUN is supplied. Note that if DATA is
*not* supplied, this means that the existing association of KEY
will be replaced by nil.

If UPDATEFUN is supplied and KEY already exists in TRIE,
UPDATEFUN is called with two arguments: DATA and the existing
association of KEY. Its return value becomes the new association
for KEY.

Returns the new association of KEY.

Note: to avoid nasty dynamic scoping bugs, UPDATEFUN must *not*
bind any variables with names commencing \"--\"."

  ;; convert trie from print-form if necessary
  (trie-transform-from-read-warn trie)

  ;; absurd variable names are an attempt to avoid dynamic scoping bugs
  (let ((--trie-insert--updatefun updatefun)
	--trie-insert--old-node-flag
	(node (trie--root trie))
	(len (length key))
	(i -1))
    ;; Descend trie, adding nodes for non-existent elements of KEY. The
    ;; update function passed to `trie--insertfun' ensures that existing
    ;; nodes are left intact.
    (while (< (incf i) len)
      (setq --trie-insert--old-node-flag nil)
      (setq node (funcall (trie--insertfun trie)
			  (trie--node-subtree node)
			  (trie--node-create (elt key i) key trie)
			  (lambda (_a b)
			    (setq --trie-insert--old-node-flag t) b))))
    ;; Create or update data node.
    (setq node (funcall (trie--insertfun trie)
			(trie--node-subtree node)
			(trie--node-create-data data)
			;; if using existing data node, wrap UPDATEFUN
			;; if any was supplied
			(when (and --trie-insert--old-node-flag
				   --trie-insert--updatefun)
			  (lambda (new old)
			    (setf (trie--node-data old)
				  (funcall --trie-insert--updatefun
					   (trie--node-data new)
					   (trie--node-data old)))
			    old))))
    (trie--node-data node)))  ; return new data




;; ----------------------------------------------------------------
;;                        Deleting data

;; The absurd argument names are to lessen the likelihood of dynamical scoping
;; bugs caused by a supplied function binding a variable with the same name as
;; one of the arguments, which would cause a nasty bug when they're called.
;; FIXME: not needed with lexical binding
(defun trie--do-delete (node --trie--do-delete--seq
			     --trie--do-delete--test
			     --trie--do-delete--deletefun
			     --trie--do-delete--emptyfun
			     --trie--do-delete--cmpfun
			     --trie--do-delete--key)
  ;; Delete --TRIE--DO-DELETE--SEQ starting from trie node NODE, and
  ;; return non-nil if we deleted a node. If --TRIE--DO-DELETE--TEST is
  ;; supplied, it is called with two arguments, the key being deleted
  ;; and the associated data, and the deletion is only carried out if it
  ;; returns non-nil.

  ;; if --TRIE--DO-DELETE--SEQ is empty, try to delete data node and
  ;; return non-nil if we did (return value of a trie's deletefun is the
  ;; deleted data, which is always non-nil for a trie)
  (if (= (length --trie--do-delete--seq) 0)
      (funcall --trie--do-delete--deletefun
	       (trie--node-subtree node)
	       (trie--node-create-dummy trie--terminator)
	       (when --trie--do-delete--test
		 (lambda (n)
		   (funcall --trie--do-delete--test
			    --trie--do-delete--key (trie--node-data n)))))
    ;; otherwise, delete on down (return value of trie's deletion function is
    ;; the deleted data, which is always non-nil for a trie)
    (let (--trie-deleted--node)
      (funcall --trie--do-delete--deletefun
	       (trie--node-subtree node)
	       (trie--node-create-dummy (elt --trie--do-delete--seq 0))
	       (lambda (n)
		 (and (setq --trie-deleted--node
			    (trie--do-delete
			     n (trie--subseq --trie--do-delete--seq 1)
			     --trie--do-delete--test
			     --trie--do-delete--deletefun
			     --trie--do-delete--emptyfun
			     --trie--do-delete--cmpfun
			     --trie--do-delete--key))
		      (funcall --trie--do-delete--emptyfun
			       (trie--node-subtree n)))))
      --trie-deleted--node)))


(defun trie-delete (trie key &optional test)
  "Delete KEY and its associated data from TRIE.

If KEY was deleted, a cons cell containing KEY and its
association is returned. Returns nil if KEY does not exist in
TRIE.

If TEST is supplied, it should be a function that accepts two
arguments: the key being deleted, and its associated data. The
key will then only be deleted if TEST returns non-nil.

Note: to avoid nasty dynamic scoping bugs, TEST must *not* bind
any variables with names commencing \"--\"."
  ;; convert trie from print-form if necessary
  (trie-transform-from-read-warn trie)
  ;; set up deletion (real work is done by `trie--do-delete'
  (let ((deleted-node
	 (trie--do-delete (trie--root trie) key test
			  (trie--deletefun trie)
			  (trie--emptyfun trie)
			  (trie--cmpfun trie)
			  key)))
    (when deleted-node (cons key (trie--node-data deleted-node)))))



;; ----------------------------------------------------------------
;;                       Retrieving data

(defun trie-lookup (trie key &optional nilflag)
  "Return the data associated with KEY in the TRIE,
or nil if KEY does not exist in TRIE.

Optional argument NILFLAG specifies a value to return instead of
nil if KEY does not exist in TRIE. This allows a non-existent KEY
to be distinguished from an element with a null association. (See
also `trie-member-p', which does this for you.)"
  ;; convert trie from print-form if necessary
  (trie-transform-from-read-warn trie)
  ;; find node corresponding to key, then find data node, then return
  ;; data
  (let (node)
    (or (and (setq node (trie--node-find (trie--root trie) key
					 (trie--lookupfun trie)))
	     (trie--find-data node (trie--lookupfun trie)))
	nilflag)))

(defalias 'trie-member 'trie-lookup)


(defun trie-member-p (trie key)
  "Return t if KEY exists in TRIE, nil otherwise."
  ;; convert trie from print-form if necessary
  (trie-transform-from-read-warn trie)
  (let ((flag '(nil)))
    (not (eq flag (trie-member trie key flag)))))




;;; ================================================================
;;;                      Mapping over tries

(defun trie--mapc (--trie--mapc--function --trie--mapc--mapfun
		   --trie--mapc--root --trie--mapc--seq
		   &optional --trie--mapc--reverse)
  ;; Apply TRIE--MAPC--FUNCTION to all elements in a trie beneath
  ;; TRIE--MAPC--ROOT, which should correspond to the sequence
  ;; TRIE--MAPC--SEQ. TRIE--MAPC--FUNCTION is passed two arguments: the
  ;; trie node itself and the sequence it corresponds to. It is applied
  ;; in ascending order, or descending order if TRIE--MAPC--REVERSE is
  ;; non-nil.

  ;; The absurd argument names are to lessen the likelihood of dynamical
  ;; scoping bugs caused by a supplied function binding a variable with
  ;; the same name as one of the arguments.
  (funcall
   --trie--mapc--mapfun
   (lambda (--trie--mapc--node)
     ;; data node: apply function
     (if (trie--node-data-p --trie--mapc--node)
	 (funcall --trie--mapc--function
		  --trie--mapc--node
		  --trie--mapc--seq)
       ;; internal node: append split value to seq and keep descending
       (trie--mapc --trie--mapc--function
		   --trie--mapc--mapfun
		   --trie--mapc--node
		   (trie--seq-append
		    (copy-sequence --trie--mapc--seq)
		    (trie--node-split --trie--mapc--node))
		   --trie--mapc--reverse)))
   ;; --TRIE--MAPC--MAPFUN target
   (trie--node-subtree --trie--mapc--root)
   --trie--mapc--reverse))


(defun trie-mapc-internal (function trie &optional type)
  "Apply FUNCTION to all internal associative arrays within TRIE.
FUNCTION is passed two arguments: an associative array, and the
sequence it corresponds to.

Optional argument TYPE (one of the symbols vector, lisp or
string) sets the type of sequence passed to FUNCTION. Defaults to
vector."
  (trie--mapc-internal function (trie--mapfun trie) (trie--root trie)
		       (cond ((eq type 'string) "")
			     ((eq type 'lisp) ())
			     (t []))))


(defun trie--mapc-internal (--trie--mapc-internal--function
			     --trie--mapc-internal--mapfun
			     --trie--mapc-internal--root
			     --trie--mapc-internal--seq)
  (funcall
   --trie--mapc-internal--mapfun
   (lambda (--trie--mapc-internal--node)
     ;; data node
     (unless (trie--node-data-p --trie--mapc-internal--node)
       (funcall --trie--mapc-internal--function
		(trie--node-subtree --trie--mapc-internal--node)
		--trie--mapc-internal--seq)
       (trie--mapc-internal
	--trie--mapc-internal--function
	--trie--mapc-internal--mapfun
	--trie--mapc-internal--node
	(trie--seq-append
	 (copy-sequence --trie--mapc-internal--seq)
	 (trie--node-split --trie--mapc-internal--node)))))
   (trie--node-subtree --trie--mapc-internal--root)))


(defun trie-map (function trie &optional type reverse)
  "Modify all elements in TRIE by applying FUNCTION to them.

FUNCTION should take two arguments: a sequence stored in the trie
and its associated data. Its return value replaces the existing
data.

Optional argument TYPE (one of the symbols vector, lisp or
string) sets the type of sequence passed to FUNCTION. Defaults to
vector.

FUNCTION is applied in ascending order, or descending order if
REVERSE is non-nil.

Note: to avoid nasty dynamic scoping bugs, FUNCTION must *not*
bind any variables with names commencing \"--\"."
  ;; convert from print-form if necessary
  (trie-transform-from-read-warn trie)
  ;; map FUNCTION over TRIE
  (let ((--trie-map--function function)) ; avoid dynamic scoping bugs
    (trie--mapc
     (lambda (node seq)
       (setf (trie--node-data node)
	     (funcall --trie-map--function seq (trie--node-data node))))
     (trie--mapfun trie)
     (trie--root trie)
     (cond ((eq type 'string) "") ((eq type 'lisp) ()) (t []))
     reverse)))


(defun trie-mapc (function trie &optional type reverse)
  "Apply FUNCTION to all elements in TRIE for side effect only.

FUNCTION should take two arguments: a sequence stored in the trie
and its associated data.

Optional argument TYPE (one of the symbols vector, lisp or
string) sets the type of sequence passed to FUNCTION. Defaults to
vector.

FUNCTION is applied in ascending order, or descending order if
REVERSE is non-nil.

Note: to avoid nasty dynamic scoping bugs, FUNCTION must *not*
bind any variables with names commencing \"--\"."
  ;; convert from print-form if necessary
  (trie-transform-from-read-warn trie)
  ;; map FUNCTION over TRIE
  (let ((--trie-mapc--function function)) ; avoid dynamic scoping bugs
    (trie--mapc
     (lambda (node seq)
       (funcall --trie-mapc--function seq (trie--node-data node)))
     (trie--mapfun trie)
     (trie--root trie)
     (cond ((eq type 'string) "") ((eq type 'lisp) ()) (t []))
     reverse)))


(defun trie-mapf (function combinator trie &optional type reverse)
  "Apply FUNCTION to all elements in TRIE, and combine the results
using COMBINATOR.

FUNCTION should take two arguments: a sequence stored in the
trie, and its associated data.

Optional argument TYPE (one of the symbols vector, lisp or
string; defaults to vector) sets the type of sequence passed to
FUNCTION. If TYPE is `string', it must be possible to apply the
function `string' to the individual elements of key sequences
stored in TRIE.

The FUNCTION is applied and the results combined in ascending
order, or descending order if REVERSE is non-nil.

Note: to avoid nasty dynamic scoping bugs, FUNCTION and
COMBINATOR must *not* bind any variables with names
commencing \"--\"."
  ;; convert from print-form if necessary
  (trie-transform-from-read-warn trie)
  ;; map FUNCTION over TRIE, combining results with COMBINATOR
  (let ((--trie-mapf--function function) ; avoid dynamic scoping bugs
	--trie-mapf--accumulate)
    (trie--mapc
     (lambda (node seq)
       (setq --trie-mapf--accumulate
	     (funcall combinator
		      (funcall --trie-mapf--function
			       seq (trie--node-data node))
		      --trie-mapf--accumulate)))
     (trie--mapfun trie)
     (trie--root trie)
     (cond ((eq type 'string) "") ((eq type 'lisp) ()) (t []))
     reverse)
    --trie-mapf--accumulate))


(defun trie-mapcar (function trie &optional type reverse)
  "Apply FUNCTION to all elements in TRIE,
and make a list of the results.

FUNCTION should take two arguments: a sequence stored in the trie
and its associated data.

Optional argument TYPE (one of the symbols vector, lisp or
string) sets the type of sequence passed to FUNCTION. Defaults to
vector.

The FUNCTION is applied and the list constructed in ascending
order, or descending order if REVERSE is non-nil.

Note that if you don't care about the order in which FUNCTION is
applied, just that the resulting list is in the correct order,
then

  (trie-mapf function \\='cons trie type (not reverse))

is more efficient."
  ;; convert from print-form if necessary
  (trie-transform-from-read-warn trie)
  ;; map FUNCTION over TRIE and accumulate in a list
  (nreverse (trie-mapf function #'cons trie type reverse)))




;;; ================================================================
;;;                    Using tries as stacks

(defstruct (trie--stack
	    (:constructor nil)
	    (:constructor
	     trie--stack-create
	     (trie
	      &optional
	      (type 'vector)
	      reverse
	      &aux
	      (comparison-function (trie--comparison-function trie))
	      (lookupfun (trie--lookupfun trie))
	      (stackcreatefun (trie--stack-createfun trie))
	      (stackpopfun (trie--stack-popfun trie))
	      (stackemptyfun (trie--stack-emptyfun trie))
	      (repopulatefun #'trie--stack-repopulate)
	      (store
	       (if (trie-empty trie)
		   nil
		 (trie--stack-repopulate
		  (list (cons
			 (cond ((eq type 'list) ())
			       ((eq type 'string) "")
			       (t []))
			 (funcall
			  stackcreatefun
			  (trie--node-subtree (trie--root trie))
			  reverse)))
		  reverse
		  comparison-function lookupfun
		  stackcreatefun stackpopfun stackemptyfun)))
	      (pushed '())
	      ))
	    (:constructor
	     trie--complete-stack-create
	     (trie prefix
	      &optional
	      reverse
	      &aux
	      (comparison-function (trie--comparison-function trie))
	      (lookupfun (trie--lookupfun trie))
	      (stackcreatefun (trie--stack-createfun trie))
	      (stackpopfun (trie--stack-popfun trie))
	      (stackemptyfun (trie--stack-emptyfun trie))
	      (repopulatefun #'trie--stack-repopulate)
	      (store (trie--complete-stack-construct-store
		      trie prefix reverse))
	      (pushed '())
	      ))
	    (:constructor
	     trie--regexp-stack-create
	     (trie regexp
	      &optional
	      reverse
	      &aux
	      (comparison-function (trie--comparison-function trie))
	      (lookupfun (trie--lookupfun trie))
	      (stackcreatefun (trie--stack-createfun trie))
	      (stackpopfun (trie--stack-popfun trie))
	      (stackemptyfun (trie--stack-emptyfun trie))
	      (repopulatefun #'trie--regexp-stack-repopulate)
	      (store (trie--regexp-stack-construct-store
		      trie regexp reverse))
	      (pushed '())
	      ))
	    (:constructor
	     trie--fuzzy-match-stack-create
	     (trie string distance
	      &optional
	      reverse
	      &aux
	      (comparison-function (trie--comparison-function trie))
	      (lookupfun (trie--lookupfun trie))
	      (stackcreatefun (trie--stack-createfun trie))
	      (stackpopfun (trie--stack-popfun trie))
	      (stackemptyfun (trie--stack-emptyfun trie))
	      (repopulatefun #'trie--fuzzy-match-stack-repopulate)
	      (store (trie--fuzzy-match-stack-construct-store
		      trie string distance reverse))
	      (pushed '())
	      ))
	    (:constructor
	     trie--fuzzy-complete-stack-create
	     (trie prefix distance
	      &optional
	      reverse
	      &aux
	      (comparison-function (trie--comparison-function trie))
	      (lookupfun (trie--lookupfun trie))
	      (stackcreatefun (trie--stack-createfun trie))
	      (stackpopfun (trie--stack-popfun trie))
	      (stackemptyfun (trie--stack-emptyfun trie))
	      (repopulatefun #'trie--fuzzy-complete-stack-repopulate)
	      (store (trie--fuzzy-complete-stack-construct-store
		      trie prefix distance reverse))
	      (pushed '())
	      ))
	    (:copier nil))
  reverse comparison-function lookupfun
  stackcreatefun stackpopfun stackemptyfun
  repopulatefun store pushed)


(defun trie-stack (trie &optional type reverse)
  "Return an object that allows TRIE to be accessed as a stack.

The stack is sorted in \"lexicographic\" order, i.e. the order
defined by the trie's comparison function, or in reverse order if
REVERSE is non-nil. Calling `trie-stack-pop' pops the top element
\(a cons cell containing a key and its associated data\) from the
stack.

Optional argument TYPE \(one of the symbols `vector', `lisp' or
`string'\) sets the type of sequence used for the keys,
defaulting to `vector'. \(If TYPE is string, it must be possible
to apply `string' to individual elements of TRIE keys.\)

Note that any modification to TRIE *immediately* invalidates all
trie-stacks created before the modification \(in particular,
calling `trie-stack-pop' will give unpredictable results\).

Operations on trie-stacks are significantly more efficient than
constructing a real stack containing all the contents of the trie
and using standard stack functions. As such, they can be useful
in implementing efficient algorithms over tries. However, in
cases where mapping functions `trie-mapc', `trie-mapcar' or
`trie-mapf' would be sufficient, it may be better to use one of
those instead."
  ;; convert trie from print-form if necessary
  (trie-transform-from-read-warn trie)
  ;; if stack functions aren't defined for trie type, throw error
  (if (not (functionp (trie--stack-createfun trie)))
      (error "Trie type does not support stack operations")
    ;; otherwise, create and initialise a stack
    (trie--stack-create trie type reverse)))


(defun trie-stack-pop (trie-stack &optional nilflag)
  "Pop the first element from TRIE-STACK.

Returns nil if the stack is empty, or NILFLAG if specified. (The
latter allows an empty stack to be distinguished from a null
element stored in the trie.)"
  ;; return nilflag if stack is empty
  (if (trie-stack-empty-p trie-stack)
      nilflag
    ;; if elements have been pushed onto the stack, pop those first
    (if (trie--stack-pushed trie-stack)
	(pop (trie--stack-pushed trie-stack))
      ;; otherwise, pop first element from trie-stack and repopulate it
      (prog1
	  (pop (trie--stack-store trie-stack))
	(setf (trie--stack-store trie-stack)
	      (funcall (trie--stack-repopulatefun trie-stack)
		       (trie--stack-store trie-stack)
		       (trie--stack-reverse trie-stack)
		       (trie--stack-comparison-function trie-stack)
		       (trie--stack-lookupfun trie-stack)
		       (trie--stack-stackcreatefun trie-stack)
		       (trie--stack-stackpopfun trie-stack)
		       (trie--stack-stackemptyfun trie-stack)))))))


(defun trie-stack-push (element trie-stack)
  "Push ELEMENT onto TRIE-STACK."
  (push element (trie--stack-pushed trie-stack)))


(defun trie-stack-first (trie-stack &optional nilflag)
  "Return the first element from TRIE-STACK, without removing it
from the stack.

Returns nil if the stack is empty, or NILFLAG if specified. (The
latter allows an empty stack to be distinguished from a null
element stored in the trie.)"
  ;; return nilflag if stack is empty
  (if (trie-stack-empty-p trie-stack)
      nilflag
    ;; if elements have been pushed onto the stack, return first of
    ;; those
    (if (trie--stack-pushed trie-stack)
	(car (trie--stack-pushed trie-stack))
      ;; otherwise, return first element from trie-stack
      (car (trie--stack-store trie-stack)))))


(defalias 'trie-stack-p 'trie--stack-p
  "Return t if argument is a trie-stack, nil otherwise.")


(defun trie-stack-empty-p (trie-stack)
  "Return t if TRIE-STACK is empty, nil otherwise."
  (and (null (trie--stack-store trie-stack))
       (null (trie--stack-pushed trie-stack))))


(defun trie--stack-repopulate
  (store reverse _comparison-function _lookupfun
	 stack-createfun stack-popfun stack-emptyfun)
  ;; Recursively push children of the node at the head of STORE onto the
  ;; front of STORE, until a data node is reached.

  ;; nothing to do if stack is empty
  (when store
    (let ((node (funcall stack-popfun (cdar store)))
	  (seq (caar store)))
      (when (funcall stack-emptyfun (cdar store))
	;; using (pop store) here produces irritating compiler warnings
	(setq store (cdr store)))

      (while (not (trie--node-data-p node))
	(push
	 (cons (trie--seq-append seq (trie--node-split node))
	       (funcall stack-createfun (trie--node-subtree node) reverse))
	 store)
	(setq node (funcall stack-popfun (cdar store))
	      seq (caar store))
	(when (funcall stack-emptyfun (cdar store))
	  (setq store (cdr store))))

      (push (cons seq (trie--node-data node)) store))))



;; trie-stacks *are* iterators (with additional push and inspect-first-element
;; operations). If we're running on a modern Emacs that includes the
;; `generator' library, we can trivially define trie iterator generators in
;; terms of trie-stacks.

(heap--when-generators
 (iter-defun trie-iter (trie &optional type reverse)
   "Return a trie iterator object.

Calling `iter-next' on this object will retrieve the next element
\(a cons cell containing a key and its associated data\) from
TRIE, in \"lexicographic\" order, i.e. the order defined by the
trie's comparison function, or in reverse order if REVERSE is
non-nil.

Optional argument TYPE \(one of the symbols `vector', `list' or
`string'\) sets the type of sequence used for the keys,
defaulting to `vector'. \(If TYPE is string, it must be possible
to apply `string' to individual elements of TRIE keys.\)

Note that any modification to TRIE *immediately* invalidates all
iterators created from TRIE before the modification \(in
particular, calling `iter-next' will give unpredictable
results\)."
   (let ((stack (trie-stack trie type reverse)))
     (while (not (trie-stack-empty-p stack))
       (iter-yield (trie-stack-pop stack))))))






;; ================================================================
;;                   Query-building utility macros

;; Implementation Note
;; -------------------
;; For queries ranked in anything other than lexicographic order, we use a
;; partial heap-sort to find the k=MAXNUM highest ranked matches among the n
;; possibile matches. This has worst-case time complexity O(n log k), and is
;; both simple and elegant. An optimal algorithm (e.g. partial quick-sort
;; discarding the irrelevant partition at each step) would have complexity
;; O(n + k log k), but is probably not worth the extra coding effort. It would
;; also have worse space complexity unless coded to work "in-place", which
;; would be highly non-trivial. (I haven't done any benchmarking, though, so
;; feel free to do so and let me know the results!)

(defmacro trie--construct-accumulator (maxnum filter resultfun)
  ;; Does what it says on the tin! | sed -e 's/tin/macro name/'
  (declare (debug t))
  `(cond
    ;; filter, maxnum, resultfun
    ((and ,filter ,maxnum ,resultfun)
     (lambda (seq data)
       (when (funcall ,filter seq data)
	 (aset trie--accumulate 0
	       (cons (funcall ,resultfun seq data)
		     (aref trie--accumulate 0)))
	 (and (>= (length (aref trie--accumulate 0)) ,maxnum)
	      (throw 'trie--accumulate-done nil)))))
    ;; filter, maxnum, !resultfun
    ((and ,filter ,maxnum (not ,resultfun))
     (lambda (seq data)
       (when (funcall ,filter seq data)
	 (aset trie--accumulate 0
	       (cons (cons seq data)
		     (aref trie--accumulate 0)))
	 (and (>= (length (aref trie--accumulate 0)) ,maxnum)
	      (throw 'trie--accumulate-done nil)))))
    ;; filter, !maxnum, resultfun
    ((and ,filter (not ,maxnum) ,resultfun)
     (lambda (seq data)
       (when (funcall ,filter seq data)
	 (aset trie--accumulate 0
	       (cons (funcall ,resultfun seq data)
		     (aref trie--accumulate 0))))))
    ;; filter, !maxnum, !resultfun
    ((and ,filter (not ,maxnum) (not ,resultfun))
     (lambda (seq data)
       (when (funcall ,filter seq data)
	 (aset trie--accumulate 0
	       (cons (cons seq data)
		     (aref trie--accumulate 0))))))
    ;; !filter, maxnum, resultfun
    ((and (not ,filter) ,maxnum ,resultfun)
     (lambda (seq data)
       (aset trie--accumulate 0
	     (cons (funcall ,resultfun seq data)
		   (aref trie--accumulate 0)))
       (and (>= (length (aref trie--accumulate 0)) ,maxnum)
	    (throw 'trie--accumulate-done nil))))
    ;; !filter, maxnum, !resultfun
    ((and (not ,filter) ,maxnum (not ,resultfun))
     (lambda (seq data)
       (aset trie--accumulate 0
	     (cons (cons seq data)
		   (aref trie--accumulate 0)))
       (and (>= (length (aref trie--accumulate 0)) ,maxnum)
	    (throw 'trie--accumulate-done nil))))
    ;; !filter, !maxnum, resultfun
    ((and (not ,filter) (not ,maxnum) ,resultfun)
     (lambda (seq data)
       (aset trie--accumulate 0
	     (cons (funcall ,resultfun seq data)
		   (aref trie--accumulate 0)))))
    ;; !filter, !maxnum, !resultfun
    ((and (not ,filter) (not ,maxnum) (not ,resultfun))
     (lambda (seq data)
       (aset trie--accumulate 0
	     (cons (cons seq data)
		   (aref trie--accumulate 0)))))
    ))



(defmacro trie--construct-ranked-accumulator (maxnum filter)
  ;; Does what it says on the tin! | sed -e 's/tin/macro name/'
  (declare (debug t))
  `(cond
    ;; filter, maxnum
    ((and ,filter ,maxnum)
     (lambda (seq data)
       (when (funcall ,filter seq data)
	 (heap-add trie--accumulate (cons seq data))
	 (and (> (heap-size trie--accumulate) ,maxnum)
	      (heap-delete-root trie--accumulate)))))
    ;; filter, !maxnum
    ((and ,filter (not ,maxnum))
     (lambda (seq data)
       (when (funcall ,filter seq data)
	 (heap-add trie--accumulate (cons seq data)))))
    ;; !filter, maxnum
    ((and (not ,filter) ,maxnum)
     (lambda (seq data)
       (heap-add trie--accumulate (cons seq data))
       (and (> (heap-size trie--accumulate) ,maxnum)
	    (heap-delete-root trie--accumulate))))
    ;; !filter, !maxnum
    ((and (not ,filter) (not ,maxnum))
     (lambda (seq data)
       (heap-add trie--accumulate (cons seq data))))))



(defmacro trie--accumulate-results
  (rankfun maxnum reverse filter resultfun accfun duplicates &rest body)
  (declare (debug t))
  ;; Accumulate results of running BODY code, and return them in appropriate
  ;; order. BODY should call ACCFUN to accumulate a result, passing it two
  ;; arguments: a trie key and its associated data. BODY can throw
  ;; trie--accumulate-done to terminate the accumulation and return the
  ;; results. A non-null DUPLICATES flag signals that the accumulated results
  ;; might contain duplicates, which should be deleted. Note that DUPLICATES
  ;; is ignored if RANKFUN is null, and that duplicates *do* count towards
  ;; MAXNUM. The remaining arguments have the usual meanings, and should be
  ;; passed straight through from the query function's arguments.

  ;; rename functions to help avoid dynamic-scoping bugs
  ;; FIXME: not needed with lexical scoping
  `(let* ((--trie-accumulate--rankfun ,rankfun)
	  (--trie-accumulate--filter ,filter)
	  (--trie-accumulate--resultfun ,resultfun)
	  ;; construct structure in which to accumulate results
	  (trie--accumulate
	   (if ,rankfun
	       (heap-create  ; heap order is inverse of rank order
		(if ,reverse
		    (lambda (a b)
		      (funcall --trie-accumulate--rankfun a b))
		  (lambda (a b)
		    (not (funcall --trie-accumulate--rankfun a b))))
		(when ,maxnum (1+ ,maxnum)))
	     (make-vector 1 nil)))
	  ;; construct function to accumulate results
	  (,accfun
	   (if ,rankfun
	       (trie--construct-ranked-accumulator
		,maxnum --trie-accumulate--filter)
	     (trie--construct-accumulator
	      ,maxnum --trie-accumulate--filter
	      --trie-accumulate--resultfun))))

     ;; accumulate results
     (catch 'trie--accumulate-done ,@body)

     ;; return list of results
     (cond
      ;; for a ranked query, extract results from heap
      (,rankfun
       (let (results)
	 ;; check for and delete duplicates if flag is set
	 (if ,duplicates
	     (while (not (heap-empty trie--accumulate))
	       (if (equal (car (heap-root trie--accumulate))
			  (caar results))
		   (heap-delete-root trie--accumulate)
		 (push (heap-delete-root trie--accumulate)
		       results)))
	   ;; skip duplicate checking if flag is not set
	   (while (not (heap-empty trie--accumulate))
	     (if ,resultfun
		 (let ((res (heap-delete-root trie--accumulate)))
		   (push (funcall ,resultfun (car res) (cdr res))
			 results))
	       (push (heap-delete-root trie--accumulate)
		     results))))
	 results))

      ;; for lexicographic query, reverse result list if MAXNUM supplied
      (,maxnum (nreverse (aref trie--accumulate 0)))
      ;; otherwise, just return list
      (t (aref trie--accumulate 0)))))




;; ================================================================
;;                          Completing

(defun trie-complete
  (trie prefix &optional rankfun maxnum reverse filter resultfun)
  "Return an alist containing all completions of PREFIX in TRIE
along with their associated data, in the order defined by
RANKFUN, defaulting to \"lexicographic\" order \(i.e. the order
defined by the trie's comparison function\). If REVERSE is
non-nil, the completions are sorted in the reverse order. Returns
nil if no completions are found.

PREFIX must be a sequence (vector, list or string) containing
elements of the type used to reference data in the trie. (If
PREFIX is a string, it must be possible to apply `string' to
individual elements of the sequences stored in the trie.) The
completions returned in the alist will be sequences of the same
type as KEY. If PREFIX is a list of sequences, completions of all
sequences in the list are included in the returned alist. All
sequences in the list must be of the same type.

The optional integer argument MAXNUM limits the results to the
first MAXNUM completions. Otherwise, all completions are
returned.

If specified, RANKFUN must accept two arguments, both cons
cells. The car contains a sequence from the trie (of the same
type as PREFIX), the cdr contains its associated data. It should
return non-nil if first argument is ranked strictly higher than
the second, nil otherwise.

The FILTER argument sets a filter function for the
completions. If supplied, it is called for each possible
completion with two arguments: the completion, and its associated
data. If the filter function returns nil, the completion is not
included in the results, and does not count towards MAXNUM.

RESULTFUN defines a function used to process results before
adding them to the final result list. If specified, it should
accept two arguments: a key and its associated data. Its return
value is what gets added to the final result list, instead of the
default key-data cons cell."

  ;; convert trie from print-form if necessary
  (trie-transform-from-read-warn trie)
  ;; wrap prefix in a list if necessary
  ;; FIXME: the test for a list of prefixes, below, will fail if the
  ;;        PREFIX sequence is a list, and the elements of PREFIX are
  ;;        themselves lists (there might be no easy way to fully fix
  ;;        this...)
  (if (or (atom prefix)
	  (and (listp prefix) (not (sequencep (car prefix)))))
      (setq prefix (list prefix))
    ;; sort list of prefixes if sorting completions lexicographicly
    (when (null rankfun)
      (setq prefix
	    (sort prefix (trie-construct-sortfun
			  (trie--comparison-function trie))))))

  ;; accumulate completions
  (let (node)
    (trie--accumulate-results
     rankfun maxnum reverse filter resultfun accumulator nil
     (mapc (lambda (pfx)
	     (setq node (trie--node-find (trie--root trie) pfx
					 (trie--lookupfun trie)))
	     (when node
	       (trie--mapc
		(lambda (node seq)
		  (funcall accumulator seq (trie--node-data node)))
		(trie--mapfun trie) node pfx
		(if maxnum reverse (not reverse)))))
	   prefix))
    ))



(defun trie-complete-stack (trie prefix &optional reverse)
  "Return an object that allows completions of PREFIX to be accessed
as if they were a stack.

The stack is sorted in \"lexicographic\" order, i.e. the order
defined by TRIE's comparison function, or in reverse order if
REVERSE is non-nil. Calling `trie-stack-pop' pops the top element
\(a cons cell containing the next completion and its associated
data\) from the stack.

PREFIX must be a sequence (vector, list or string) that forms the
initial part of a TRIE key, or a list of such sequences. \(If
PREFIX is a string, it must be possible to apply `string' to
individual elements of TRIE keys.\) The completions returned by
`trie-stack-pop' will be sequences of the same type as KEY. If
PREFIX is a list of sequences, they must all be of the same
type. In this case, completions of all sequences in the list are
included in the stack.

Note that any modification to TRIE *immediately* invalidates all
trie-stacks created before the modification \(in particular,
calling `trie-stack-pop' will give unpredictable results\).

Operations on trie-stacks are significantly more efficient than
constructing a real stack from completions of PREFIX in TRIE and
using standard stack functions. As such, they can be useful in
implementing efficient algorithms over tries. However, in cases
where `trie-complete' is sufficient, it is better to use that
instead."
  ;; convert trie from print-form if necessary
  (trie-transform-from-read-warn trie)
  ;; if stack functions aren't defined for trie type, throw error
  (if (not (functionp (trie--stack-createfun trie)))
      (error "Trie type does not support stack operations")
    ;; otherwise, create and initialise a stack
    (trie--complete-stack-create trie prefix reverse)))


(defun trie--complete-stack-construct-store (trie prefix reverse)
  ;; Construct store for completion stack based on TRIE.
  (let (store node)
    (if (or (atom prefix)
	    (and (listp prefix)
		 (not (sequencep (car prefix)))))
	(setq prefix (list prefix))
      (setq prefix
	    (sort prefix
		  (trie-construct-sortfun
		   (trie--comparison-function trie)
		   (not reverse)))))
    (dolist (pfx prefix)
      (when (setq node (trie--node-find (trie--root trie) pfx
					(trie--lookupfun trie)))
	(push (cons pfx (funcall (trie--stack-createfun trie)
				 (trie--node-subtree node)
				 reverse))
	      store)))
    (trie--stack-repopulate
     store reverse
     (trie--comparison-function trie)
     (trie--lookupfun trie)
     (trie--stack-createfun trie)
     (trie--stack-popfun trie)
     (trie--stack-emptyfun trie))))


(heap--when-generators
 (iter-defun trie-complete-iter (trie prefix &optional reverse)
   "Return an iterator object for completions of PREFIX in TRIE.

Calling `iter-next' on this object will retrieve the next
completion \(a cons cell containing a completion and its
associated data\) of PREFIX in the TRIE, in \"lexicographic\"
order, i.e. the order defined by the trie's comparison function,
or in reverse order if REVERSE is non-nil.

PREFIX must be a sequence (vector, list or string) that forms the
initial part of a TRIE key, or a list of such sequences. \(If
PREFIX is a string, it must be possible to apply `string' to
individual elements of TRIE keys.\) The completions returned by
`iter-next' will be sequences of the same type as KEY. If PREFIX
is a list of sequences, they must all be of the same type. In
this case, the iterator yields completions of all sequences in
the list.

Note that any modification to TRIE *immediately* invalidates all
iterators created from TRIE before the modification \(in
particular, calling `iter-next' will give unpredictable
results\)."
   (let ((stack (trie-complete-stack trie prefix reverse)))
     (while (not (trie-stack-empty-p stack))
       (iter-yield (trie-stack-pop stack))))))




;; ================================================================
;;                        Regexp search

(defun trie-regexp-search
  (trie regexp &optional rankfun maxnum reverse filter resultfun)
  "Return an alist containing all matches for REGEXP in TRIE
along with their associated data, in the order defined by
RANKFUN, defaulting to \"lexicographic\" order \(i.e. the order
defined by the trie's comparison function\). If REVERSE is
non-nil, the results are sorted in the reverse order. Returns nil
if no results are found.

REGEXP is a regular expression, but it need not necessarily be a
string. It must be a sequence (vector, list, or string) whose
elements are either elements of the same type as elements of the
trie keys (which behave as literals in the regexp), or a regexp
special character or backslash construct. If REGEXP is a string,
it must be possible to apply `string' to individual elements of
the keys stored in the trie. The matches returned in the alist
will be sequences of the same type as REGEXP.

Only a subset of the full Emacs regular expression syntax is
supported. There is no support for regexp constructs that are
only meaningful for strings (character ranges and character
classes inside character alternatives, and syntax-related
backslash constructs). Back-references and non-greedy postfix
operators are not supported, so `?' after a postfix operator
loses its special meaning. Also, matches are always anchored, so
`$' and `^' lose their special meanings (use `.*' at the
beginning and end of the regexp to get an unanchored match).

If the regexp contains any non-shy grouping constructs, subgroup
match data is included in the results. In this case, the car of
each match is no longer just a key. Instead, each element of the
results list has the form

    ((KEY (START1 . END1) (START2 . END2) ...) . DATA)

where the (START . END) cons cells give the start and end indices
of the elements that matched the corresponding groups, in order.


The optional integer argument MAXNUM limits the results to the
first MAXNUM matches. Otherwise, all matches are returned.


If specified, RANKFUN must accept two arguments. If the regexp
does not contain any non-shy grouping constructs, both arguments
are (KEY . DATA) cons cells, where the car is a sequence of the
same type as REGEXP. If the regexp does contain non-shy grouping
constructs, both arguments are of the form

    ((KEY (START1 . END1) (START2 . END2) ...) . DATA)

RANKFUN should return non-nil if first argument is ranked
strictly higher than the second, nil otherwise.


The FILTER argument sets a filter function for the matches. If
supplied, it is called for each possible match with two
arguments: a key and its associated data. If the regexp contains
non-shy grouping constructs, the first argument is of the form

    (KEY (START1 . END1) (START2 . END2) ...)

If the FILTER function returns nil, the match is not included in
the results, and does not count towards MAXNUM.


RESULTFUN defines a function used to process results before
adding them to the final result list. If specified, it should
accept two arguments, of the same form as those for FILTER (see
above). Its return value is what gets added to the final result
list, instead of the default key-data cons cell."

  ;; convert trie from print-form if necessary
  (trie-transform-from-read-warn trie)

  ;; accumulate results
  (trie--accumulate-results
   rankfun maxnum reverse filter resultfun accumulator nil
   (trie--do-regexp-search
    (trie--root trie)
    (tNFA-from-regexp regexp :test (trie--construct-equality-function
				    (trie--comparison-function trie)))
    (cond ((stringp regexp) "") ((listp regexp) ()) (t []))  0
    (or (and maxnum reverse) (and (not maxnum) (not reverse)))
    ;; FIXME: Is this a case where it would pay to replace these arguments
    ;;        with dynamically-scoped variables, to save stack space during
    ;;        the recursive calls to `trie--do-regexp-search'?  Alternatively,
    ;;        with lexical scoping, we could use a closure for
    ;;        `trie--do-regexp-search' instead of a function.
    (trie--comparison-function trie)
    (trie--lookupfun trie)
    (trie--mapfun trie)
    accumulator)))



(defun trie--do-regexp-search
  (--trie--regexp-search--node tNFA seq pos reverse
			       cmpfun lookupfun mapfun accumulator)
  ;; Search everything below the node --TRIE--REGEXP-SEARCH-NODE for
  ;; matches to the regexp encoded in tNFA. SEQ is the sequence
  ;; corresponding to NODE, POS is it's length. REVERSE is the usual
  ;; query argument, and the remaining arguments are the corresponding
  ;; trie functions.

  ;; if NFA has matched and we're accumulating in normal order, check if
  ;; trie contains current string
  (when (and (not reverse) (tNFA-match-p tNFA))
    (let (node groups)
      (when (setq node (trie--find-data-node
			--trie--regexp-search--node lookupfun))
	(setq groups (tNFA-group-data tNFA))
	(funcall accumulator
		 (if groups (cons seq groups) seq)
		 (trie--node-data node)))))

  (cond
   ;; ;; data node
   ;; ((trie--node-data-p --trie--regexp-search--node)
   ;;  (when (tNFA-match-p tNFA)
   ;;    (let ((groups (tNFA-group-data tNFA)))
   ;; 	(funcall accumulator
   ;; 		 (if groups (cons seq groups) seq)
   ;; 		 (trie--node-data --trie--regexp-search--node)))))

   ;; wildcard transition: map over all nodes in subtree
   ((tNFA-wildcard-p tNFA)
    (let (state)
      (funcall mapfun
	       (lambda (node)
		 (unless (trie--node-data-p node)
		     ;; (when (tNFA-match-p tNFA)
		     ;;   (setq groups (tNFA-group-data tNFA))
		     ;;   (funcall accumulator
		     ;; 		(if groups (cons seq groups) seq)
		     ;; 		(trie--node-data node)))
		   (when (setq state (tNFA-next-state
				      tNFA (trie--node-split node) pos))
		     (trie--do-regexp-search
		      node state
		      (trie--seq-append seq (trie--node-split node))
		      (1+ pos)
		      reverse cmpfun lookupfun mapfun accumulator))))
	       (trie--node-subtree --trie--regexp-search--node)
	       reverse)))

   (t ;; no wildcard transition: loop over all transitions
    ;; rename function to mitigate against dynamic scoping bugs
    ;; FIXME: not needed with lexical scoping
    (let ((--trie--do-regexp-search--cmpfun cmpfun)
	  node state)
      (dolist (chr (sort (tNFA-transitions tNFA)
			 (if reverse
			     (lambda (a b)
			       (funcall
				--trie--do-regexp-search--cmpfun
				b a))
			   cmpfun)))
	(when (and (setq node (trie--node-find
			       --trie--regexp-search--node
			       (vector chr) lookupfun))
		   (setq state (tNFA-next-state tNFA chr pos)))
	  (trie--do-regexp-search
	   node state (trie--seq-append seq chr) (1+ pos)
	   reverse cmpfun lookupfun mapfun accumulator))))))

  ;; if NFA has matched and we're accumulating in reverse order, check if
  ;; trie contains current string
  (when (and reverse (tNFA-match-p tNFA))
    (let (node groups)
      (when (setq node (trie--find-data-node
			--trie--regexp-search--node lookupfun))
	(setq groups (tNFA-group-data tNFA))
	(funcall accumulator
		 (if groups (cons seq groups) seq)
		 (trie--node-data node))))))



(defun trie-regexp-stack (trie regexp &optional reverse)
  "Return an object that allows matches to REGEXP to be accessed
as if they were a stack.

The stack is sorted in \"lexicographic\" order, i.e. the order
defined by TRIE's comparison function, or in reverse order if
REVERSE is non-nil. Calling `trie-stack-pop' pops the top element
\(a cons cell containing a key and its associated data\) from the
stack.

REGEXP is a regular expression, but it need not necessarily be a
string. It must be a sequence \(vector, list or string\) whose
elements either have the same type as elements of the trie keys
\(which behave as literals in the regexp\), or are any of the
usual regexp special characters \(character type\) or backslash
constructs \(string type\).

If REGEXP is a string, it must be possible to apply `string' to
individual elements of the keys stored in the trie. The matches
returned by `trie-stack-pop' will be sequences of the same type
as KEY.

Back-references and non-greedy postfix operators are *not*
supported, and the matches are always anchored, so `$' and `^'
lose their special meanings.

If the regexp contains any non-shy grouping constructs, subgroup
match data is included in the results. In this case, the car of
each match \(as returned by a call to `trie-stack-pop'\) is no
longer just a key. Instead, it is a list whose first element is
the matching key, and whose remaining elements are cons cells
whose cars and cdrs give the start and end indices of the
elements that matched the corresponding groups, in order."

  ;; convert trie from print-form if necessary
  (trie-transform-from-read-warn trie)
  ;; if stack functions aren't defined for trie type, throw error
  (if (not (functionp (trie--stack-createfun trie)))
      (error "Trie type does not support stack operations")
    ;; otherwise, create and initialise a regexp stack
    (trie--regexp-stack-create trie regexp reverse)))


(defun trie--regexp-stack-construct-store
  (trie regexp &optional reverse)
  ;; Construct store for regexp stack based on TRIE.
  (let ((seq (cond ((stringp regexp) "") ((listp regexp) ()) (t [])))
	store)
    (push (list seq (trie--root trie)
		(tNFA-from-regexp
		 regexp :test (trie--construct-equality-function
			       (trie--comparison-function trie)))
		0)
	  store)
    (trie--regexp-stack-repopulate
     store reverse
     (trie--comparison-function trie)
     (trie--lookupfun trie)
     (trie--stack-createfun trie)
     (trie--stack-popfun trie)
     (trie--stack-emptyfun trie))))


(defun trie--regexp-stack-repopulate
  (store reverse comparison-function lookupfun
	 stack-createfun stack-popfun stack-emptyfun)
  ;; Recursively push matching children of the node at the head of STORE
  ;; onto STORE, until a data node is reached. REVERSE is the usual
  ;; query argument, and the remaining arguments are the corresponding
  ;; trie functions.
  (let (state seq node pos groups n s)
    (while
	(progn
	  (setq pos (pop store)
		seq (nth 0 pos)
		node (nth 1 pos)
		state (nth 2 pos)
		pos (nth 3 pos))
	  (cond
	   ;; if stack is empty, we're done
	   ((null node) nil)

	   ;; if stack element is a trie node...
	   ((trie--node-p node)
	    (cond
	     ;; matching data node: add data to the stack and we're done
	     ((trie--node-data-p node)
	      (when (tNFA-match-p state)
		(setq groups (tNFA-group-data state))
		(push (cons (if groups (cons groups seq) seq)
			    (trie--node-data node))
		      store))
	      nil)  ; return nil to exit loop

	     ;; wildcard transition: add new node stack
	     ((tNFA-wildcard-p state)
	      (push (list seq
			  (funcall stack-createfun
				   (trie--node-subtree node) reverse)
			  state pos)
		    store))

	     (t ;; non-wildcard transition: add all possible next nodes
	      ;; rename function to mitigate against lexical scoping bugs
	      ;; FIXME: not needed with lexical scoping
	      (let ((--trie--regexp-stack-repopulate--cmpfun
		     comparison-function))
		(dolist (chr (sort (tNFA-transitions state)
				   (if reverse
				       --trie--regexp-stack-repopulate--cmpfun
				   (lambda (a b)
				      (funcall
				       --trie--regexp-stack-repopulate--cmpfun
				       b a)))))
		  (when (and (setq n (trie--node-find
				      node (vector chr) lookupfun))
			     (setq s (tNFA-next-state state chr pos)))
		    (push (list (trie--seq-append seq chr) n s (1+ pos))
			  store))))
	      t)))  ; return t to keep looping

	   ;; otherwise, stack element is a node stack...
	   (t
	    ;; if node stack is empty, dump it and keep repopulating
	    (if (funcall stack-emptyfun node)
		t  ; return t to keep looping
	      ;; otherwise, add node stack back, and add next node from
	      ;; stack
	      (push (list seq node state pos) store)
	      (setq node (funcall stack-popfun node)
		    state (tNFA-next-state state
					   (trie--node-split node) pos))
	      (when state
		;; matching data node: add data to the stack and we're
		;; done
		(if (trie--node-data-p node)
		    (progn
		      (push (cons seq (trie--node-data node)) store)
		      nil)  ; return nil to exit loop
		  ;; normal node: add it to the stack and keep
		  ;; repopulating
		  (push (list
			 (trie--seq-append seq (trie--node-split node))
			 node state (1+ pos))
			store)))))
	   ))))
  store)


(heap--when-generators
 (iter-defun trie-regexp-iter (trie regexp &optional reverse)
   "Return an iterator object for REGEXP matches in TRIE.

Calling `iter-next' on this object will retrieve the next match
\(a cons cell containing a key and its associated data\) for
REGEXP in the TRIE, in \"lexicographic\" order, i.e. the order
defined by the trie's comparison function, or in reverse order if
REVERSE is non-nil.

REGEXP is a regular expression, but it need not necessarily be a
string. It must be a sequence \(vector, list or string\) whose
elements either have the same type as elements of the trie keys
\(which behave as literals in the regexp\), or are any of the
usual regexp special characters \(character type\) or backslash
constructs \(string type\).

If REGEXP is a string, it must be possible to apply `string' to
individual elements of the keys stored in the trie. The matches
returned by `iter-next' will be sequences of the same type as
KEY.

Back-references and non-greedy postfix operators are *not*
supported, and the matches are always anchored, so `$' and `^'
lose their special meanings.

If the regexp contains any non-shy grouping constructs, subgroup
match data is included in the results. In this case, the car of
each match \(as returned by a call to `iter-next'\) is no longer
just a key. Instead, it is a list whose first element is the
matching key, and whose remaining elements are cons cells whose
cars and cdrs give the start and end indices of the elements that
matched the corresponding groups, in order.

Note that any modification to TRIE *immediately* invalidates all
iterators created from TRIE before the modification \(in
particular, calling `iter-next' will give unpredictable
results\)."
   (let ((stack (trie-regexp-stack trie regexp reverse)))
     (while (not (trie-stack-empty-p stack))
       (iter-yield (trie-stack-pop stack))))))




;; ================================================================
;;                        Fuzzy matching


;; Basic Lewenstein distance (edit distance) functions
;; ---------------------------------------------------

(defun* Lewenstein-distance (str1 str2 &key (test 'equal))
  "Return the Lewenstein distance between strings STR1 and STR2
\(a.k.a. edit distance\).

The Lewenstein distance is the minimum number of single-character
insertions, deletions or substitutions required to transform STR1
into STR2.

More generally, STR1 and STR2 can be sequences of elements all of
the same type. The optional keyword argument :test specifies the
function to use to test equality of sequence elements, defaulting
to `equal'."
  (let ((row (apply #'vector (number-sequence 0 (length str2)))))
    (dotimes (i (length str1))
      (setq row (Lewenstein--next-row row str2 (elt str1 i) test)))
    (aref row (1- (length row)))))


(defalias 'edit-distance 'Lewenstein-distance)


(defun Lewenstein--next-row (row string chr equalfun)
  ;; Compute next row of Lewenstein distance matrix.
  (let ((next-row (make-vector (length row) nil))
	(i 0) inscost delcost subcost)
    (aset next-row 0 (1+ (aref row 0)))
    (while (< (incf i) (length row))
      (setq inscost (1+ (aref next-row (1- i)))
	    delcost (1+ (aref row i))
	    subcost (if (funcall equalfun chr (elt string (1- i)))
			(aref row (1- i))
		      (1+ (aref row (1- i)))))
      (aset next-row i (min inscost delcost subcost)))
    next-row))



;; Implementation Note
;; -------------------
;; The standard dynamical-programming solution to computing Lewenstein
;; distance constructs a table of Lewenstein distances to successive prefixes
;; of the target string, row-by-row. Our trie search algorithms are based on
;; constructing the next row of this table as we (recursively) descend the
;; trie. Since the each row only depends on entries in the previous row, we
;; only need to pass a single row of the table down the recursion stack. (A
;; nice description of this algorithm can be found at
;; http://stevehanov.ca/blog/index.php?id=114.)
;;
;; I haven't benchmarked this (let me know the results if you do!), but it
;; seems clear that this algorithm will be much faster than constructing a
;; Lewenstein automata and stepping through it as we descend the trie
;; (similarly to regexp searches, cf. `trie-regexp-match'.)


(defun trie-fuzzy-match
  (trie string distance &optional rankfun maxnum reverse filter resultfun)
  "Return matches for STRING in TRIE within Lewenstein DISTANCE
\(edit distance\) of STRING along with their associated data, in
the order defined by RANKFUN, defaulting to \"lexicographic\"
order \(i.e. the order defined by the trie's comparison
function\). If REVERSE is non-nil, the results are sorted in the
reverse order. Returns nil if no results are found.

Returns a list of matches, with elements of the form:

    ((KEY . DIST) . DATA)

where KEY is a matching key from the trie, DATA its associated
data, and DIST is its Lewenstein distance \(edit distance\) from
STRING.

STRING is a sequence (vector, list or string), whose elements are
of the same type as elements of the trie keys. If STRING is a
string, it must be possible to apply `string' to individual
elements of the keys stored in the trie. The KEYs returned in the
list will be sequences of the same type as STRING.

DISTANCE must be a positive integer. (Note that DISTANCE=0 will
not give meaningful results; use `trie-member' instead.)


RANKFUN overrides the default ordering of the results. If it is t,
matches are instead ordered by increasing Lewenstein distance
\(with same-distance matches ordered lexicographically\).

If RANKFUN is a function, it must accept two arguments, both of
the form:

    ((KEY . DIST) . DATA)

where KEY is a key from the trie, DIST is its Lewenstein
distances from STRING, and DATA is its associated data. RANKFUN
should return non-nil if first argument is ranked strictly higher
than the second, nil otherwise.


The optional integer argument MAXNUM limits the results to the
first MAXNUM matches. Otherwise, all matches are returned.

The FILTER argument sets a filter function for the matches. If
supplied, it is called for each possible match with two
arguments: a (KEY . DIST) cons cell, and DATA. If the filter
function returns nil, the match is not included in the results,
and does not count towards MAXNUM.

RESULTFUN defines a function used to process results before
adding them to the final result list. If specified, it should
accept two arguments: a (KEY . DIST) cons cell, and DATA. Its
return value is what gets added to the final result list, instead
of the default key-dist-data list."

  ;; convert trie from print-form if necessary
  (trie-transform-from-read-warn trie)

  ;; construct rankfun to sort by Lewenstein distance if requested
  (when (eq rankfun t)
    (setq rankfun (trie--construct-Lewenstein-rankfun
		   (trie--comparison-function trie))))

  ;; accumulate results
  (trie--accumulate-results
   rankfun maxnum reverse filter resultfun accumulator nil
   (funcall (trie--mapfun trie)
	    (lambda (node)
	      (trie--do-fuzzy-match
	       node
	       (apply #'vector (number-sequence 0 (length string)))
	       (cond ((stringp string) "") ((listp string) ()) (t []))
	       ;; FIXME: Would it pay to replace these arguments with
	       ;;        dynamically-scoped variables, to save stack space?
	       string distance (if maxnum reverse (not reverse))
	       (trie--comparison-function trie)
	       (trie--construct-equality-function
		(trie--comparison-function trie))
	       (trie--lookupfun trie)
	       (trie--mapfun trie)
	       accumulator))
	    (trie--node-subtree (trie--root trie))
	    (if maxnum reverse (not reverse)))))


(defun trie--do-fuzzy-match (node row seq string distance reverse
			     cmpfun equalfun lookupfun mapfun accumulator)
  ;; Search everything below NODE for matches within Lewenstein distance
  ;; DISTANCE of STRING. ROW is the previous row of the Lewenstein table. SEQ
  ;; is the sequence corresponding to NODE. If COMPLETE is non-nil, return
  ;; completions of matches, otherwise return matches themselves. Remaining
  ;; arguments are corresponding trie functions.

  ;; if we're at a data node and SEQ is within DISTANCE of STRING (i.e. last
  ;; entry of row is <= DISTANCE), accumulate result
  (if (trie--node-data-p node)
      (when (<= (aref row (1- (length row))) distance)
	(funcall accumulator
		 (cons seq (aref row (1- (length row))))
		 (trie--node-data node)))

    ;; build next row of Lewenstein table
    (setq row (Lewenstein--next-row
	       row string (trie--node-split node) equalfun)
	  seq (trie--seq-append seq (trie--node-split node)))

    ;; as long as some row entry is <= DISTANCE, recursively search below NODE
    (when (<= (apply #'min (append row nil)) distance)
      (funcall mapfun
	       (lambda (n)
		 (trie--do-fuzzy-match
		  n row seq string distance reverse
		  cmpfun equalfun lookupfun mapfun accumulator))
	       (trie--node-subtree node)
	       reverse))))



(defun trie-fuzzy-match-stack (trie string distance &optional reverse)
  "Return an object that allows fuzzy matches to be accessed
as if they were a stack.

The stack is sorted in \"lexicographic\" order, i.e. the order
defined by TRIE's comparison function, or in reverse order if
REVERSE is non-nil. Calling `trie-stack-pop' pops the top element
from the stack. Each stack element has the form:

    ((KEY . DIST) . DATA)

where KEY is a matching key from the trie, DATA its associated
data, and DIST is its Lewenstein distance \(edit distance\) from
STRING.

STRING is a sequence (vector, list or string), whose elements are
of the same type as elements of the trie keys. If STRING is a
string, it must be possible to apply `string' to individual
elements of the keys stored in the trie. The KEYs in the matches
returned by `trie-stack-pop' will be sequences of the same type
as STRING.

DISTANCE is a positive integer. The fuzzy matches in the stack
will be within Lewenstein distance \(edit distance\) DISTANCE of
STRING."
  ;; convert trie from print-form if necessary
  (trie-transform-from-read-warn trie)
  ;; if stack functions aren't defined for trie type, throw error
  (cond
   ((not (functionp (trie--stack-createfun trie)))
    (error "Trie type does not support stack operations"))
   ;; fuzzy-match-stacks don't work for distance=0; return a `trie-stack'
   ;; instead
   ((= distance 0)
    (trie--stack-create trie string reverse))
   (t ;; otherwise, create and initialise a fuzzy match stack
    (trie--fuzzy-match-stack-create trie string distance reverse))))


(defun trie--fuzzy-match-stack-construct-store
    (trie string distance &optional reverse)
  ;; Construct store for fuzzy stack based on TRIE.
  (let ((seq (cond ((stringp string) "") ((listp string) ()) (t [])))
	store)
    (push (list seq
		(funcall (trie--stack-createfun trie)
			 (trie--node-subtree (trie--root trie))
			 reverse)
		string distance
		(apply #'vector (number-sequence 0 (length string))))
	  store)
    (trie--fuzzy-match-stack-repopulate
     store reverse
     (trie--comparison-function trie)
     (trie--lookupfun trie)
     (trie--stack-createfun trie)
     (trie--stack-popfun trie)
     (trie--stack-emptyfun trie))))


(defun trie--fuzzy-match-stack-repopulate
  (store reverse comparison-function _lookupfun
	 stack-createfun stack-popfun stack-emptyfun)
  ;; Recursively push matching children of the node at the head of STORE
  ;; onto STORE, until a data node is reached. REVERSE is the usual
  ;; query argument, and the remaining arguments are the corresponding
  ;; trie functions.

  (when store
    (let ((equalfun (trie--construct-equality-function comparison-function))
	  nextrow)

      (destructuring-bind (seq node string distance row) (car store)
	(setq node (funcall stack-popfun node))
	(when (funcall stack-emptyfun (nth 1 (car store)))
	  ;; using (pop store) here produces irritating compiler warnings
	  (setq store (cdr store)))

	;; push children of node at head of store that are within DISTANCE of
	;; STRING, until we find a data node where entire SEQ is within
	;; DISTANCE of STRING (i.e. last entry of row is <= DISTANCE)
	(while (and node
		    (not (and (trie--node-data-p node)
			      (<= (aref row (1- (length row))) distance))))
	  ;; drop data nodes whose SEQ is greater than DISTANCE
	  (unless (trie--node-data-p node)
	    (setq nextrow (Lewenstein--next-row
			   row string (trie--node-split node) equalfun))
	    ;; push children of non-data nodes whose SEQ is less than DISTANCE
	    ;; onto stack
	    (when (<= (apply #'min (append row nil)) distance)
	      (push
	       (list (trie--seq-append seq (trie--node-split node))
		     (funcall stack-createfun
			      (trie--node-subtree node) reverse)
		     string distance nextrow)
	       store)))
	  ;; get next node from stack
	  (when (setq node (car store))
	    (setq seq (nth 0 node)
		  string (nth 2 node)
		  distance (nth 3 node)
		  row (nth 4 node)
		  node (funcall stack-popfun (nth 1 node)))
	    ;; drop head of stack if nodes are exhausted
	    (when (funcall stack-emptyfun (nth 1 (car store)))
	      (setq store (cdr store)))))

	;; push next fuzzy match onto head of stack
	(when node
	  (push (cons (cons seq (aref row (1- (length row))))
		      (trie--node-data node))
		store))))))


(heap--when-generators
 (iter-defun trie-fuzzy-match-iter (trie string distance &optional reverse)
   "Return an iterator object for fuzzy matches to STRING in TRIE.

Calling `iter-next' on this object will return the next match
within DISTANCE of STRING in TRIE, in \"lexicographic\" order,
i.e. the order defined by the trie's comparison function, or in
reverse order if REVERSE is non-nil. Each returned element has
the form:

    ((KEY . DIST) . DATA)

where KEY is a matching key from the trie, DATA its associated
data, and DIST is its Lewenstein distance \(edit distance\) from
STRING.

STRING is a sequence (vector, list or string) whose elements are
of the same type as elements of the trie keys. If STRING is a
string, it must be possible to apply `string' to individual
elements of the keys stored in the trie. The KEYs in the matches
returned by `iter-next' will be sequences of the same type as
STRING.

DISTANCE is a positive integer. The fuzzy matches in the stack
will be within Lewenstein distance \(edit distance\) DISTANCE of
STRING.

Note that any modification to TRIE *immediately* invalidates all
iterators created from TRIE before the modification \(in
particular, calling `iter-next' will give unpredictable
results\)."
   (let ((stack (trie-fuzzy-match-stack trie string distance reverse)))
     (while (not (trie-stack-empty-p stack))
       (iter-yield (trie-stack-pop stack))))))




;; ================================================================
;;                        Fuzzy completing

(defun trie-fuzzy-complete
  (trie prefix distance &optional rankfun maxnum reverse filter resultfun)
  "Return completions of prefixes within Lewenstein DISTANCE of PREFIX
along with their associated data, in the order defined by
RANKFUN, defaulting to \"lexicographic\" order \(i.e. the order
defined by the trie's comparison function\). If REVERSE is
non-nil, the results are sorted in the reverse order. Returns nil
if no results are found.

Returns a list of completions, with elements of the form:

    ((KEY DIST PFXLEN) . DATA)

where KEY is a matching completion from the trie, DATA its
associated data, PFXLEN is the length of the prefix part of KEY,
and DIST is its Lewenstein distance \(edit distance\) from
PREFIX.

PREFIX is a sequence (vector, list or string), whose elements are
of the same type as elements of the trie keys. If PREFIX is a
string, it must be possible to apply `string' to individual
elements of the keys stored in the trie. The KEYs returned in the
list will be sequences of the same type as PREFIX.

DISTANCE must be a positive integer. (Note that DISTANCE=0 will
not give meaningful results; use `trie-complete' instead.)

The optional integer argument MAXNUM limits the results to the
first MAXNUM matches. Otherwise, all matches are returned.


RANKFUN overrides the default ordering of the results. If it is t,
matches are instead ordered by increasing Lewenstein distance of
their prefix \(with same-distance prefixes ordered
lexicographically\).

If RANKFUN is a function, it must accept two arguments, both of
the form:

    ((KEY DIST PFXLEN) . DATA)

where KEY is a key from the trie, DIST is its Lewenstein
distances from PREFIX, and DATA is its associated data. RANKFUN
should return non-nil if first argument is ranked strictly higher
than the second, nil otherwise.


The FILTER argument sets a filter function for the matches. If
supplied, it is called for each possible match with two
arguments: a (KEY DIST PFXLEN) list, and DATA. If the filter
function returns nil, the match is not included in the results,
and does not count towards MAXNUM.

RESULTFUN defines a function used to process results before
adding them to the final result list. If specified, it should
accept two arguments: a (KEY DIST PFXLEN) list, and DATA. Its
return value is what gets added to the final result list, instead
of the default key-dist-data list."

  ;; convert trie from print-form if necessary
  (trie-transform-from-read-warn trie)

  ;; construct rankfun to sort by Lewenstein distance if requested
  (when (eq rankfun t)
    (setq rankfun (trie--construct-Lewenstein-rankfun
		   (trie--comparison-function trie))))

  ;; accumulate results
  (trie--accumulate-results
   rankfun maxnum reverse filter resultfun accumulator nil
   (funcall (trie--mapfun trie)
	    (lambda (node)
	      (trie--do-fuzzy-complete
	       node
	       (apply #'vector (number-sequence 0 (length prefix)))
	       (cond ((stringp prefix) "") ((listp prefix) ()) (t []))
	       (length prefix) 0
	       ;; FIXME: Would it pay to replace these arguments with
	       ;;        dynamically-scoped variables, to save stack space?
	       prefix distance (if maxnum reverse (not reverse))
	       (trie--comparison-function trie)
	       (trie--construct-equality-function
		(trie--comparison-function trie))
	       (trie--lookupfun trie)
	       (trie--mapfun trie)
	       accumulator))
	    (trie--node-subtree (trie--root trie))
	    (if maxnum reverse (not reverse)))))


(defun trie--do-fuzzy-complete (node row seq pfxcost pfxlen
				prefix distance reverse
				cmpfun equalfun lookupfun mapfun accumulator)
  ;; Search everything below NODE for completions of prefixes within
  ;; Lewenstein distance DISTANCE of PREFIX. ROW is the previous row of the
  ;; Lewenstein table. SEQ is the sequence corresponding to NODE. PFXCOST is
  ;; minimum distance of any prefix of seq. Remaining arguments are
  ;; corresponding trie functions.

  ;; if we're at a data node and SEQ is within DISTANCE of PREFIX (i.e. last
  ;; entry of row is <= DISTANCE), accumulate result
  (if (trie--node-data-p node)
      (when (<= (aref row (1- (length row))) distance)
	(funcall accumulator
		 (list seq (aref row (1- (length row))) (length seq))
		 (trie--node-data node)))

    ;; build next row of Lewenstein table
    (setq row (Lewenstein--next-row
	       row prefix (trie--node-split node) equalfun)
	  seq (trie--seq-append seq (trie--node-split node)))
    (when (<= (aref row (1- (length row))) pfxcost)
      (setq pfxcost (aref row (1- (length row)))
	    pfxlen (length seq)))

    ;; as long as some row entry is < DISTANCE, recursively search below NODE
    (if (<= (apply #'min (append row nil)) distance)
	(funcall mapfun
		 (lambda (n)
		   (trie--do-fuzzy-complete
		    n row seq pfxcost pfxlen prefix distance reverse
		    cmpfun equalfun lookupfun mapfun accumulator))
		 (trie--node-subtree node)
		 reverse)

      ;; otherwise, if we've found a prefix within DISTANCE of PREFIX,
      ;; accumulate all completions below node
      (when (<= pfxcost distance)
	(trie--mapc
	 (lambda (n s)
	   (funcall accumulator (list s pfxcost pfxlen) (trie--node-data n)))
	 mapfun node seq reverse))
      )))



(defun trie-fuzzy-complete-stack (trie prefix distance &optional reverse)
  "Return an object that allows fuzzy completions to be accessed
as if they were a stack.

The stack is sorted in \"lexicographic\" order, i.e. the order
defined by TRIE's comparison function, or in reverse order if
REVERSE is non-nil. Calling `trie-stack-pop' pops the top element
from the stack. Each stack element has the form:

    ((KEY DIST PFXLEN) . DATA)

where KEY is a matching completion from the trie, DATA its
associated data, PFXLEN is the length of the prefix part of KEY,
and DIST is the Lewenstein distance \(edit distance\) from PREFIX
of the prefix whose completion is KEY.

PREFIX is a sequence (vector, list or string), whose elements are
of the same type as elements of the trie keys. If PREFIX is a
string, it must be possible to apply `string' to individual
elements of the keys stored in the trie. The KEYs in the stack
elements will be sequences of the same type as PREFIX.

DISTANCE is a positive integer. The fuzzy completions in the
stack will have prefixes within Lewenstein distance \(edit
distance\) DISTANCE of PREFIX. (Note that DISTANCE=0 will not
give meaningful results; use `trie-complete-stack' instead.)"
  ;; convert trie from print-form if necessary
  (trie-transform-from-read-warn trie)
  (cond
   ;; if stack functions aren't defined for trie type, throw error
   ((not (functionp (trie--stack-createfun trie)))
    (error "Trie type does not support stack/iterator operations"))
   ;; fuzzy-complete-stacks don't work for distance=0; return
   ;; a `trie-complete-stack' instead
   ((= distance 0)
    (trie--complete-stack-create trie prefix reverse))
   (t ;; otherwise, create and initialise a fuzzy stack
    (trie--fuzzy-complete-stack-create trie prefix distance reverse))))


(defun trie--fuzzy-complete-stack-construct-store
    (trie prefix distance &optional reverse)
  ;; Construct store for fuzzy completion stack based on TRIE.
  (let ((seq (cond ((stringp prefix) "") ((listp prefix) ()) (t [])))
	store)
    (push (list seq
		(funcall (trie--stack-createfun trie)
			 (trie--node-subtree (trie--root trie))
			 reverse)  ; node
		prefix distance
		(apply #'vector (number-sequence 0 (length prefix)))  ; row
		(length prefix) 0)  ; pfxcost pfxlen
	  store)
    (trie--fuzzy-complete-stack-repopulate
     store reverse
     (trie--comparison-function trie)
     (trie--lookupfun trie)
     (trie--stack-createfun trie)
     (trie--stack-popfun trie)
     (trie--stack-emptyfun trie))))


(defun trie--fuzzy-complete-stack-repopulate
  (store reverse comparison-function _lookupfun
	 stack-createfun stack-popfun stack-emptyfun)
  ;; Recursively push matching children of the node at the head of STORE
  ;; onto STORE, until a data node is reached. REVERSE is the usual
  ;; query argument, and the remaining arguments are the corresponding
  ;; trie functions.

  (when store
    (let ((equalfun (trie--construct-equality-function comparison-function)))

      (destructuring-bind (seq node prefix distance row pfxcost pfxlen)
	  (car store)
	(setq node (funcall stack-popfun node))
	(when (funcall stack-emptyfun (nth 1 (car store)))
	  ;; using (pop store) here produces irritating compiler warnings
	  (setq store (cdr store)))

	;; push children of node at head of store that are within DISTANCE of
	;; PREFIX, until we either find a data node whose entire SEQ is within
	;; DISTANCE of PREFIX (i.e. last entry of row is <= DISTANCE), or
	;; we've found a prefix within DISTANCE of PREFIX and are gathering
	;; all its completions
	(while (and node
		    (not (and (trie--node-data-p node)
			      (or (eq distance t)  ; completing a prefix
				  (<= (aref row (1- (length row))) distance))
			      )))
	   ;; drop data nodes whose SEQ is greater than DISTANCE
	  (unless (trie--node-data-p node)
	    ;; build next row of Lewenstein table
	    (setq row (Lewenstein--next-row
		       row prefix (trie--node-split node) equalfun)
		  seq (trie--seq-append seq (trie--node-split node)))
	    (when (<= (aref row (1- (length row))) pfxcost)
	      (setq pfxcost (aref row (1- (length row)))
		    pfxlen (length seq)))

	    (cond
	     ;; if we're completing a prefix, always push next node onto stack
	     ((eq distance t)
	      (push
	       (list seq
		     (funcall stack-createfun
			      (trie--node-subtree node) reverse)
		     prefix t row pfxcost pfxlen)
	       store))

	     ;; if we've found a prefix within DISTANCE of PREFIX, then
	     ;; everything below node belongs on stack
	     ((<= (aref row (1- (length row))) distance)
	      (push
	       (list seq
		     (funcall stack-createfun
			      (trie--node-subtree node) reverse)
		     ;; t in distance slot indicates completing
		     prefix t row pfxcost pfxlen)
	       store))

	     ;; if some row entry for non-data node is <= DISTANCE, push node
	     ;; onto stack
	     ((<= (apply #'min (append row nil)) distance)
	      (push
	       (list seq
		     (funcall stack-createfun
			      (trie--node-subtree node) reverse)
		     prefix distance row pfxcost pfxlen)
	       store))))

	  ;; get next node from stack
	  (when (setq node (car store))
	    (setq seq (nth 0 node)
		  prefix (nth 2 node)
		  distance (nth 3 node)
		  row (nth 4 node)
		  node (funcall stack-popfun (nth 1 node)))
	    ;; drop head of stack if nodes are exhausted
	    (when (funcall stack-emptyfun (nth 1 (car store)))
	      (setq store (cdr store)))))


	;; push next fuzzy completion onto head of stack
	(when node
	  (push (cons (list seq pfxcost pfxlen) (trie--node-data node))
		store))))))


(heap--when-generators
 (iter-defun trie-fuzzy-complete-iter (trie prefix distance &optional reverse)
   "Return an iterator object for fuzzy matches of STRING in TRIE.

Calling `iter-next' on this object will return the next match
within DISTANCE of STRING in TRIE, in \"lexicographic\" order,
i.e. the order defined by the trie's comparison function, or in
reverse order if REVERSE is non-nil. Each returned element has
the form:

    ((KEY DIST PFXLEN) . DATA)

where KEY is a matching completion from the trie, DATA its
associated data, PFXLEN is the length of the prefix part of KEY,
and DIST is the Lewenstein distance \(edit distance\) of that
prefix part from PREFIX

PREFIX is a sequence (vector, list or string), whose elements are
of the same type as elements of the trie keys. If PREFIX is a
string, it must be possible to apply `string' to individual
elements of the keys stored in the trie. The KEYs in the elements
returned by `iter-next' will be sequences of the same type as
PREFIX.

DISTANCE is a positive integer. The fuzzy completions returned by
`iter-next' will have prefixes within Lewenstein distance \(edit
distance\) DISTANCE of PREFIX.

Note that any modification to TRIE *immediately* invalidates all
iterators created from TRIE before the modification \(in
particular, calling `iter-next' will give unpredictable
results\)."
   (let ((stack (trie-fuzzy-complete-stack trie prefix distance reverse)))
     (while (not (trie-stack-empty-p stack))
       (iter-yield (trie-stack-pop stack))))))





;; ----------------------------------------------------------------
;;            Pretty-print tries during edebug

;; Note:
;; -----

;; We advise the `edebug-prin1' and `edebug-prin1-to-string' functions
;; (actually, aliases) so that they print "#<trie>" instead of the full
;; print form for tries.
;;
;; This is because, if left to its own devices, edebug hangs for ages
;; whilst printing large tries, and you either have to wait for a *very*
;; long time for it to finish, or kill Emacs entirely. (Even C-g C-g
;; fails!)
;;
;; We do this also for lists of tries, since those occur quite often,
;; but not for other sequence types or deeper nested structures, to keep
;; the implementation as simple as possible.
;;
;; Since the print form of a trie is practically incomprehensible
;; anyway, we don't lose much by doing this. If you *really* want to
;; print tries in full whilst edebugging, despite this warning, disable
;; the advice.
;;
;; FIXME: We should probably use the `cust-print' features instead of advice
;; here.


(eval-when-compile
  (require 'edebug)
  (require 'advice))

(defun trie--prin1 (_trie stream)
  (princ "#<trie>" stream))

(defun trie--edebug-pretty-print (object)
  (cond
   ((trie-p object) "#<trie>")
   ((null object) "nil")
   ((let ((tlist object) (test t))
      (while (or (trie-p (car-safe tlist))
		 (and tlist (setq test nil)))
	(setq tlist (cdr tlist)))
      test)
    (concat "(" (mapconcat (lambda (_dummy) "#<trie>") object " ") ")"))
;; ((vectorp object)
;;  (let ((pretty "[") (len (length object)))
;;    (dotimes (i (1- len))
;; 	(setq pretty
;; 	      (concat pretty
;; 		      (if (trie-p (aref object i))
;; 			  "#<trie>" (prin1-to-string (aref object i))) " ")))
;;    (concat pretty
;; 	      (if (trie-p (aref object (1- len)))
;; 		  "#<trie>" (prin1-to-string (aref object (1- len))))
;; 	      "]")))
   ))

(if (fboundp 'cl-print-object)
    (cl-defmethod cl-print-object ((object trie-) stream)
      (trie--prin1 object stream))

  (when (fboundp 'ad-define-subr-args)
    (ad-define-subr-args 'edebug-prin1 '(object &optional printcharfun)))

  (defadvice edebug-prin1
      (around trie activate compile preactivate)
    (let ((pretty (trie--edebug-pretty-print object)))
      (if pretty
	  (progn
	    (prin1 pretty printcharfun)
	    (setq ad-return-value pretty))
        ad-do-it)))

  (when (fboundp 'ad-define-subr-args)
    (ad-define-subr-args 'edebug-prin1-to-string '(object &optional noescape)))

  (defadvice edebug-prin1-to-string
      (around trie activate compile preactivate)
    (let ((pretty (trie--edebug-pretty-print object)))
      (if pretty
	  (setq ad-return-value pretty)
        ad-do-it))))

;;;; ChangeLog:

;; 2017-08-16  Toby S. Cubitt  <tsc25@cantab.net>
;; 
;; 	Upgrade data structure packages to latest versions.
;; 
;; 2016-07-11  Paul Eggert	 <eggert@cs.ucla.edu>
;; 
;; 	Fix some quoting problems in doc strings
;; 
;; 	Most of these are minor issues involving, e.g., quoting `like this' 
;; 	instead of 'like this'.	 A few involve escaping ` and ' with a preceding
;; 	\= when the characters should not be turned into curved single quotes.
;; 
;; 2014-10-15  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/trie/trie.el (edebug-prin1, edebug-prin1-to-string): Use 
;; 	advice-add when available.
;; 
;; 2012-11-16  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* trie.el (trie--node-data): Simplify.
;; 
;; 2012-09-11  Toby S. Cubitt  <tsc25@cantab.net>
;; 
;; 	Updated trie.el package to version 0.2.6
;; 
;; 2012-05-05  Toby S. Cubitt  <tsc25@cantab.net>
;; 
;; 	trie.el, tNFA.el, dict-tree.el: minor package header and autoload fixes
;; 
;; 	* trie.el: remove emacs version dependency.
;; 
;; 	* tNFA.el: add missing autoload cookies.
;; 
;; 	* dict-tree.el: minor change to package description, to match the other
;; 	data structure packages.
;; 
;; 2012-04-30  Toby S. Cubitt  <tsc25@cantab.net>
;; 
;; 	Minor fixes to commentaries, package headers, and whitespace
;; 
;; 	* queue.el: fix description of data structure in Commentary; add
;; 	Maintainer
;; 	 header.
;; 
;; 	* queue.el, heap.el, tNFA.el, trie.el, dict-tree.el: trivial whitespace
;; 	fixes.
;; 
;; 2012-04-29  Toby S. Cubitt  <tsc25@cantab.net>
;; 
;; 	Add trie.el
;; 



(provide 'trie)

;;; trie.el ends here
