;;; dict-tree.el --- Dictionary data structure  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2015, 2017  Free Software Foundation, Inc

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.14
;; Keywords: extensions, matching, data structures
;;           trie, tree, dictionary, completion, regexp
;; Package-Requires: ((trie "0.3") (tNFA "0.1.1") (heap "0.3"))
;; URL: http://www.dr-qubit.org/emacs.php

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
;; A dict-tree (created using `dictree-create') is used to store strings,
;; along with arbitrary data associated with each string. (Note that the
;; "strings" can be any sequence data type, not just Elisp strings.) As well
;; as basic data insertion (`dictree-insert'), manipulation
;; (`dictree-insert'), and retrieval (`dictree-lookup', `dictree-member-p'), a
;; dict-tree can perform sophisticated queries on strings, including:
;;
;; - retrieve all completions of a prefix
;;   (`dictree-complete')
;;
;; - retrieve all strings that match a regular expression
;;   (`dictree-regexp-search')
;;
;; - retrieve all fuzzy matches to a string, i.e. matches within a specified
;;   Lewenstein distance (a.k.a. edit distance)
;;   (`dictree-fuzzy-match')
;;
;; - retrieve all fuzzy completions of a prefix, i.e. completions of prefixes
;;   within a specified Lewenstein distance
;;   (`dictree-fuzzy-complete')
;;
;; The results of all of these queries can be ranked in alphabetical order, or
;; according to any other desired ranking. The results can also be limited to
;; a given number of matches.
;;
;; These sophisticated string queries are fast even for very large dict-trees,
;; and dict-tree's also cache query results (and automatically keep these
;; caches synchronised) to speed up queries even further.
;;
;; Other functions allow you to:
;;
;; - create dict-tree stack objects, which allow efficient access to the
;;   strings in the dictionary or in query results as though they were sorted
;;   on a stack (useful for designing efficient algorithms on top of
;;   dict-trees)
;;   (`dictree-stack', `dictree-complete-stack', `dictree-regexp-stack',
;;    `dictree-fuzzy-match-stack', `dictree-fuzzy-complete-stack')
;;
;; - generate dict-tree iterator objects which allow you to retrieve
;;   successive elements by calling `iter-next'
;;   (`dictree-iter', `dictree-complete-iter', `dictree-regexp-iter',
;;    `dictree-fuzzy-match-iter', `dictree-fuzzy-complete-iter')
;;
;; - map over all strings in alphabetical order
;;   (`dictree-mapc', `dictree-mapcar' and `dictree-mapf')
;;
;; Dict-trees can be combined together into a "meta dict-tree", which combines
;; the data from identical keys in its constituent dict-trees, in whatever way
;; you specify (`dictree-create-meta-dict'). Any number of dict-trees can be
;; combined in this way. Meta-dicts behave *exactly* like dict-trees: all of
;; the above functions work on meta-dicts as well as dict-trees, and
;; meta-dicts can themselves be used in new meta-dicts.
;;
;; The package also provides persistent storage of dict-trees to file.
;; (`dictree-save', `dictree-write', `dictee-load')
;;
;; This package uses the trie package trie.el, the tagged NFA package tNFA.el,
;; and the heap package heap.el.


;;; Code:

(eval-when-compile (require 'cl))
(require 'trie)
(require 'tNFA)


;;; ================================================================
;;;            Replacements for CL and Elisp functions

;; copied from cl-extra.el
(defun dictree--subseq (seq start &optional end)
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



;; `goto-line' without messing around with mark and messages
(defun dictree--goto-line (line)
  "Goto line LINE, counting from line 1 at beginning of buffer."
  (goto-char 1)
  (if (eq selective-display t)
      (re-search-forward "[\n\C-m]" nil 'no-error (1- line))
    (forward-line (1- line))))




;;; ====================================================================
;;;  Internal functions and variables for use in the dictionary package

(defvar dictree-loaded-list nil
  "Stores list of loaded dictionaries.")


;; ----------------------------------------------------------------
;;                   Dictionary data cell structures

;; Note: It would be more elegant to use a defstruct for the data cells,
;;       but the problem is that the resulting setf in
;;       `dictree--wrap-insfun' won't get expanded into the cell-data
;;       accessor function at compile-time because it's burried inside a
;;       backquote construct. Not only is it inelegant to have to expand
;;       macros at run-time whenever `dictree--wrap-insfun' is called,
;;       but it also requires the 'cl-macs package to be loaded at
;;       run-time rather than just at compile-time. We could use
;;       `lexical-let' instead, but it doesn't seem worth it here.

;; wrap data in a cons cell
(defalias 'dictree--cell-create 'cons)  ; INTERNAL USE ONLY

;; get data component from data cons cell
(defalias 'dictree--cell-data 'car)  ; INTERNAL USE ONLY

;; get property list component from data cons cell
(defalias 'dictree--cell-plist 'cdr)  ; INTERNAL USE ONLY

;; set data component of data cons cell
(defalias 'dictree--cell-set-data 'setcar)  ; INTERNAL USE ONLY

;; set property list component of data cons cell
(defalias 'dictree--cell-set-plist 'setcdr)  ; INTERNAL USE ONLY

;; define setf methods so we can use setf abstraction wherever possible
(defsetf dictree--cell-data dictree--cell-set-data)
(defsetf dictree--cell-plist dictree--cell-set-plist)


;; ----------------------------------------------------------------
;;                 Dictionary cache entry structures

;; Note: We *could* use a defstruct for the cache entries, but for
;;       something this simple it doesn't seem worth it, especially
;;       given that we're using the defalias approach anyway for the
;;       data cells (above).

;; Construct and return a completion cache entry
(defalias 'dictree--cache-create 'cons)  ; INTERNAL USE ONLY

;; Return the completions list for cache entry CACHE
(defalias 'dictree--cache-results 'car)  ; INTERNAL USE ONLY

;; Return the max number of completions returned for cache entry CACHE
(defalias 'dictree--cache-maxnum 'cdr)  ; INTERNAL USE ONLY

;; Set the completions list for cache entry CACHE
(defalias 'dictree--cache-set-results 'setcar)  ; INTERNAL USE ONLY

;; Set the completions list for cache entry CACHE
(defalias 'dictree--cache-set-maxnum 'setcdr)  ; INTERNAL USE ONLY

;; define setf methods so we can use setf abstraction wherever possible
(defsetf dictree--cache-results dictree--cache-set-results)
(defsetf dictree--cache-maxnum dictree--cache-set-maxnum)



;; ----------------------------------------------------------------
;;                     Wrapping functions

;; return wrapped insfun to deal with data wrapping
(trie--if-lexical-binding
    (defun dictree--wrap-insfun (insfun)  ; INTERNAL USE ONLY
      (lambda (new old)
	(dictree--cell-set-data old (funcall insfun
					     (dictree--cell-data new)
					     (dictree--cell-data old)))
	old))
  (defun dictree--wrap-insfun (insfun)  ; INTERNAL USE ONLY
    `(lambda (new old)
       (dictree--cell-set-data old (,insfun (dictree--cell-data new)
					    (dictree--cell-data old)))
       old)))


;; return wrapped rankfun to deal with data wrapping
(trie--if-lexical-binding
    (defun dictree--wrap-rankfun (rankfun)  ; INTERNAL USE ONLY
      (lambda (a b)
	(funcall rankfun
		 (cons (car a) (dictree--cell-data (cdr a)))
		 (cons (car b) (dictree--cell-data (cdr b))))))
  (defun dictree--wrap-rankfun (rankfun)  ; INTERNAL USE ONLY
    `(lambda (a b)
       (,rankfun (cons (car a) (dictree--cell-data (cdr a)))
  		 (cons (car b) (dictree--cell-data (cdr b)))))))


;; return wrapped rankfun to ignore regexp grouping data
(trie--if-lexical-binding
    (defun dictree--wrap-regexp-rankfun (rankfun)
      (lambda (a b)
	;; if car of argument contains a key+group list rather than a straight
	;; key, remove group list
	;; FIXME: the test for straight key, below, will fail if the key is a
	;;        list, and the first element of the key is itself a list
	;;        (there might be no easy way to fully fix this...)
	(if (or (atom (car a))
		(and (listp (car a)) (not (sequencep (caar a)))))
	    (setq a (cons (car a) (dictree--cell-data (cdr a))))
	  (setq a (cons (caar a) (dictree--cell-data (cdr a)))))
	(if (or (atom (car b))
		(and (listp (car b)) (not (sequencep (caar b)))))
	    (setq b (cons (car b) (dictree--cell-data (cdr b))))
	  (setq b (cons (caar b) (dictree--cell-data (cdr b)))))
	(funcall rankfun a b)))
  (defun dictree--wrap-regexp-rankfun (rankfun)
    `(lambda (a b)
       ;; if car of argument contains a key+group list rather than a straight
       ;; key, remove group list
       ;; FIXME: the test for straight key, below, will fail if the key is a
       ;;        list, and the first element of the key is itself a list
       ;;        (there might be no easy way to fully fix this...)
       (if (or (atom (car a))
	       (and (listp (car a)) (not (sequencep (caar a)))))
	    (setq a (cons (car a) (dictree--cell-data (cdr a))))
  	 (setq a (cons (caar a) (dictree--cell-data (cdr a)))))
       (if (or (atom (car b))
	       (and (listp (car b)) (not (sequencep (caar b)))))
	   (setq b (cons (car b) (dictree--cell-data (cdr b))))
  	 (setq b (cons (caar b) (dictree--cell-data (cdr b)))))
       (,rankfun a b))))

;; return wrapped sortfun to ignore regexp grouping data
(trie--if-lexical-binding
    (defun dictree--wrap-regexp-sortfun (cmpfun &optional reverse)
	(let ((sortfun (trie-construct-sortfun cmpfun reverse)))
	  (lambda (a b)
	    ;; if car of argument contains a key+group list rather than a
	    ;; straight key, remove group list
	    ;; FIXME: the test for straight key, below, will fail if the key
	    ;;        is a list, and the first element of the key is itself a
	    ;;        list (there might be no easy way to fully fix this...)
	    (if (or (atom (car a))
		    (and (listp (car a)) (not (sequencep (caar a)))))
		(setq a (car a))
	      (setq a (caar a)))
	    (if (or (atom (car b))
		    (and (listp (car b)) (not (sequencep (caar b)))))
		(setq b (car b))
	      (setq b (caar b)))
	    (funcall sortfun a b))))
  (defun dictree--wrap-regexp-sortfun (cmpfun &optional reverse)
    (let ((sortfun (trie-construct-sortfun cmpfun reverse)))
      `(lambda (a b)
	 ;; if car of argument contains a key+group list rather than a
	 ;; straight key, remove group list
	 ;; FIXME: the test for straight key, below, will fail if the key
	 ;;        is a list, and the first element of the key is itself a
	 ;;        list (there might be no easy way to fully fix this...)
	 (if (or (atom (car a))
		 (and (listp (car a)) (not (sequencep (caar a)))))
	     (setq a (car a))
	   (setq a (caar a)))
	 (if (or (atom (car b))
		 (and (listp (car b)) (not (sequencep (caar b)))))
	     (setq b (car b))
	   (setq b (caar b)))
	 (,sortfun a b)))))


;; return wrapped rankfun to ignore fuzzy query distance data
(trie--if-lexical-binding
    (defun dictree--wrap-fuzzy-rankfun (rankfun)  ; INTERNAL USE ONLY
      (lambda (a b)
	(funcall rankfun
		 (cons (nth 0 (car a)) (dictree--cell-data (cdr a)))
		 (cons (nth 0 (car b)) (dictree--cell-data (cdr b))))))
  (defun dictree--wrap-fuzzy-rankfun (rankfun)  ; INTERNAL USE ONLY
    `(lambda (a b)
       (,rankfun (cons (nth 0 (car a)) (dictree--cell-data (cdr a)))
		 (cons (nth 0 (car b)) (dictree--cell-data (cdr b)))))))

;; return wrapped sortfun to ignore fuzzy query distance data
(trie--if-lexical-binding
    (defun dictree--wrap-fuzzy-sortfun (cmpfun &optional reverse)
      (let ((sortfun (trie-construct-sortfun cmpfun reverse)))
	(lambda (a b) (funcall sortfun (car a) (car b)))))
  (defun dictree--wrap-fuzzy-sortfun (cmpfun &optional reverse)
    (let ((sortfun (trie-construct-sortfun cmpfun reverse)))
      `(lambda (a b) (,sortfun (car a) (car b))))))


;; return wrapped combfun to deal with data wrapping
(trie--if-lexical-binding
    (defun dictree--wrap-combfun (combfun)  ; INTERNAL USE ONLY
      (lambda (cell1 cell2)
	(dictree--cell-create
	 (funcall combfun
		  (dictree--cell-data cell1)
		  (dictree--cell-data cell2))
	 (append (dictree--cell-plist cell1)
		 (dictree--cell-plist cell2)))))
  (defun dictree--wrap-combfun (combfun)  ; INTERNAL USE ONLY
    `(lambda (cell1 cell2)
       (dictree--cell-create
	(,combfun (dictree--cell-data cell1)
		  (dictree--cell-data cell2))
	(append (dictree--cell-plist cell1)
		(dictree--cell-plist cell2))))))


;; return wrapped filter function to deal with data wrapping
(trie--if-lexical-binding
    (defun dictree--wrap-filter (filter)  ; INTERNAL USE ONLY
      (lambda (key data) (funcall filter key (dictree--cell-data data))))
  (defun dictree--wrap-filter (filter)  ; INTERNAL USE ONLY
    `(lambda (key data) (,filter key (dictree--cell-data data)))))


;; return wrapped result function to deal with data wrapping
(trie--if-lexical-binding
    (defun dictree--wrap-resultfun (resultfun)  ; INTERNAL USE ONLY
      (lambda (res)
	(funcall resultfun (car res) (dictree--cell-data (cdr res)))))
  (defun dictree--wrap-resultfun (resultfun)  ; INTERNAL USE ONLY
    `(lambda (res) (,resultfun (car res) (dictree--cell-data (cdr res))))))


;; construct lexicographic sort function from DICT's comparison function
(trie--if-lexical-binding
    (defun dictree--construct-sortfun (dict)  ; INTERNAL USE ONLY
      (let ((sortfun (trie-construct-sortfun
		      (dictree-comparison-function dict))))
	(lambda (a b) (funcall sortfun (car a) (car b)))))
    (defun dictree--construct-sortfun (dict)  ; INTERNAL USE ONLY
      `(lambda (a b)
	 (,(trie-construct-sortfun (dictree-comparison-function (car dict)))
	  (car a) (car b)))))




;; ----------------------------------------------------------------
;;                 The dictionary data structures

(defstruct
  (dictree-
   :named
   (:constructor nil)
   (:constructor dictree--create
		 (&optional
		  filename
		  (name (and filename
			     (file-name-sans-extension
			      (file-name-nondirectory filename))))
		  autosave
		  _unlisted
		  (comparison-function #'<)
		  (insert-function (lambda (a _b) a))
		  (rank-function (lambda (a b) (> (cdr a) (cdr b))))
		  (cache-policy 'time)
		  cache-threshold
		  (cache-update-policy 'synchronize)
		  key-savefun key-loadfun
		  data-savefun data-loadfun
		  plist-savefun plist-loadfun
		  (trie-type 'avl)
		  &aux
		  (modified nil)
		  (trie (make-trie comparison-function trie-type))
		  (lookup-cache nil)
		  (complete-cache nil)
		  (regexp-cache nil)
		  (fuzzy-match-cache nil)
		  (fuzzy-complete-cache nil)
		  (meta-dict-list nil)
		  ))
   (:constructor dictree--create-custom
		 (&optional
		  filename
		  (name (and filename
			     (file-name-sans-extension
			      (file-name-nondirectory filename))))
		  autosave
		  _unlisted
		  (comparison-function #'<)
		  (insert-function (lambda (a _b) a))
		  (rank-function (lambda (a b) (> (cdr a) (cdr b))))
		  (cache-policy 'time)
		  cache-threshold
		  (cache-update-policy 'synchronize)
		  key-savefun key-loadfun
		  data-savefun data-loadfun
		  plist-savefun plist-loadfun
		  &key
		  createfun insertfun deletefun
		  lookupfun mapfun emptyfun
		  stack-createfun stack-popfun stack-emptyfun
		  transform-for-print transform-from-read
		  &aux
		  (modified nil)
		  (trie (make-trie-custom
			 comparison-function
			 :createfun createfun
			 :insertfun insertfun
			 :deletefun deletefun
			 :lookupfun lookupfun
			 :mapfun mapfun
			 :emptyfun emptyfun
			 :stack-createfun stack-createfun
			 :stack-popfun stack-popfun
			 :stack-emptyfun stack-emptyfun
			 :transform-for-print transform-for-print
			 :transform-from-read transform-from-read))
		  (lookup-cache nil)
		  (complete-cache nil)
		  (regexp-cache nil)
		  (fuzzy-match-cache nil)
		  (fuzzy-complete-cache nil)
		  (meta-dict-list nil)
		  ))
   (:copier dictree--copy))
  name filename autosave modified
  comparison-function insert-function rank-function
  cache-policy cache-threshold cache-update-policy
  lookup-cache complete-cache regexp-cache
  fuzzy-match-cache fuzzy-complete-cache
  key-savefun key-loadfun
  data-savefun data-loadfun
  plist-savefun plist-loadfun
  trie meta-dict-list)


(defstruct
  (dictree--meta-dict
   :named
   (:constructor nil)
   (:constructor dictree--meta-dict-create
		 (dictionary-list
		  &optional
		  filename
		  (name (when filename
			  (file-name-sans-extension
			   (file-name-nondirectory filename))))
		  autosave
		  _unlisted
		  (combine-function #'+)
		  (cache-policy 'time)
		  cache-threshold
		  (cache-update-policy 'synchronize)
		  &aux
		  (dictlist
		   (mapcar
		    (lambda (dic)
		      (cond
		       ((dictree-p dic) dic)
		       ((symbolp dic) (symbol-value dic))
		       (t (error "Invalid object in DICTIONARY-LIST"))))
		    dictionary-list))
		  (lookup-cache nil)
		  (complete-cache nil)
		  (regexp-cache nil)
		  (fuzzy-match-cache nil)
		  (fuzzy-complete-cache nil)
		  ))
   (:copier dictree--meta-dict-copy))
  name filename autosave modified combine-function
  cache-policy cache-threshold cache-update-policy
  lookup-cache complete-cache regexp-cache
  fuzzy-match-cache fuzzy-complete-cache
  dictlist meta-dict-list)




;; ----------------------------------------------------------------
;;           Miscelaneous internal functions and macros

(defun dictree--trielist (dict)
  ;; Return a list of all the tries on which DICT is based. If DICT is a
  ;; meta-dict, this recursively descends the hierarchy, gathering all
  ;; the tries from the base dictionaries.
  (dictree--do-trielist dict))

(defun dictree--do-trielist (dict)
  (if (dictree-meta-dict-p dict)
      (apply #'nconc (mapcar #'dictree--do-trielist
			     (dictree--meta-dict-dictlist dict)))
    (list (dictree--trie dict))))


(defun dictree--merge (list1 list2 cmpfun &optional combfun maxnum)
  ;; Destructively merge together sorted lists LIST1 and LIST2, sorting
  ;; elements according to CMPFUN. For non-null MAXNUM, only the first
  ;; MAXNUM are kept. For non-null COMBFUN, duplicate elements will be
  ;; merged by passing the two elements as arguments to COMBFUN, and
  ;; using the return value as the merged element.
  (or (listp list1) (setq list1 (append list1 nil)))
  (or (listp list2) (setq list2 (append list2 nil)))
  (let (res (i 0))

    ;; build up result list backwards
    (while (and list1 list2 (or (null maxnum) (< (incf i) maxnum)))
      ;; move smaller element to result list
      (if (funcall cmpfun (car list1) (car list2))
	  (push (pop list1) res)
	(if (funcall cmpfun (car list2) (car list1))
	    (push (pop list2) res)
	  ;; if elements are equal, merge them for non-null COMBFUN
	  (if combfun
	      (push (funcall combfun (pop list1) (pop list2))
		    res)
	    ;; otherwise, add both to result list, in order
	    (push (pop list1) res)
	    (push (pop list2) res)))))

    ;; return result if we already have MAXNUM entries
    (if (and maxnum (= i maxnum))
	(nreverse res)
      ;; otherwise, return result plus enough leftover entries to make
      ;; up MAXNUM (only one of list1 or list2 will be non-nil)
      (let (tmp)
	(or (null maxnum)
	    (and (setq tmp (nthcdr (- maxnum i 1) list1))
		 (setcdr tmp nil))
	    (and (setq tmp (nthcdr (- maxnum i 1) list2))
		 (setcdr tmp nil)))
	(nconc (nreverse res) list1 list2)))
    ))


;; (defun dictree--merge-sort (list sortfun &optional combfun)
;;   ;; Destructively sort LIST according to SORTFUN, combining
;;   ;; identical elements using COMBFUN if supplied.
;;   (dictree--do-merge-sort list (/ (length list) 2) sortfun combfun))


;; (defun dictree--do-merge-sort (list1 len sortfun combfun)
;;   ;; Merge sort LIST according to SORTFUN, combining identical
;;   ;; elements using COMBFUN.
;;   (let* ((p (nthcdr (1- len) list1))
;; 	 (list2 (cdr p)))
;;     (setcdr p nil)
;;     (dictree--merge
;;      (dictree--do-merge-sort list1 (/ len 2) sortfun combfun)
;;      (dictree--do-merge-sort list2 (/ len 2) sortfun combfun)
;;      sortfun combfun)))




;;; ================================================================
;;;    The (mostly) public functions which operate on dictionaries

;;;###autoload
(defun make-dictree
  (&optional
   name filename autosave unlisted
   comparison-function insert-function rank-function
   cache-policy cache-threshold cache-update-policy
   key-savefun key-loadfun
   data-savefun data-loadfun
   plist-savefun plist-loadfun
   trie-type)
  "Create an empty dictionary and return it.

If NAME is supplied, the dictionary is stored in the variable
NAME. Defaults to FILENAME stripped of directory and
extension. (Regardless of the value of NAME, the dictionary will
be stored in the default variable name when it is reloaded from
file.)

FILENAME supplies a directory and file name to use when saving
the dictionary. If the AUTOSAVE flag is non-nil, then the
dictionary will automatically be saved to this file when it is
unloaded or when exiting Emacs.

If UNLISTED is non-nil, the dictionary will not be added to the
list of loaded dictionaries. Note that this disables autosaving.

COMPARE-FUNCTION sets the function used to compare elements of
the keys. It should take two arguments, A and B, both of the type
contained by the sequences used as keys \(e.g. if the keys will
be strings, the function will be passed two characters\). It
should return t if the first is \"less than\" the
second. Defaults to `<'.

INSERT-FUNCTION sets the function used to insert data into the
dictionary. It should take two arguments: the new data, and the
data already in the dictionary, and should return the data to
insert. Defaults to replacing any existing data with the new
data.

RANK-FUNCTION sets the function used to rank the results of
`dictree-complete'. It should take two arguments, each a cons
whose car is a dictree key (a sequence) and whose cdr is the data
associated with that key. It should return non-nil if the first
argument is \"better\" than the second, nil otherwise. It
defaults to \"lexicographic\" comparison of the keys, ignoring
the data \(which is not very useful, since an unranked
`dictree-complete' query already does this much more
efficiently\).

CACHE-POLICY should be a symbol (`time', `length',
`time-and-length' or `time-or-length'), which determines which
query operations are cached. The `time' setting caches queries
that take longer (in seconds) than the CACHE-THRESHOLD value.

The `length' setting caches query operations based on the length
of the string involved the query. For this setting, CACHE-POLICY
should be a plist with properties :long and :short. Lookups,
fuzzy matches, and regexp queries that do not end in \".*\" will
be cached if the string is longer than the :long value (since
long strings are likely to be the slower ones in these
cases). Completions, fuzzy completions, and regexp queries that
end in \".*\" will be cached if the string or regexp is shorter
than the :short value \(since short strings are likely to be the
slower ones for those cases\).

The `time-and-length' setting only caches results if both
conditions are satisfied simultaneously, whereas the
`time-or-length' setting caches results if either condition is
satisfied. For these settings, CACHE-THRESHOLD must be a plist
with properties :time, :long and :short, specifying the
corresponding cache thresholds.

CACHE-THRESHOLD defaults to nil. The values nil and t are
special. If CACHE-THRESHOLD is set to nil, no caching is done. If
it is t, everything is cached for that type of query \(similar
behaviour can be obtained by setting the a `time' CACHE-THRESHOLD
of 0, but it is better to use t\).

CACHE-UPDATE-POLICY should be a symbol (`synchronize' or
`delete'), which determines how the caches are updated when data
is inserted or deleted. The former updates tainted cache entries,
which makes queries faster but insertion and deletion slower,
whereas the latter deletes any tainted cache entries, which makes
queries slower but insertion and deletion faster.

KEY-SAVEFUN, DATA-SAVEFUN and PLIST-SAVEFUN are functions used to
convert keys, data and property lists into lisp objects that have
a valid read syntax, for writing to file. DATA-SAVEFUN and
PLIST-SAVEFUN are used when saving the dictionary (see
`dictree-save' and `dictree-write'), and all three functions are
used when dumping the contents of the dictionary \(see
`dictree-dump-to-buffer' and `dictree-dump-to-file'\).
KEY-SAVEFUN, DATA-SAVEFUN and PLIST-SAVEFUN should each accept
one argument: a key, data or property list from DICT,
respectively. They should return a lisp object which has a valid
read syntax. When defining these functions, be careful not to
accidentally modify the lisp object in the dictionary; usually,
you will need to make a copy before converting it.

KEY-LOADFUN, DATA-LOADFUN and PLIST-LOADFUN are used to convert
keys, data and property lists back again when loading a
dictionary (only DATA-LOADFUN and PLIST-LOADFUN, see
`dictree-save' and `dictree-write') or populating it from a
file (all three, see `dictree-populate-from-file'). They should
accept one argument: a lisp object of the type produced by the
corresponding SAVEFUN, and return a lisp object to use in the
loaded dictionary.

TRIE-TYPE sets the type of trie to use as the underlying data
structure. See `trie-create' for details."

  ;; sadly, passing null values overrides the defaults in the defstruct
  ;; dictree--create, so we have to explicitly set the defaults again here
  (or name (setq name (and filename (make-symbol
				     (file-name-sans-extension
				     (file-name-nondirectory filename))))))
  (or comparison-function (setq comparison-function '<))
  (or insert-function (setq insert-function (lambda (a _b) a)))
  (or rank-function (setq rank-function (lambda (a b) (> (cdr a) (cdr b)))))
  (or cache-policy (setq cache-policy 'time))
  (or cache-update-policy (setq cache-update-policy 'synchronize))
  (or trie-type (setq trie-type 'avl))

  (let ((dict
	 (dictree--create
	  filename (when name (symbol-name name)) autosave unlisted
	  comparison-function insert-function rank-function
	  cache-policy cache-threshold cache-update-policy
	  key-savefun key-loadfun
	  data-savefun data-loadfun
	  plist-savefun plist-loadfun
	  trie-type)))
    ;; store dictionary in variable NAME
    (when name (set name dict))
    ;; add it to loaded dictionary list, unless it's unlisted
    (unless (or (null name) unlisted)
      (push dict dictree-loaded-list))
    dict))


;;;###autoload
(defalias 'dictree-create 'make-dictree)


;;;###autoload
(defun* make-dictree-custom
    (&optional
     name filename autosave unlisted
     &key
     comparison-function insert-function rank-function
     cache-policy cache-threshold cache-update-policy
     key-savefun key-loadfun
     data-savefun data-loadfun
     plist-savefun plist-loadfun
     createfun insertfun deletefun lookupfun mapfun emptyfun
     stack-createfun stack-popfun stack-emptyfun
     transform-for-print transform-from-read)
  "Create an empty dictionary and return it.

The NAME through PLIST-LOADFUN arguments are as for
`dictree-create' (which see).

The remaining arguments control the type of trie to use as the
underlying data structure. See `trie-create' for details."

  ;; sadly, passing null values over-rides the defaults in the defstruct
  ;; dictree--create, so we have to explicitly set the defaults again
  ;; here
  (or name (setq name (and filename (file-name-sans-extension
				     (file-name-nondirectory filename)))))
  (or comparison-function (setq comparison-function #'<))
  (or insert-function (setq insert-function (lambda (a _b) a)))
  (or rank-function (setq rank-function (lambda (a b) (< (cdr a) (cdr b)))))
  (or cache-policy (setq cache-policy 'time))
  (or cache-update-policy (setq cache-update-policy 'synchronize))

  (let ((dict
	 (dictree--create-custom
	  filename (when name (symbol-name name)) autosave unlisted
	  comparison-function insert-function rank-function
	  cache-policy cache-threshold cache-update-policy
	  key-savefun key-loadfun
	  data-savefun data-loadfun
	  plist-savefun plist-loadfun
	  :createfun createfun
	  :insertfun insertfun
	  :deletefun deletefun
	  :lookupfun lookupfun
	  :mapfun mapfun
	  :emptyfun emptyfun
	  :stack-createfun stack-createfun
	  :stack-popfun stack-popfun
	  :stack-emptyfun stack-emptyfun
	  :transform-for-print transform-for-print
	  :transform-from-read transform-from-read)))
    ;; store dictionary in variable NAME
    (when name (set name dict))
    ;; add it to loaded dictionary list, unless it's unlisted
    (unless (or (null name) unlisted)
      (push dict dictree-loaded-list))
    dict))


;;;###autoload
(defalias 'dictree-create-custom 'make-dictree-custom)


;;;###autoload
(defun make-dictree-meta-dict
  (dictionary-list
   &optional
   name filename autosave unlisted
   combine-function
   cache-policy cache-threshold cache-update-policy)
  "Create a meta-dictionary based on the list of dictionaries
in DICTIONARY-LIST.

COMBINE-FUNCTION is used to combine data from different
dictionaries. It is passed two pieces of data, each an
association of the same key, but in different dictionaries. It
should return a combined datum.

The other arguments are as for `dictree-create'. Note that
caching is only possible if NAME is supplied, otherwise the
CACHE-THRESHOLD argument is ignored and caching is disabled."

  ;; sadly, passing null values over-rides the defaults in the defstruct
  ;; `dictree--create', so we have to explicitly set the defaults again
  ;; here
  (or name (setq name (and filename
			   (file-name-sans-extension
			    (file-name-nondirectory filename)))))
  (or combine-function (setq combine-function #'+))
  (or cache-policy (setq cache-policy 'time))
  (or cache-update-policy (setq cache-update-policy 'synchronize))

  (let ((dict
	 (dictree--meta-dict-create
	  dictionary-list filename (when name (symbol-name name))
	  autosave unlisted
	  combine-function
	  cache-policy (when name cache-threshold) cache-update-policy
	 )))
    ;; store dictionary in variable NAME
    (when name (set name dict))
    ;; add it to loaded dictionary list, unless it's unlisted
    (unless (or (null name) unlisted)
      (push dict dictree-loaded-list))
    ;; update meta-dict-list cells of constituent dictionaries
    (unless (or (null name) (not cache-threshold))
      (mapc
       (lambda (dic)
	 (if (symbolp dic) (setq dic (symbol-value dic)))
	 (setf (dictree--meta-dict-list dic)
	       (cons dict (dictree--meta-dict-list dic))))
       dictionary-list))
    dict))

(defalias 'dictree-create-meta-dict 'make-dictree-meta-dict)


;;;###autoload
(defun dictree-p (obj)
  "Return t if OBJ is a dictionary tree, nil otherwise."
  (or (dictree--p obj) (dictree--meta-dict-p obj)))


(defalias 'dictree-meta-dict-p 'dictree--meta-dict-p
  "Return t if argument is a meta-dictionary, nil otherwise.")

(defun dictree-empty-p (dict)
  "Return t if the dictionary DICT is empty, nil otherwise."
  (if (dictree--meta-dict-p dict)
      (catch 'nonempty
	(mapc (lambda (dic)
		(if (not (dictree-empty-p dic)) (throw 'nonempty t)))
	      (dictree--meta-dict-dictlist dict)))
    (trie-empty (dictree--trie dict))))

(defsubst dictree-autosave (dict)
  "Return dictionary's autosave flag."
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-autosave dict)
    (dictree--autosave dict)))

(defsetf dictree-autosave (dict) (val)
  ;; setf method for dictionary autosave flag
  `(if (dictree--meta-dict-p ,dict)
       (setf (dictree--meta-dict-autosave ,dict) ,val)
     (setf (dictree--autosave ,dict) ,val)))

(defsubst dictree-modified (dict)
  "Return dictionary's modified flag."
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-modified dict)
    (dictree--modified dict)))

(defsetf dictree-modified (dict) (val)
  ;; setf method for dictionary modified flag
  `(if (dictree--meta-dict-p ,dict)
       (setf (dictree--meta-dict-modified ,dict) ,val)
     (setf (dictree--modified ,dict) ,val)))

(defsubst dictree-name (dict)
  "Return dictionary DICT's name."
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-name dict)
    (dictree--name dict)))

(defsetf dictree-name (dict) (name)
  ;; setf method for dictionary name
  `(if (dictree--meta-dict-p ,dict)
       (setf (dictree--meta-dict-name ,dict) ,name)
    (setf (dictree--name ,dict) ,name)))

(defsubst dictree-filename (dict)
  "Return dictionary DICT's associated file name."
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-filename dict)
    (dictree--filename dict)))

(defsetf dictree-filename (dict) (filename)
  ;; setf method for dictionary filename
  `(if (dictree--meta-dict-p ,dict)
       (setf (dictree--meta-dict-filename ,dict) ,filename)
     (setf (dictree--filename ,dict) ,filename)))

(defun dictree-comparison-function (dict)
  "Return dictionary DICT's comparison function."
  (if (dictree--meta-dict-p dict)
      (dictree-comparison-function
       (car (dictree--meta-dict-dictlist dict)))
    (dictree--comparison-function dict)))

(defalias 'dictree-insert-function 'dictree--insert-function
  "Return the insertion function for dictionary DICT.")

(defun dictree-rank-function (dict)
  "Return the rank function for dictionary DICT"
  (if (dictree--meta-dict-p dict)
      (dictree-rank-function (car (dictree--meta-dict-dictlist dict)))
    (dictree--rank-function dict)))

(defalias 'dictree-meta-dict-combine-function
  'dictree--meta-dict-combine-function
  "Return the combine function for meta-dictionary DICT.")

(defalias 'dictree-meta-dict-dictlist
  'dictree--meta-dict-dictlist
  "Return the list of constituent dictionaries
for meta-dictionary DICT.")

(defsubst dictree-cache-policy (dict)
  "Return the cache policy for dictionary DICT."
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-cache-policy dict)
    (dictree--cache-policy dict)))

(defsubst dictree-cache-update-policy (dict)
  "Return the cache update policy for dictionary DICT."
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-cache-update-policy dict)
    (dictree--cache-update-policy dict)))

(defsubst dictree-cache-threshold (dict)
  "Return the cache threshold for dictionary DICT."
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-cache-threshold dict)
    (dictree--cache-threshold dict)))

(defsetf dictree-cache-threshold (dict) (param)
  ;; setf method for cache threshold
  `(if (dictree--meta-dict-p ,dict)
       (setf (dictree--meta-dict-cache-threshold ,dict)
	     ,param)
     (setf (dictree--cache-threshold ,dict)
	   ,param)))


(defun dictree-lookup-cache (dict)
  ;; Return the lookup cache for dictionary DICT.
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-lookup-cache dict)
    (dictree--lookup-cache dict)))

(defsetf dictree-lookup-cache (dict) (param)
  ;; setf method for lookup cache
  `(if (dictree--meta-dict-p ,dict)
       (setf (dictree--meta-dict-lookup-cache ,dict)
	     ,param)
     (setf (dictree--lookup-cache ,dict)
	   ,param)))

(defun dictree-create-lookup-cache (dict)
  ;; Create DICT's lookup cache if it doesn't already exist.
  (unless (dictree-lookup-cache dict)
    (setf (dictree-lookup-cache dict)
	  (make-hash-table :test 'equal))))


(defun dictree-complete-cache (dict)
  ;; Return the completion cache for dictionary DICT.
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-complete-cache dict)
    (dictree--complete-cache dict)))

(defsetf dictree-complete-cache (dict) (param)
  ;; setf method for complete cache
  `(if (dictree--meta-dict-p ,dict)
       (setf (dictree--meta-dict-complete-cache ,dict)
	     ,param)
     (setf (dictree--complete-cache ,dict)
	   ,param)))

(defun dictree-create-complete-cache (dict)
  ;; Create DICT's completion cache if it doesn't already exist.
  (unless (dictree-complete-cache dict)
    (setf (dictree-complete-cache dict)
	  (make-hash-table :test 'equal))))


(defun dictree-regexp-cache (dict)
  ;; Return the regexp cache for dictionary DICT.
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-regexp-cache dict)
    (dictree--regexp-cache dict)))

(defsetf dictree-regexp-cache (dict) (param)
  ;; setf method for regexp cache
  `(if (dictree--meta-dict-p ,dict)
       (setf (dictree--meta-dict-regexp-cache ,dict)
	     ,param)
     (setf (dictree--regexp-cache ,dict)
	   ,param)))

(defun dictree-create-regexp-cache (dict)
  ;; Create DICT's regexp cache if it doesn't already exist.
  (unless (dictree-regexp-cache dict)
    (setf (dictree-regexp-cache dict)
	  (make-hash-table :test 'equal))))


(defun dictree-fuzzy-match-cache (dict)
  ;; Return the fuzzy match cache for dictionary DICT.
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-fuzzy-match-cache dict)
    (dictree--fuzzy-match-cache dict)))

(defsetf dictree-fuzzy-match-cache (dict) (param)
  ;; setf method for fuzzy match cache
  `(if (dictree--meta-dict-p ,dict)
       (setf (dictree--meta-dict-fuzzy-match-cache ,dict)
	     ,param)
     (setf (dictree--fuzzy-match-cache ,dict)
	   ,param)))

(defun dictree-create-fuzzy-match-cache (dict)
  ;; Create DICT's fuzzy match cache if it doesn't already exist.
  (unless (dictree-fuzzy-match-cache dict)
    (setf (dictree-fuzzy-match-cache dict)
	  (make-hash-table :test 'equal))))


(defun dictree-fuzzy-complete-cache (dict)
  ;; Return the regexp cache for dictionary DICT.
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-fuzzy-complete-cache dict)
    (dictree--fuzzy-complete-cache dict)))

(defsetf dictree-fuzzy-complete-cache (dict) (param)
  ;; setf method for fuzzy completion cache
  `(if (dictree--meta-dict-p ,dict)
       (setf (dictree--meta-dict-fuzzy-complete-cache ,dict)
	     ,param)
     (setf (dictree--fuzzy-complete-cache ,dict)
	   ,param)))

(defun dictree-create-fuzzy-complete-cache (dict)
  ;; Create DICT's fuzzy completion cache if it doesn't already exist.
  (unless (dictree-fuzzy-complete-cache dict)
    (setf (dictree-fuzzy-complete-cache dict)
	  (make-hash-table :test 'equal))))





;; ----------------------------------------------------------------
;;                  Inserting and deleting data

(defun dictree-insert (dict key &optional data insert-function)
  "Insert KEY and DATA into dictionary DICT.
If KEY does not already exist, this creates it. How the data is
inserted depends on the dictionary's insertion function \(see
`dictree-create'\).

The optional INSERT-FUNCTION over-rides the dictionary's own
insertion function. If KEY already exists in DICT,
INSERT-FUNCTION is called with two arguments: the data DATA, and
the data associated with KEY in the dictionary. Its return value
becomes the new association for KEY."

  ;; if dictionary is a meta-dictionary, insert key into all the
  ;; dictionaries it's based on
  (if (dictree--meta-dict-p dict)
      (mapc (lambda (dic)
	      (dictree-insert dic key data insert-function))
	    (dictree--meta-dict-dictlist dict))

    ;; otherwise...
    (let ((insfun (or (and insert-function
			   (dictree--wrap-insfun insert-function))
		      (dictree--wrap-insfun (dictree--insert-function dict))))
	  olddata newdata)
      ;; set the dictionary's modified flag
      (setf (dictree-modified dict) t)
      ;; insert key in dictionary's ternary search tree
      (setq newdata
	    (trie-insert
	     (dictree--trie dict) key (dictree--cell-create data nil)
	     (lambda (nd od)
	       (setq olddata od)
	       (funcall insfun nd od))))
      ;; update dictionary's caches
      (dictree--update-cache dict key olddata newdata)
      ;; update cache's of any meta-dictionaries based on dict
      (mapc (lambda (dic) (dictree--update-cache dic key olddata newdata))
	    (dictree--meta-dict-list dict))

      ;; return the new data
      (dictree--cell-data newdata))))



(defun dictree-delete (dict key &optional test)
  "Delete KEY from DICT.
Returns non-nil if KEY was deleted, nil if KEY was not in DICT.

If TEST is supplied, it should be a function that accepts three
arguments: the key being deleted, its associated data, and its
associated property list. The key will then only be deleted if
TEST returns non-nil."

  (let ((dictree--delete-test test)
	olddata deleted del)
    (cond
     ;; if DICT is a meta-dictionary, delete KEY from all dictionaries
     ;; it's based on
     ((dictree--meta-dict-p dict)
      (dolist (dic (dictree--meta-dict-dictlist dict))
	(when (setq del (dictree-delete dic key))
	  (setq deleted (cons del deleted))))
      (setf (dictree-modified dict) (and deleted t))
      (setq deleted (nreverse deleted)))

     ;; otherwise...
     (t
      (setq deleted
	    (trie-delete (dictree--trie dict) key
			 (lambda (k cell)
			   (setq olddata (dictree--cell-data cell))
			   (if dictree--delete-test
			       (funcall dictree--delete-test
					k (dictree--cell-data cell)
					(dictree--cell-plist cell))
			     t))))
      ;; if key was deleted, have to update the caches
      (when deleted
	(dictree--update-cache dict key olddata nil t)
	(setf (dictree-modified dict) t)
	;; update cache's of any meta-dictionaries based on DICT
	(mapc (lambda (dic)
		(dictree--update-cache dic key olddata nil t))
	      (dictree--meta-dict-list dict)))))

    ;; return deleted key/data pair
    (when deleted
      (cons (car deleted) (dictree--cell-data (cdr deleted))))))




;; ----------------------------------------------------------------
;;                     Cache updating

(defun dictree--prefix-p (prefix str)
  "Return t if PREFIX is a prefix of STR, nil otherwise.

PREFIX and STR can be any sequence type (string, vector, or
list), but they must both be the same type. PREFIX can also be a
list of sequences, in which case it returns t if any element of
PREFIX is a prefix of STR."
  ;; wrap prefix in a list if necessary
  ;; FIXME: the test for a list of prefixes, below, will fail if the
  ;;        PREFIX sequence is a list, and the elements of PREFIX are
  ;;        themselves lists (there might be no easy way to fully fix
  ;;        this...)
  (when (or (atom prefix)
	    (and (listp prefix) (not (sequencep (car prefix)))))
    (setq prefix (list prefix)))
  (let (len)
    (catch 'is-prefix
      (dolist (pfx prefix)
	(setq len (length pfx))
	(when (and (<= len (length str))
		   (equal pfx (dictree--subseq str 0 len)))
	  (throw 'is-prefix t))))))


(defun dictree--above-cache-threshold-p
  (time length policy threshold &optional cache-long-keys)
  ;; Return t if query taking TIME seconds for a key of length LENGTH
  ;; should be cached according to the cache POLICY and
  ;; THRESHOLD. Otherwise, return nil. Optional argument CACHE-LONG-KEYS
  ;; means that keys of length longer than THRESHOLD are to be
  ;; cached. Default is keys of length shorter than THRESHOLD.
  (and threshold
       (or (eq threshold t)
	   (and (eq policy 'time) (>= time threshold))
	   (and (eq policy 'length)
		(if cache-long-keys
		    (>= length (plist-get threshold :long))
		  (<= length (plist-get threshold :short))))
	   (and (eq policy 'time-and-length)
		(>= time (plist-get threshold :time))
		(if cache-long-keys
		    (>= length (plist-get threshold :long))
		  (<= length (plist-get threshold :short))))
	   (and (eq policy 'time-or-length)
		(or (>= time (plist-get threshold :time))
		    (if cache-long-keys
			(>= length (plist-get threshold :long))
		      (<= length (plist-get threshold :short))))))))



(defun dictree--update-cache (dict key olddata newdata &optional deleted)
  ;; Synchronise dictionary DICT's caches, given that the data associated with
  ;; KEY has been updated from OLDDATA to NEWDATA, or KEY has been deleted if
  ;; DELETED is non-nil (NEWDATA is ignored in that case)."
  (when (dictree-cache-threshold dict)

    ;; synchronise lookup cache if dict is a meta-dictionary, since it doesn't
    ;; happen automatically for a meta-dict
    (when (dictree--meta-dict-p dict)
      (cond
       ;; updating dirty cache entries
       ((eq (dictree-cache-update-policy dict) 'synchronize)
	(when (and (dictree--lookup-cache dict)
		   (gethash key (dictree--lookup-cache dict)))
	  (if deleted
	      (remhash key (dictree--lookup-cache dict))
	    (puthash key newdata (dictree--lookup-cache dict)))))
       ;; deleting dirty cache entries
       (t (remhash key (dictree--lookup-cache dict)))))

    ;; synchronize query caches if something's actually changed
    (when (or deleted (not (equal olddata newdata)))
      (dolist (cachefuns
	       '((dictree-complete-cache
		  dictree--synchronize-completion-cache
		  dictree--prefix-p)
		 (dictree-regexp-cache
		  dictree--synchronize-regexp-cache
		  (lambda (arg key)
		    (tNFA-regexp-match
		     arg key :test (trie--construct-equality-function
				    (dictree--comparison-function dict)))))
		 (dictree-fuzzy-match-cache
		  dictree--synchronize-fuzzy-match-cache
		  (lambda (string dist key)
		    (<= (Lewenstein-distance string key) dist)))
		 (dictree-fuzzy-complete-cache
		  dictree--synchronize-fuzzy-completion-cache
		  (lambda (prefix dist key)
		    (<= (Lewenstein-distance prefix key) dist)))
		 ))
	(when (funcall (nth 0 cachefuns) dict)
	  (maphash
	   (lambda (cache-key cache-entry)
	     (destructuring-bind
		 (arg auxargs rank-function reverse filter) cache-key
	       (when (apply (nth 2 cachefuns)
			    (append (list arg) auxargs (list key)))
		 (cond
		  ;; updating dirty cache entries
		  ((eq (dictree-cache-update-policy dict) 'synchronize)
		   (funcall (nth 1 cachefuns)
			    dict key olddata newdata deleted cache-entry
			    arg auxargs rank-function reverse filter))
		  ;; deleting dirty cache entries
		  (t (remhash (list arg auxargs rank-function reverse filter)
			      (funcall (nth 0 cachefuns) dict)))))))
	   (funcall (nth 0 cachefuns) dict)))
	))))



(defun dictree--synchronize-completion-cache
    (dict key olddata newdata deleted cache-entry
	  arg auxargs rank-function reverse filter)
  ;; Synchronize DICT's completion CACHE-ENTRY for a query with arguments ARG,
  ;; AUXARGS, RANK-FUNCTION, REVERSE and FILTER, where KEY's data was either
  ;; updated from OLDDATA to NEWDATA or DELETED,

  (let* ((completions (dictree--cache-results cache-entry))
	 (maxnum (dictree--cache-maxnum cache-entry))
	 (cmpl (assoc key completions))
	 (rankfun (cond ((eq rank-function t)
			 (dictree--wrap-rankfun
			  (dictree--rank-function dict)))
			(rank-function
			 (dictree--wrap-rankfun rank-function)))))
    ;; for meta-dict, get old data from cache instead of OLDDATA
    (when (dictree--meta-dict-p dict) (setq olddata (cdr cmpl)))
    ;; skip cache update if key/data pair doesn't pass FILTER
    (when (or (null filter)
	      (funcall filter key olddata)
	      (funcall filter key newdata))
      ;; if key was...
      (cond

       ;; deleted and in cached result: remove cache entry and re-run the
       ;; same completion to update the cache
       ((and deleted cmpl)
	(remhash (list arg auxargs rank-function reverse filter)
		 (dictree-complete-cache dict))
	(dictree-complete dict arg rank-function maxnum reverse filter))

       ;; modified and not in cached result: merge it into the completion
       ;; list, retaining only the first maxnum
       ((and (not deleted) (not cmpl))
	(when (or (null filter) (funcall filter key newdata))
	  (setf (dictree--cache-results cache-entry)
		(dictree--merge
		 (list (cons key newdata)) completions
		 (or rankfun
		     `(lambda (a b)
			(,(trie-construct-sortfun
			   (dictree-comparison-function dict))
			 (car a) (car b))))
		 (when (dictree--meta-dict-p dict)
		   (dictree--wrap-combfun
		    (dictree--meta-dict-combine-function dict)))
		 maxnum))))

       ;; modified and in the cached result
       ((and (not deleted) cmpl)
	;; update the associated data if dict is a meta-dictionary (this
	;; happens automatically for a normal dict)
	(when (dictree--meta-dict-p dict) (setcdr cmpl newdata))
	;; if updated entry gets filtered, or gets sorted at end of list,
	;; re-run the same query to update the cache
	(when (or (and filter (not (funcall filter key newdata)))
		  (and rankfun
		       (setf (dictree--cache-results cache-entry)
			     (sort completions rankfun))
		       (equal key (car (last (dictree--cache-results
					      cache-entry))))))
	  (remhash (list arg auxargs rank-function reverse filter)
		   (dictree-complete-cache dict))
	  (dictree-complete dict arg rank-function maxnum reverse filter)))

       ;; deleted and not in cached result: requires no action
       ))))


(defun dictree--synchronize-regexp-cache
    (dict key olddata newdata deleted cache-entry
	  arg auxargs rank-function reverse filter)
  ;; Synchronize DICT's regexp CACHE-ENTRY for a query with arguments ARG,
  ;; AUXARGS, RANK-FUNCTION, REVERSE and FILTER, where KEY's data was either
  ;; updated from OLDDATA to NEWDATA or DELETED,

  (let* ((completions (dictree--cache-results cache-entry))
	 (maxnum (dictree--cache-maxnum cache-entry))
	 group-data
	 (cmpl (catch 'found
		 (dolist (c completions)
		   (if (and (listp (car c))
			    (or (stringp (caar c))
				(vectorp (caar c))
				(listp (caar c))))
		       (when (equal key (caar c)) (throw 'found c))
		     (when (equal key (car c)) (throw 'found c))))))
	 (rankfun (cond ((eq rank-function t)
			 (dictree--wrap-regexp-rankfun
			  (dictree-rank-function dict)))
			(rank-function
			 (dictree--wrap-regexp-rankfun rank-function)))))
    ;; for meta-dict, get old data from cache instead of OLDDATA
    (when (dictree--meta-dict-p dict) (setq olddata (cdr cmpl)))
    ;; skip cache update if key/data pair doesn't pass FILTER
    (when (or (null filter)
	      (funcall filter key olddata)
	      (funcall filter key newdata))
      ;; if key was...
      (cond

       ;; deleted and in cached result: remove cache entry and re-run the
       ;; same completion to update the cache
       ((and deleted cmpl)
	(remhash (list arg auxargs rank-function reverse filter)
		 (dictree-regexp-cache dict))
	(dictree-regexp-search dict arg rank-function maxnum reverse filter))

       ;; modified and not in cached result: merge it into the completion
       ;; list, retaining only the first maxnum
       ((and (not deleted) (not cmpl))
	(when (or (null filter) (funcall filter key newdata))
	  (save-match-data
	    (set-match-data nil)
	    (tNFA-regexp-match arg key
			       :test (trie--construct-equality-function
				      (dictree--comparison-function dict)))
	    (when (setq group-data (nthcdr 2 (match-data)))
	      (setq key (cons key group-data))))
	  (setf (dictree--cache-results cache-entry)
		(dictree--merge
		 (list (cons key newdata)) completions
		 (or rankfun
		     `(lambda (a b)
			(,(trie-construct-sortfun
			   (dictree-comparison-function dict))
			 ,(if group-data '(caar a) '(car a))
			 ,(if group-data '(caar b) '(car b)))))
		 (when (dictree--meta-dict-p dict)
		   (dictree--wrap-combfun
		    (dictree--meta-dict-combine-function dict)))
		 maxnum))))

       ;; modified and in the cached result
       ((and (not deleted) cmpl)
	;; update the associated data if dict is a meta-dictionary (this
	;; happens automatically for a normal dict)
	(when (dictree--meta-dict-p dict) (setcdr cmpl newdata))
	;; if updated entry gets filtered, or gets sorted at end of list,
	;; re-run the same query to update the cache
	(when (or (and filter (not (funcall filter key newdata)))
		  (and rankfun
		       (setf (dictree--cache-results cache-entry)
			     (sort completions rankfun))
		       (equal key (car (last (dictree--cache-results
					      cache-entry))))))
	  (remhash (list arg auxargs rank-function reverse filter)
		   (dictree-regexp-cache dict))
	  (dictree-regexp-search dict arg rank-function maxnum reverse filter)
	  ))

       ;; deleted and not in cached result: requires no action
       ))))


(defun dictree--synchronize-fuzzy-match-cache
    (dict key olddata newdata deleted cache-entry
	  arg auxargs rank-function reverse filter)
  ;; Synchronize DICT's fuzzy match CACHE-ENTRY for a query with arguments
  ;; ARG, AUXARGS, RANK-FUNCTION, REVERSE and FILTER, where KEY's data was
  ;; either updated from OLDDATA to NEWDATA or DELETED,

  (let* ((completions (dictree--cache-results cache-entry))
	 (maxnum (dictree--cache-maxnum cache-entry))
	 (cmpl (catch 'found
		 (dolist (c completions)
		   (when (equal key (caar c)) (throw 'found c)))))
	 (distance (Lewenstein-distance key arg))
	 (rankfun (cond ((eq rank-function t)
			 (dictree--wrap-fuzzy-rankfun
			  (dictree-rank-function dict)))
			((eq rank-function 'distance)
			 (dictree--wrap-fuzzy-rankfun
			  (trie--construct-Lewenstein-rankfun
			   (dictree-comparison-function dict))))
			(rank-function
			 (dictree--wrap-fuzzy-rankfun rank-function)))))
    ;; for meta-dict, get old data from cache instead of OLDDATA
    (when (dictree--meta-dict-p dict) (setq olddata (cdr cmpl)))
    ;; skip cache update if key/data pair doesn't pass FILTER
    (when (or (null filter)
	      (funcall filter key olddata)
	      (funcall filter key newdata))
      ;; if key was...
      (cond

       ;; deleted and in cached result: remove cache entry and re-run the
       ;; same completion to update the cache
       ((and deleted cmpl)
	(remhash (list arg auxargs rank-function reverse filter)
		 (dictree-fuzzy-match-cache dict))
	(dictree-fuzzy-match dict arg (car auxargs)
			     rank-function maxnum reverse filter))

       ;; modified and not in cached result: merge it into the completion
       ;; list, retaining only the first maxnum
       ((and (not deleted) (not cmpl))
	(when (or (null filter) (funcall filter key newdata))
	  (setf (dictree--cache-results cache-entry)
		(dictree--merge
		 (list (cons (cons key distance) newdata)) completions
		 (or rankfun
		     `(lambda (a b)
			(,(trie-construct-sortfun
			   (dictree-comparison-function dict))
			 (caar a) (caar b))))
		 (when (dictree--meta-dict-p dict)
		   (dictree--wrap-combfun
		    (dictree--meta-dict-combine-function dict)))
		 maxnum))))

       ;; modified and in the cached result
       ((and (not deleted) cmpl)
	;; update the associated data if dict is a meta-dictionary (this
	;; happens automatically for a normal dict)
	(when (dictree--meta-dict-p dict) (setcdr cmpl newdata))
	;; if updated entry gets filtered, or gets sorted at end of list,
	;; re-run the same query to update the cache
	(when (or (and filter (not (funcall filter key newdata)))
		  (and rankfun
		       (setf (dictree--cache-results cache-entry)
			     (sort completions rankfun))
		       (equal key (car (last (dictree--cache-results
					      cache-entry))))))
	  (remhash (list arg auxargs rank-function reverse filter)
		   (dictree-fuzzy-match-cache dict))
	  (dictree-fuzzy-match dict arg (car auxargs)
			       rank-function maxnum reverse filter)))

       ;; deleted and not in cached result: requires no action
       ))))


(defun dictree--synchronize-fuzzy-complete-cache
    (dict key olddata newdata deleted cache-entry
	  arg auxargs rank-function reverse filter)
  ;; Synchronize DICT's fuzzy completion CACHE-ENTRY for a query with
  ;; arguments ARG, AUXARGS, RANK-FUNCTION, REVERSE and FILTER, where KEY's
  ;; data was either updated from OLDDATA to NEWDATA or DELETED,

  (let* ((completions (dictree--cache-results cache-entry))
	 (maxnum (dictree--cache-maxnum cache-entry))
	 (cmpl (catch 'found
		 (dolist (c completions)
		   (when (equal key (caar c)) (throw 'found c)))))
	 (distance (Lewenstein-distance key arg))
	 (rankfun (cond ((eq rank-function t)
			 (dictree--wrap-fuzzy-rankfun
			  (dictree-rank-function dict)))
			((eq rank-function 'distance)
			 (dictree--wrap-fuzzy-rankfun
			  (trie--construct-Lewenstein-rankfun
			   (dictree-comparison-function dict))))
			(rank-function
			 (dictree--wrap-fuzzy-rankfun rank-function)))))
    ;; for meta-dict, get old data from cache instead of OLDDATA
    (when (dictree--meta-dict-p dict) (setq olddata (cdr cmpl)))
    ;; skip cache update if key/data pair doesn't pass FILTER
    (when (or (null filter)
	      (funcall filter key olddata)
	      (funcall filter key newdata))
      ;; if key was...
      (cond

       ;; deleted and in cached result: remove cache entry and re-run the
       ;; same completion to update the cache
       ((and deleted cmpl)
	(remhash (list arg auxargs rank-function reverse filter)
		 (dictree-fuzzy-complete-cache dict))
	(dictree-fuzzy-complete dict arg (car auxargs)
				rank-function maxnum reverse filter))

       ;; modified and not in cached result: merge it into the completion
       ;; list, retaining only the first maxnum
       ((and (not deleted) (not cmpl))
	(when (or (null filter) (funcall filter key newdata))
	  (setf (dictree--cache-results cache-entry)
		(dictree--merge
		 (list (cons key (cons distance newdata))) completions
		 (or rankfun
		     `(lambda (a b)
			(,(trie-construct-sortfun
			   (dictree-comparison-function dict))
			 (car a) (car b))))
		 (when (dictree--meta-dict-p dict)
		   (dictree--wrap-combfun
		    (dictree--meta-dict-combine-function dict)))
		 maxnum))))

       ;; modified and in the cached result
       ((and (not deleted) cmpl)
	;; update the associated data if dict is a meta-dictionary (this
	;; happens automatically for a normal dict)
	(when (dictree--meta-dict-p dict) (setcdr cmpl newdata))
	;; if updated entry gets filtered, or gets sorted at end of list,
	;; re-run the same query to update the cache
	(when (or (and filter (not (funcall filter key newdata)))
		  (and rankfun
		       (setf (dictree--cache-results cache-entry)
			     (sort completions rankfun))
		       (equal key (car (last (dictree--cache-results
					      cache-entry))))))
	  (remhash (list arg auxargs rank-function reverse filter)
		   (dictree-fuzzy-complete-cache dict))
	  (dictree-fuzzy-complete dict arg (car auxargs)
				  rank-function maxnum reverse filter)))

       ;; deleted and not in cached result: requires no action
       ))))


(defun dictree-clear-caches (dict)
  "Clear all DICT's query caches."
  (interactive (list (read-dict "Dictionary: ")))
  (when (and (called-interactively-p 'any) (symbolp dict))
    (setq dict (symbol-value dict)))
  (dolist (cachefun '(dictree-lookup-cache
		      dictree-complete-cache
		      dictree-regexp-cache
		      dictree-fuzzy-match-cache
		      dictree-fuzzy-complete-cache))
    (when (funcall cachefun dict)
      (clrhash (funcall cachefun dict))))
  (when (called-interactively-p 'interactive)
    (message "Cleared caches for dictionary %s" (dictree-name dict))))




;; ----------------------------------------------------------------
;;                        Retrieving data

(defun dictree-member (dict key &optional nilflag)
  "Return the data associated with KEY in dictionary DICT,
or nil if KEY is not in the dictionary.

Optional argument NILFLAG specifies a value to return instead of
nil if KEY does not exist in TREE. This allows a non-existent KEY
to be distinguished from an element with a null association. (See
also `dictree-member-p' for testing existence alone.)"
  (let* ((data (dictree--lookup dict key nilflag)))
    (if (eq data nilflag)
	nilflag
      (dictree--cell-data data))))

(defalias 'dictree-lookup 'dictree-member)

(defun dictree-member-p (dict key)
  "Return t if KEY exists in DICT, nil otherwise."
  (let ((flag '(nil)))
    (not (eq flag (dictree-member dict key flag)))))


(defun dictree--lookup (dict key nilflag)
  ;; Return association of KEY in DICT, or NILFLAG if KEY does not
  ;; exist. Does not do any data/meta-data unwrapping

  (let* ((flag '(nil))
	 (data flag)
	 time)
    ;; if KEY is in the cache, then we're done
    (unless (and (dictree-lookup-cache dict)
		 (setq data (gethash key (dictree--lookup-cache dict))))

      ;; otherwise, we have to look in the dictionary itself...
      (cond
       ;; if DICT is a meta-dict, look in its constituent dictionaries
       ((dictree--meta-dict-p dict)
	(let (newdata (newflag '(nil)))
	  ;; time the lookup for caching
	  (setq time (float-time))
	  ;; look in each constituent dictionary in turn
	  (dolist (dic (dictree--meta-dict-dictlist dict))
	    (setq newdata (dictree--lookup dic key newflag))
	    ;; skip dictionary if it doesn't contain KEY
	    (unless (eq newdata newflag)
	      ;; if we haven't found KEY before, we have now!
	      (if (eq data flag) (setq data newdata)
		;; otherwise, combine the previous data with the new data
		(setq data
		      (funcall (dictree--wrap-combfun
				(dictree--meta-dict-combine-function dict))
			       data newdata)))))
	  (setq time (- (float-time) time))))

       ;; otherwise, DICT is a normal dictionary, so look in it's trie
       (t
	;; time the lookup for caching
	(setq time (float-time))
	(setq data (trie-member (dictree--trie dict) key flag))
	(setq time (- (float-time) time))))

      ;; if lookup found something, and we're above the cache-threshold, cache
      ;; the result
      (when (and (not (eq data flag))
		 (dictree--above-cache-threshold-p
		  time (length key) (dictree-cache-policy dict)
		  (dictree-cache-threshold dict) 'long-keys))
	(setf (dictree-modified dict) t)
	;; create lookup cache if it doesn't already exist
	(dictree-create-lookup-cache dict)
	(puthash key data (dictree-lookup-cache dict))))

    ;; return the desired data
    (if (eq data flag) nilflag data)))




;; ----------------------------------------------------------------
;;                 Getting and setting meta-data

(defun dictree-put-property (dict key property value)
  "Set PROPERTY for KEY in dictionary DICT.
PROPERTY should be a symbol. Returns VALUE if successful, nil if
KEY was not found in DICT.

Note that if DICT is a meta-dictionary, then this will set KEY's
PROPERTY to VALUE in *all* its constituent dictionaries.

Unlike the data associated with a key (cf. `dictree-insert'),
properties are not included in the results of queries on the
dictionary \(`dictree-lookup', `dictree-complete',
`dictree-complete-ordered'\), nor do they affect the outcome of
any of the queries. They merely serves to tag a key with some
additional information, and can only be retrieved using
`dictree-get-property'."

  ;; sort out arguments
  (and (symbolp dict) (setq dict (symbol-value dict)))
  (cond
   ;; set PROPERTY for KEY in all constituent dicts of a meta-dict
   ((dictree--meta-dict-p dict)
    (warn "Setting %s property for key %s in all constituent\
 dictionaries of meta-dictionary %s" property key (dictree-name dict))
    (setf (dictree-modified dict) t)
    (let (dictree--put-property-ret)
      (mapc (lambda (dic k p v)
	      (setq dictree--put-property-ret
		    (or dictree--put-property-ret
			(dictree-put-property dic k p v))))
	    (dictree--meta-dict-dictlist dict))
      ;; return VALUE if KEY was found in at least one constituent dict
      dictree--put-property-ret))
   (t  ;; set PROPERTY for KEY in normal dict
    (let ((cell (trie-member (dictree--trie dict) key)))
      (when cell
	(setf (dictree-modified dict) t)
	(setf (dictree--cell-plist cell)
	      (plist-put (dictree--cell-plist cell) property value))
	value)))  ; return VALUE
   ))



(defun dictree-delete-property (dict key property)
  "Delete PROPERTY from KEY in dictionary DICT.
Returns the new property list for KEY, with PROPERTY deleted.

Setting PROPERTY to nil using `dictree-put-property' is not quite
the same thing as deleting it, since null property values can
still be detected by supplying the optional argument to
`dictree-get-propery' (which see).

Note that if DICT is a meta-dictionary, then this will delete
KEY's PROPERTY in *all* its constituent dictionaries."
  ;; sort out arguments
  (and (symbolp dict) (setq dict (symbol-value dict)))
  (cond
   ;; delete PROPERTY from KEY in all constituent dicts of a meta-dict
   ((dictree--meta-dict-p dict)
    (warn "Deleting %s property from key %s in all constituent\
 dictionaries of meta-dicttionary %s" property key (dictree-name dict))
    (setf (dictree-modified dict) t)
    (mapcar (lambda (dic k p) (dictree-delete-property dic k p))
	    (dictree--meta-dict-dictlist dict)))
   (t  ;; delete PROPERTY from KEY in normal dict
    (let* ((cell (trie-member (dictree--trie dict) key))
	   plist tail)
      (when (and cell
		 (setq tail
		       (plist-member
			(setq plist (dictree--cell-plist cell))
			property)))
	(setf (dictree-modified dict) t)
	;; delete property and value from plist
	(setcdr tail (cddr tail))
	(setq plist (delq property plist))
	(setf (dictree--cell-plist cell) plist))))
   ))



(defun dictree-get-property (dict key property &optional nilflag)
  "Get the value of PROPERTY for KEY in dictionary DICT,
or return nil if KEY is not in the dictionary.

Optional argument NILFLAG specifies a value to return instead of
nil if KEY does not exist in TREE. This allows a non-existent KEY
to be distinguished from a key for which PROPERTY is not
set. (See also `dictree-member-p' for testing existence alone.)"
  (let ((cell (dictree--lookup dict key nilflag)))
    (unless (eq cell nilflag)
      (plist-get (dictree--cell-plist cell) property))))




;; ----------------------------------------------------------------
;;                        Mapping functions

(defun dictree-mapc (function dict &optional type reverse)
  "Apply FUNCTION to all entries in dictionary DICT,
for side-effects only.

FUNCTION will be passed two arguments: a key of type
TYPE (`string', `vector', or `list', defaulting to `vector') from the
dictionary, and the data associated with that key. The dictionary
entries will be traversed in \"lexicographic\" order, i.e. the
order defined by the dictionary's comparison function (cf.
`dictree-create').

If TYPE is string, it must be possible to apply the function
`string' to the elements of sequences stored in DICT.

FUNCTION is applied in ascending order, or descending order if
REVERSE is non-nil."

  ;; "rename" FUNCTION to something hopefully unique to lessen the
  ;; likelihood of dynamic scoping bugs caused by a supplied function
  ;; binding a variable with the same name as one of the arguments
  (let ((--dictree-mapc--function function))
    (dictree--mapc
     (lambda (key data _plist)
       (funcall --dictree-mapc--function key data))
     dict type reverse)))



(defun dictree--mapc (function dict &optional type reverse)
  ;; Like `dictree-mapc', but FUNCTION is passed three arguments: the
  ;; key, the data, and the property list, instead of just key and data.

  ;; try to avoid dynamic binding bugs
  (let ((--dictree--mapc--function function))
    (if (dictree--meta-dict-p dict)
	;; for a meta-dict, use a dictree-stack
	(let ((stack (dictree-stack dict))
	      entry)
	  (while (setq entry (dictree--stack-pop stack))
	    (funcall --dictree--mapc--function
		     (car entry)
		     (dictree--cell-data (cdr entry))
		     (dictree--cell-plist (cdr entry)))))
      ;; for a normal dictionary, map the function over its trie
      (trie-mapc
       (lambda (key cell)
	 (funcall --dictree--mapc--function
		  key
		  (dictree--cell-data cell)
		  (dictree--cell-plist cell)))
       (dictree--trie dict)
       type reverse)
      )))



(defun dictree-mapf (function combinator dict &optional type reverse)
  "Apply FUNCTION to all entries in dictionary DICT,
and combine the results using COMBINATOR.

FUNCTION should take two arguments: a key sequence from the
dictionary and its associated data.

Optional argument TYPE (one of the symbols `vector', `lisp' or
`string'; defaults to `vector') sets the type of sequence passed to
FUNCTION. If TYPE is `string', it must be possible to apply the
function `string' to the individual elements of key sequences
stored in DICT.

The FUNCTION will be applied and the results combined in
asscending \"lexicographic\" order (i.e. the order defined by the
dictionary's comparison function; cf. `dictree-create'), or
descending order if REVERSE is non-nil."

  ;; try to avoid dynamic scoping bugs
  (let ((--dictree-mapf--function function)
	(--dictree-mapf--combinator combinator))

    ;; for a normal dictionary, map the function over its trie
    (if (not (dictree--meta-dict-p dict))
	(trie-mapf
	 `(lambda (key data)
	    (,--dictree-mapf--function key (dictree--cell-data data)))
	 --dictree-mapf--combinator (dictree--trie dict) type reverse)

      ;; for a meta-dict, use a dictree-stack
      (let ((--dictree-mapf--stack (dictree-stack dict))
	    --dictree-mapf--entry
	    --dictree-mapf--accumulate)
	(while (setq --dictree-mapf--entry
		     (dictree-stack-pop --dictree-mapf--stack))
	  (setq --dictree-mapf--accumulate
		(funcall --dictree-mapf--combinator
			 (funcall --dictree-mapf--function
				  (car --dictree-mapf--entry)
				  (cdr --dictree-mapf--entry))
			 --dictree-mapf--accumulate)))
	--dictree-mapf--accumulate))))



(defun dictree-mapcar (function dict &optional type reverse)
  "Apply FUNCTION to all entries in dictionary DICT,
and make a list of the results.

FUNCTION should take two arguments: a key sequence from the
dictionary and its associated data.

Optional argument TYPE (one of the symbols `vector', `list' or
`string'; defaults to `vector') sets the type of sequence passed
to FUNCTION. If TYPE is string, it must be possible to apply the
function `string' to the individual elements of key sequences
stored in DICT.

The FUNCTION will be applied and the results combined in
asscending \"lexicographic\" order \(i.e. the order defined by
the dictionary's comparison function; cf. `dictree-create'\), or
descending order if REVERSE is non-nil.

Note that if you don't care about the order in which FUNCTION is
applied, just that the resulting list is in the correct order,
then

  (dictree-mapf function #\\='cons dict type (not reverse))

is more efficient."
  (nreverse (dictree-mapf function #'cons dict type reverse)))



(defun dictree-size (dict)
  "Return the number of entries in dictionary DICT.
Interactively, DICT is read from the mini-buffer."
  (interactive (list (read-dict "Dictionary: ")))
  (when (and (called-interactively-p 'any) (symbolp dict))
    (setq dict (symbol-value dict)))
  (let ((count 0))
    (dictree-mapc (lambda (&rest _dummy) (incf count)) dict)
    (when (called-interactively-p 'interactive)
      (message "Dictionary %s contains %d entries"
	       (dictree--name dict) count))
    count))




;; ----------------------------------------------------------------
;;                        Using dictrees as stacks

;; A dictree--meta-stack is the meta-dict version of a dictree-stack (the
;; ordinary version is just a single trie-stack). It consists of a heap of
;; trie-stacks for its constituent tries, where the heap order is the usual
;; lexicographic order over the keys at the top of the trie-stacks.

(defstruct
  (dictree--meta-stack
   (:constructor nil)
   (:constructor dictree--meta-stack-create
		 (dict &optional (type 'vector) reverse
		  &aux
		  (combfun (dictree--wrap-combfun
			    (dictree--meta-dict-combine-function dict)))
		  (sortfun (trie-construct-sortfun
			    (dictree-comparison-function dict)))
		  (heap (heap-create
			 (dictree--construct-meta-stack-heapfun sortfun)
			 (length (dictree--trielist dict))))
		  (pushed '())
		  (_dummy (mapc
			   (lambda (dic)
			     (heap-add
			      heap (trie-stack dic type reverse)))
			   (dictree--trielist dict)))))
   (:constructor dictree--complete-meta-stack-create
		 (dict prefix &optional reverse
		  &aux
		  (combfun (dictree--wrap-combfun
			    (dictree--meta-dict-combine-function dict)))
		  (sortfun (trie-construct-sortfun
			    (dictree-comparison-function dict)))
		  (heap (heap-create
			 (dictree--construct-meta-stack-heapfun
			  sortfun reverse)
			 (length (dictree--trielist dict))))
		  (pushed '())
		  (_dummy (mapc
			   (lambda (trie)
			     (let ((stack (trie-complete-stack
					   trie prefix reverse)))
			       (unless (trie-stack-empty-p stack)
				 (heap-add heap stack))))
			   (dictree--trielist dict)))))
      (:constructor dictree--regexp-meta-stack-create
		 (dict regexp &optional reverse
		  &aux
		  (combfun (dictree--wrap-combfun
			    (dictree--meta-dict-combine-function dict)))
		  (sortfun (dictree--wrap-regexp-sortfun
			    (dictree-comparison-function dict) 'reverse))
		  (heap (heap-create
			 (dictree--construct-meta-stack-heapfun
			  sortfun reverse)
			 (length (dictree--trielist dict))))
		  (pushed '())
		  (_dummy (mapc
			   (lambda (trie)
			     (let ((stack (trie-regexp-stack
					   trie regexp reverse)))
			       (unless (trie-stack-empty-p stack)
				 (heap-add heap stack))))
			   (dictree--trielist dict)))))
      (:constructor dictree--fuzzy-match-meta-stack-create
		 (dict string distance &optional reverse
		  &aux
		  (combfun (dictree--wrap-combfun
			    (dictree--meta-dict-combine-function dict)))
		  (sortfun (dictree--wrap-fuzzy-sortfun
			    (dictree-comparison-function dict)))
		  (heap (heap-create
			 (dictree--construct-meta-stack-heapfun
			  sortfun reverse)
			 (length (dictree--trielist dict))))
		  (pushed '())
		  (_dummy (mapc
			   (lambda (trie)
			     (let ((stack (trie-fuzzy-match-stack
					   trie string distance reverse)))
			       (unless (trie-stack-empty-p stack)
				 (heap-add heap stack))))
			   (dictree--trielist dict)))))
      (:constructor dictree--fuzzy-complete-meta-stack-create
		 (dict prefix distance &optional reverse
		  &aux
		  (combfun (dictree--wrap-combfun
			    (dictree--meta-dict-combine-function dict)))
		  (sortfun (dictree--wrap-fuzzy-sortfun
			    (dictree-comparison-function dict)))
		  (heap (heap-create
			 (dictree--construct-meta-stack-heapfun
			  sortfun reverse)
			 (length (dictree--trielist dict))))
		  (pushed '())
		  (_dummy (mapc
			   (lambda (trie)
			     (let ((stack (trie-fuzzy-complete-stack
					   trie prefix distance reverse)))
			       (unless (trie-stack-empty-p stack)
				 (heap-add heap stack))))
			   (dictree--trielist dict)))))
   (:copier nil))
  combfun sortfun heap pushed)



(defun dictree--construct-meta-stack-heapfun (sortfun &optional reverse)
  ;; Wrap SORTFUN, which sorts keys, so it can act on
  ;; dictree--meta-stack elements.
  (if reverse
      `(lambda (b a) (,sortfun (car (dictree-stack-first a))
			       (car (dictree-stack-first b))))
    `(lambda (a b) (,sortfun (car (dictree-stack-first a))
			     (car (dictree-stack-first b))))))


(defun dictree-stack (dict &optional type reverse)
  "Create an object that allows DICT to be accessed as a stack.

The stack is sorted in \"lexicographic\" order, i.e. the order
defined by the DICT's comparison function, or in reverse order if
REVERSE is non-nil. Calling `dictree-stack-pop' pops the top
element (a key and its associated data) from the stack.

Optional argument TYPE (one of the symbols `vector', `list' or
`string') sets the type of sequence used for the keys.

Note that any modification to DICT *immediately* invalidates all
dictree-stacks created before the modification (in particular,
calling `dictree-stack-pop' will give unpredictable results).

Operations on dictree-stacks are significantly more efficient
than constructing a real stack from the dictionary and using
standard stack functions. As such, they can be useful in
implementing efficient algorithms on dictionaries. However, in
cases where mapping functions `dictree-mapc', `dictree-mapcar' or
`dictree-mapf' would be sufficient, it is better to use one of
those instead."
  (if (dictree--meta-dict-p dict)
      (dictree--meta-stack-create dict type reverse)
    (trie-stack (dictree--trie dict) type reverse)))


(defun dictree-complete-stack (dict prefix &optional reverse)
  "Return an object that allows completions of PREFIX to be accessed
as if they were a stack.

The stack is sorted in \"lexicographic\" order, i.e. the order
defined by DICT's comparison function, or in reverse order if
REVERSE is non-nil. Calling `dictree-stack-pop' pops the top
element (a key and its associated data) from the stack.

PREFIX must be a sequence (vector, list or string) that forms the
initial part of a DICT key. (If PREFIX is a string, it must be
possible to apply `string' to individual elements of DICT keys.)
The returned keys will be sequences of the same type as
PREFIX. If PREFIX is a list of sequences, completions of all
sequences in the list are included in the stack. All sequences in
the list must be of the same type.

Note that any modification to DICT *immediately* invalidates all
dictree-stacks created before the modification (in particular,
calling `dictree-stack-pop' will give unpredictable results).

Operations on dictree-stacks are significantly more efficient
than constructing a real stack from completions of PREFIX in DICT
and using standard stack functions. As such, they can be useful
in implementing efficient algorithms on dict-trees. However, in
cases where `dictree-complete' is sufficient, it is better to use
that instead."
  (if (dictree--meta-dict-p dict)
      (dictree--complete-meta-stack-create dict prefix reverse)
    (trie-complete-stack (dictree--trie dict) prefix reverse)))


(defun dictree-regexp-stack (dict regexp &optional reverse)
  "Return an object that allows REGEXP matches to be accessed
as if they were a stack.

The stack is sorted in \"lexicographic\" order, i.e. the order
defined by DICT's comparison function, or in reverse order if
REVERSE is non-nil. Calling `dictree-stack-pop' pops the top
element (a key and its associated data) from the stack.

REGEXP is a regular expression, but it need not necessarily be a
string. It must be a sequence (vector, list of string) whose
elements are either elements of the same type as elements of the
keys in DICT (which behave as literals in the regexp), or any of
the usual regexp special characters and backslash constructs. If
REGEXP is a string, it must be possible to apply `string' to
individual elements of the keys stored in DICT. The matches
returned in the alist will be sequences of the same type as KEY.

Back-references and non-greedy postfix operators are *not*
supported, and the matches are always anchored, so `$' and `^'
lose their special meanings.

If the regexp contains any non-shy grouping constructs, subgroup
match data is included in the results. In this case, the car of
each match is no longer just a key. Instead, it is a list whose
first element is the matching key, and whose remaining elements
are cons cells whose cars and cdrs give the start and end indices
of the elements that matched the corresponding groups, in order.

Note that any modification to DICT *immediately* invalidates all
dictree-stacks created before the modification (in particular,
calling `dictree-stack-pop' will give unpredictable results).

Operations on dictree-stacks are significantly more efficient
than constructing a real stack from completions of PREFIX in DICT
and using standard stack functions. As such, they can be useful
in implementing efficient algorithms on dict-trees. However, in
cases where `dictree-regexp-search' is sufficient, it is better
to use that instead."
  (if (dictree--meta-dict-p dict)
      (dictree--regexp-meta-stack-create dict regexp reverse)
    (trie-regexp-stack (dictree--trie dict) regexp reverse)))


(defun dictree-fuzzy-match-stack (dict string distance &optional reverse)
  "Return an object that allows fuzzy matches to be accessed
as if they were a stack.

The stack is sorted in \"lexicographic\" order, i.e. the order
defined by DICT's comparison function, or in reverse order if
REVERSE is non-nil. Calling `dictree-stack-pop' pops the top
element (a key and its associated data) from the stack.

STRING must be a sequence (vector, list or string), and DISTANCE
must be an integer. (If STRING is a string, it must be possible
to apply `string' to individual elements of DICT keys.) The
matches returned in the alist will be sequences of the same type
as STRING that are within Lewenstein distance DISTANCE of
STRING. If STRING is a list of sequences, keys withing DISTANCE
of any sequences in the list are included in the stack. All
sequences in the list must be of the same type.

Note that any modification to DICT *immediately* invalidates all
dictree-stacks created before the modification (in particular,
calling `dictree-stack-pop' will give unpredictable results).

Operations on dictree-stacks are significantly more efficient
than constructing a real stack from fuzzy matches within DISTANCE
of STRING in DICT and using standard stack functions. As such,
they can be useful in implementing efficient algorithms on
dict-trees. However, in cases where `dictree-fuzzy-match' is
sufficient, it is better to use that instead."
  (if (dictree--meta-dict-p dict)
      (dictree--fuzzy-match-meta-stack-create dict string distance reverse)
    (trie-fuzzy-match-stack (dictree--trie dict) string distance reverse)))


(defun dictree-fuzzy-complete-stack (dict prefix distance &optional reverse)
  "Return an object that allows fuzzy completions to be accessed
as if they were a stack.

The stack is sorted in \"lexicographic\" order, i.e. the order
defined by DICT's comparison function, or in reverse order if
REVERSE is non-nil. Calling `dictree-stack-pop' pops the top
element (a key and its associated data) from the stack.

PREFIX must be a sequence (vector, list or string), and DISTANCE
must be an integer. (If PREFIX is a string, it must be possible
to apply `string' to individual elements of DICT keys.) The
completions returned in the alist will be sequences of the same
type as STRING that are completions of prefixes within Lewenstein
distance DISTANCE of PREFIX. If PREFIX is a list of sequences,
completions within DISTANCE of any prefix in the list are
included in the stack. All sequences in the list must be of the
same type.

Note that any modification to DICT *immediately* invalidates all
dictree-stacks created before the modification (in particular,
calling `dictree-stack-pop' will give unpredictable results).

Operations on dictree-stacks are significantly more efficient
than constructing a real stack from fuzzy matches within DISTANCE
of STRING in DICT and using standard stack functions. As such,
they can be useful in implementing efficient algorithms on
dict-trees. However, in cases where `dictree-fuzzy-complete' is
sufficient, it is better to use that instead."
  (if (dictree--meta-dict-p dict)
      (dictree--fuzzy-complete-meta-stack-create dict prefix distance reverse)
    (trie-fuzzy-complete-stack (dictree--trie dict) prefix distance reverse)))


(defun dictree-stack-pop (dictree-stack)
  "Pop the first element from the DICTREE-STACK.
Returns nil if the stack is empty."
  (cond
   ;; if elements have been pushed onto a dict stack, pop those first
   ;; FIXME: shouldn't be using internal trie functions!
   ((and (trie-stack-p dictree-stack)
	 (trie--stack-pushed dictree-stack))
    (trie-stack-pop dictree-stack))
   ;; if elements have been pushed onto a meta-dict stack, pop those
   ;; first
   ((and (dictree--meta-stack-p dictree-stack)
	 (dictree--meta-stack-pushed dictree-stack))
    (pop (dictree--meta-stack-pushed dictree-stack)))
   ;; otherwise, pop first element from dictree-stack
   (t (let ((popped (dictree--stack-pop dictree-stack)))
	(when popped
	  (cons (car popped) (dictree--cell-data (cdr popped))))))
   ))


(defun dictree-stack-push (element dictree-stack)
  "Push ELEMENT onto DICTREE-STACK."
  (if (trie-stack-p dictree-stack)
      ;; normal dict
      (trie-stack-push element dictree-stack)
    ;; meta-dict
    (push element (dictree--meta-stack-pushed dictree-stack))))


(defun dictree-stack-first (dictree-stack)
  "Return the first element from DICTREE-STACK, without removing it.
Returns nil if the stack is empty."
  ;; if elements have been pushed onto the stack, return first of those
  (if (and (dictree--meta-stack-p dictree-stack)
	   (dictree--meta-stack-pushed dictree-stack))
      (car (dictree--meta-stack-pushed dictree-stack))
    ;; otherwise, return first element from dictree-stack
    (let ((first (dictree--stack-first dictree-stack)))
      (cons (car first) (dictree--cell-data (cdr first))))))


(defun dictree-stack-empty-p (dictree-stack)
  "Return t if DICTREE-STACK is empty, nil otherwise."
  (if (trie-stack-p dictree-stack)
      ;; normal dict
      (trie-stack-empty-p dictree-stack)
    ;; meta-dict
    (and (heap-empty (dictree--meta-stack-heap dictree-stack))
	 (null (dictree--meta-stack-pushed dictree-stack)))))


(defun dictree--stack-first (dictree-stack)
  "Return the first element from DICTREE-STACK, without removing it.
Returns nil if the stack is empty."
  (if (trie-stack-p dictree-stack)
      ;; normal dict
      (trie-stack-first dictree-stack)
    ;; meta-dict
    (if (dictree--meta-stack-pushed dictree-stack)
	;; pushed element
	(car (dictree--meta-stack-pushed dictree-stack))
      ;; dictree-stack element
      (dictree--stack-first
       (heap-root (dictree--meta-stack-heap dictree-stack))))))


(defun dictree--stack-pop (dictree-stack)
  ;; Pop the raw first element from DICTREE-STACK. Returns nil if the
  ;; stack is empty.

  ;; dictree-stack for normal dictionaries is a trie-stack
  (if (trie-stack-p dictree-stack)
      (trie-stack-pop dictree-stack)

    ;; meta-dictionary dictree-stack...more work!
    ;; if elements have been pushed onto meta-dict stack, pop those
    ;; first
    (if (dictree--meta-stack-pushed dictree-stack)
	(pop (dictree--meta-stack-pushed dictree-stack))
      ;; otherwise...
      (let ((heap (dictree--meta-stack-heap dictree-stack))
	    (sortfun (dictree--meta-stack-sortfun dictree-stack))
	    stack curr next)
	(unless (heap-empty heap)
	  ;; remove the first dictree-stack from the heap, pop it's
	  ;; first element, and add it back to the heap (note that it
	  ;; will almost certainly not end up at the root again)
	  (setq stack (heap-delete-root heap))
	  (setq curr (dictree--stack-pop stack))
	  (unless (dictree-stack-empty-p stack) (heap-add heap stack))
	  ;; peek at the first element of the stack now at the root of
	  ;; the heap
	  (unless (heap-empty heap)
	    (setq next (dictree--stack-first (heap-root heap)))
	    ;; repeat this as long as we keep finding elements with the
	    ;; same key, combining them together as we go
	    (when (dictree--meta-stack-combfun dictree-stack)
	      (while (and (null (funcall sortfun
					 (car curr) (car next)))
			  (null (funcall sortfun
					 (car next) (car curr))))
		(setq stack (heap-delete-root heap))
		(setq next (dictree--stack-pop stack))
		(setq curr (cons (car curr)
				 (funcall
				  (dictree--meta-stack-combfun dictree-stack)
				  (cdr curr) (cdr next))))
		(heap-add heap stack)
		(setq next (dictree--stack-first (heap-root heap))))))
	  ;; return the combined dictionary element
	  curr)))))




;; ----------------------------------------------------------------
;;                    dictree iterator generators

;; dictree-stacks *are* iterators (with additional push and
;; inspect-first-element operations). If we're running on a modern Emacs that
;; includes the `generator' library, we can trivially define dictree iterator
;; generators in terms of dictree-stacks.

(heap--when-generators
 (iter-defun dictree-iter (dict &optional type reverse)
   "Return a dictree iterator object.

Calling `iter-next' on this object will retrieve the next
element (a cons cell containing a key and its associated data)
from DICT in \"lexicographic\" order, i.e. the order defined by
the DICT's comparison function, or in reverse order if REVERSE is
non-nil.

Optional argument TYPE (one of the symbols `vector', `list' or
`string') sets the type of sequence used for the keys.

Note that any modification to DICT *immediately* invalidates all
iterators created from DICT before the modification (in
particular, calling `iter-next' will give unpredictable
results). If DICT is a meta-dict, this includes any modifications
to its constituent dicts."
   (let ((stack (dictree-stack dict type reverse)))
     (while (not (dictree-stack-empty-p stack))
       (iter-yield (dictree-stack-pop stack))))))


(heap--when-generators
 (iter-defun dictree-complete-iter (dict prefix &optional reverse)
   "Return an iterator object for completions of PREFIX in DICT.

Calling `iter-next' on this object will retrieve the next
completion of PREFIX (a cons cell containing a key and its
associated data) from DICT in \"lexicographic\" order, i.e. the
order defined by DICT's comparison function, or in reverse order
if REVERSE is non-nil.

PREFIX must be a sequence (vector, list or string) that forms the
initial part of a DICT key. (If PREFIX is a string, it must be
possible to apply `string' to individual elements of DICT keys.)
The returned keys will be sequences of the same type as
PREFIX. If PREFIX is a list of sequences, completions of all
sequences in the list are included in the stack. All sequences in
the list must be of the same type.

Note that any modification to DICT *immediately* invalidates all
iterators created from DICT before the modification (in
particular, calling `iter-next' will give unpredictable
results). If DICT is a meta-dict, this includes any modifications
to its constituent dicts."
   (let ((stack (dictree-complete-stack dict prefix reverse)))
     (while (not (dictree-stack-empty-p stack))
       (iter-yield (dictree-stack-pop stack))))))


(heap--when-generators
 (iter-defun dictree-regexp-iter (dict regexp &optional reverse)
   "Return an iterator object for REGEXP matches in DICT.

Calling `iter-next' on this object will retrieve the next match
\(a cons cell containing a key and its associated data\) in
\"lexicographic\" order, i.e. the order defined by DICT's
comparison function, or in reverse order if REVERSE is non-nil.

REGEXP is a regular expression, but it need not necessarily be a
string. It must be a sequence (vector, list of string) whose
elements are either elements of the same type as elements of the
keys in DICT (which behave as literals in the regexp), or any of
the usual regexp special characters and backslash constructs. If
REGEXP is a string, it must be possible to apply `string' to
individual elements of the keys stored in DICT. The matches
returned in the alist will be sequences of the same type as KEY.

Back-references and non-greedy postfix operators are *not*
supported, and the matches are always anchored, so `$' and `^'
lose their special meanings.

If the regexp contains any non-shy grouping constructs, subgroup
match data is included in the results. In this case, the car of
each match is no longer just a key. Instead, it is a list whose
first element is the matching key, and whose remaining elements
are cons cells whose cars and cdrs give the start and end indices
of the elements that matched the corresponding groups, in order.

Note that any modification to DICT *immediately* invalidates all
iterators created from DICT before the modification (in
particular, calling `iter-next' will give unpredictable
results). If DICT is a meta-dict, this includes any modifications
to its constituent dicts."
   (let ((stack (dictree-regexp-stack dict regexp reverse)))
     (while (not (dictree-stack-empty-p stack))
       (iter-yield (dictree-stack-pop stack))))))

(heap--when-generators
 (iter-defun dictree-fuzzy-match-iter (dict string distance &optional reverse)
   "Return an iterator object for fuzzy matches to STRING in DICT.

Calling `iter-next' on this object will retrieve the next match
\(a cons cell containing a key and its associated data\) in
\"lexicographic\" order, i.e. the order defined by DICT's
comparison function, or in reverse order if REVERSE is non-nil.

STRING must be a sequence (vector, list or string), and DISTANCE
must be an integer. (If STRING is a string, it must be possible
to apply `string' to individual elements of DICT keys.) The
returned keys will be sequences of the same type as STRING that
are within Lewenstein distance DISTANCE of STRING. If STRING is a
list of sequences, keys withing DISTANCE of any sequences in the
list are included in the stack. All sequences in the list must be
of the same type.

Note that any modification to DICT *immediately* invalidates all
iterators created from DICT before the modification (in
particular, calling `iter-next' will give unpredictable
results). If DICT is a meta-dict, this includes any modifications
to its constituent dicts."
   (let ((stack (dictree-fuzzy-match-stack dict string distance reverse)))
     (while (not (dictree-stack-empty-p stack))
       (iter-yield (dictree-stack-pop stack))))))


(heap--when-generators
 (iter-defun dictree-fuzzy-complete-iter (dict prefix distance &optional reverse)
   "Return an iterator object for fuzzy completions of PREFIX in DICT.

Calling `iter-next' on this object will retrieve the next fuzzy
completion in \"lexicographic\" order, i.e. the order defined by
DICT's comparison function, or in reverse order if REVERSE is
non-nil. Each returned element has the form:

    ((KEY . DIST) . DATA)

PREFIX must be a sequence (vector, list or string), and DISTANCE
must be an integer. (If PREFIX is a string, it must be possible
to apply `string' to individual elements of DICT keys.) The
returned keys will be sequences of the same type as STRING that
are completions of prefixes within Lewenstein distance DISTANCE
of PREFIX. If PREFIX is a list of sequences, completions within
DISTANCE of any prefix in the list are included in the stack. All
sequences in the list must be of the same type.

Note that any modification to DICT *immediately* invalidates all
iterators created from DICT before the modification (in
particular, calling `iter-next' will give unpredictable
results). If DICT is a meta-dict, this includes any modifications
to its constituent dicts."
   (let ((stack (dictree-fuzzy-complete-stack dict prefix distance reverse)))
     (while (not (dictree-stack-empty-p stack))
       (iter-yield (dictree-stack-pop stack))))))




;; ----------------------------------------------------------------
;;             Functions for building advanced queries

(defun dictree--query
    (dict triefun stackfun cachefun cachecreatefun cache-long no-cache arg
     &optional auxargs rank-function rankfun maxnum reverse filter resultfun)
  ;; Return results of querying DICT with argument ARG (and AUXARGS list, if
  ;; any) using TRIEFUN or STACKFUN. If DICT's cache-threshold is non-nil,
  ;; look first for cached result in cache returned by calling CACHEFUN on
  ;; DICT, and cache result if query fulfils caching conditions. Non-nil
  ;; CACHE-LONG indicates long ARGs should be cached, rather than short
  ;; ARGs. If RANK-FUNCTION is non-nil, return results ordered
  ;; accordingly. RANKFUN should be the appropriately wrapped version of
  ;; RANK-FUNCTION. If MAXNUM is an integer, only the first MAXNUM results
  ;; will be returned. If REVERSE is non-nil, results are in reverse order. A
  ;; non-nil NO-CACHE prevents caching of results, irrespective of DICT's
  ;; cache settings. If FILTER is supplied, only results that pass FILTER are
  ;; included. A non-nil RESULTFUN is applied to results before adding them to
  ;; final results list. Otherwise, an alist of key-data associations is
  ;; returned.

  ;; map over all dictionaries in list
  (when (dictree-p dict) (setq dict (list dict)))
  (let ((sort-function (dictree--construct-sortfun (car dict)))
	cache results res cache-entry)
    (dolist (dic dict)
      (when cachefun (setq cache (funcall cachefun dic)))
      (cond

       ;; if there's a cache entry with enough results, use it
       ((and (symbolp rank-function) (symbolp filter)
	     (setq cache-entry
		   (when cache
		     (gethash (list arg auxargs rank-function reverse filter)
			      cache)))
	     (or (null (dictree--cache-maxnum cache-entry))
		 (and maxnum
		      (<= maxnum (dictree--cache-maxnum cache-entry)))))
	(setq res (dictree--cache-results cache-entry))
	;; drop any excess results
	(when (and maxnum
		   (or (null (dictree--cache-maxnum cache-entry))
		       (> (dictree--cache-maxnum cache-entry) maxnum)))
	  (setcdr (nthcdr (1- maxnum) results) nil)))

       (t  ;; if there was nothing useful in the cache, do query and time it
	(let (time)
	  (setq time (float-time))
	  (setq res
		(dictree--do-query
		 dic triefun stackfun arg auxargs rankfun maxnum reverse
		 (when filter (dictree--wrap-filter filter))))
	  (setq time (- (float-time) time))
	  ;; if we're above the dictionary's cache threshold, cache the result
	  (when (and cachefun (not no-cache)
		     (dictree--above-cache-threshold-p
		      time (length arg) (dictree-cache-policy dic)
		      (dictree-cache-threshold dic) cache-long))
	    (setf (dictree-modified dic) t)
	    ;; create query cache if it doesn't already exist
	    (funcall cachecreatefun dic)
	    (puthash (list arg auxargs rank-function reverse filter)
		     (dictree--cache-create res maxnum)
		     (funcall cachefun dic))))))

      ;; merge new result into results list
      (setq results
	    (dictree--merge results res (or rankfun sort-function)
			    nil maxnum)))


    ;; return results list, applying RESULTFUN if specified, otherwise just
    ;; stripping meta-data
    (mapcar (if resultfun
		(dictree--wrap-resultfun resultfun)
	      (lambda (el) (cons (car el) (dictree--cell-data (cdr el)))))
	    results)))



(defun dictree--do-query
   (dict triefun stackfun arg &optional auxargs rankfun maxnum reverse filter)
  ;; Return first MAXNUM results of querying DICT with argument ARG (and
  ;; AUXARGS list, if any) using TRIEFUN or STACKFUN that satisfy FILTER,
  ;; ordered according to RANKFUN (defaulting to "lexicographic" order).

  ;; for a meta-dict, use a dictree-stack
  (if (dictree--meta-dict-p dict)
      (let ((stack (apply stackfun
			  (append (list dict arg) auxargs (list reverse))))
	    (heap (when rankfun
		    (heap-create   ; heap order is inverse of rank order
			(if reverse
			    rankfun
			  (lambda (a b)
			    (not (funcall rankfun a b))))
			(1+ maxnum))))
	    (i 0) res results)
	;; pop MAXNUM results from the stack
	(while (and (or (null maxnum) (< i maxnum))
		    (setq res (dictree--stack-pop stack)))
	  ;; check result passes FILTER
	  (when (or (null filter) (funcall filter res))
	    (if rankfun
		(heap-add heap res)   ; for ranked query, add to heap
	      (push res results))     ; for lexicographic query, add to list
	    (incf i)))
	(if (null rankfun)
	    ;; for lexicographic query, reverse and return result list (we
	    ;; built it backwards)
	    (nreverse results)
	  ;; for ranked query, pass rest of results through heap
	  (while (setq res (dictree--stack-pop stack))
	    (heap-add heap res)
	    (heap-delete-root heap))
	  ;; extract results from heap
	  (while (setq res (heap-delete-root heap))
	    (push res results))
	  results))  ; return result list

    ;; for a normal dict, call corresponding trie function on dict's
    ;; trie. Note: could use a dictree-stack here too - would it be more
    ;; efficient?
    (apply triefun
	   (append (list (dictree--trie dict) arg) auxargs
		   (list rankfun maxnum reverse filter)))))




;; ----------------------------------------------------------------
;;                        Completing

(defun dictree-complete
  (dict prefix
   &optional rank-function maxnum reverse filter resultfun no-cache)
  "Return an alist containing all completions of PREFIX in DICT
along with their associated data, sorted according to
RANK-FUNCTION (defaulting to \"lexicographic\" order, i.e. the
order defined by the dictionary's comparison function,
cf. `dictree-create'). Return nil if no completions are found.

PREFIX can also be a list of sequences, in which case completions
of all elements in the list are returned, merged together in a
single sorted alist.

DICT can also be a list of dictionaries, in which case
completions are sought in all dictionaries in the list. (Note
that if the same key appears in multiple dictionaries, the alist
may contain the same key multiple times, each copy associated
with the data from a different dictionary. If you want to combine
identical keys, use a meta-dictionary; see
`dictree-create-meta-dict'.)

If optional argument RANK-FUNCTION is t, the completions are
sorted according to the dictionary's rank-function (see
`dictree-create'). Any non-nil value that *is* a function
over-rides this. In that case, RANK-FUNCTION should accept two
arguments, both cons cells. The car of each contains a completion
from DICT (of the same type as PREFIX), the cdr contains its
associated data. The RANK-FUNCTION should return non-nil if first
argument is ranked strictly higher than the second, nil
otherwise.

The optional integer argument MAXNUM limits the results to the
first MAXNUM completions. The default is to return all matches.

If the optional argument NO-CACHE is non-nil, it prevents caching
of the result. Ignored for dictionaries that do not have
completion caching enabled.

The FILTER argument sets a filter function for the
completions. For each potential completion, it is passed two
arguments: the completion, and its associated data. If the filter
function returns nil, the completion is not included in the
results, and doesn't count towards MAXNUM.

RESULTFUN defines a function used to process results before
adding them to the final result list. If specified, it should
accept two arguments: a key and its associated data. Its return
value is what gets added to the final result list, instead of the
default key-data cons cell."
  ;; run completion query
  (dictree--query
   dict #'trie-complete #'dictree-complete-stack
   #'dictree-complete-cache #'dictree-create-complete-cache
   nil no-cache  ; cache short PREFIXes
   prefix nil
   rank-function
   (when rank-function
     (if (functionp rank-function)
	 (dictree--wrap-rankfun rank-function)
       (dictree--wrap-rankfun
	(dictree--rank-function (if (listp dict) (car dict) dict)))))
   maxnum reverse filter resultfun))


(defun dictree-collection-function (dict string predicate all)
  "Function for use in `try-completion', `all-completions',
and `completing-read'. To complete from dictionary DICT, use the
following as the COLLECTION argument of any of those functions:

  (lambda (string predicate all)
    (dictree-collection-function dict string predicate all))

Note that PREDICATE will be called with two arguments: the
completion, and its associated data."
  (let ((completions
	 (dictree-complete dict string nil nil nil predicate
			   (lambda (key _data) key))))
    (if all completions (try-completion "" completions))))




;; ----------------------------------------------------------------
;;                      Regexp search

(defun dictree-regexp-search
  (dict regexp
   &optional rank-function maxnum reverse filter resultfun no-cache)
  "Return an alist containing all matches for REGEXP in DICT
along with their associated data, in the order defined by
RANKFUN, defauling to \"lexicographic\" order. If REVERSE is
non-nil, the completions are sorted in the reverse order. Returns
nil if no completions are found.

DICT can also be a list of dictionaries, in which case matches
are sought in all dictionaries in the list. (Note that if the
same key appears in multiple dictionaries, the alist may contain
the same key multiple times, each copy associated with the data
from a different dictionary. If you want to combine identical
keys, use a meta-dictionary; see `dictree-create-meta-dict'.)

REGEXP is a regular expression, but it need not necessarily be a
string. It must be a sequence (vector, list of string) whose
elements are either of the same type as elements of DICT
keys (these behave as literals in the regexp), or any of the
usual regexp special characters and backslash constructs. If
REGEXP is a string, it must be possible to apply `string' to
individual elements of the keys stored in DICT. The matches
returned in the alist will be sequences of the same type as
REGEXP.

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
first MAXNUM matches. The default is to return all matches.

If the optional argument NO-CACHE is non-nil, it prevents caching
of the result. Ignored for dictionaries that do not have wildcard
caching enabled.


If optional argument RANK-FUNCTION is t, the matches are sorted
according to the dictionary's rank-function (see
`dictree-create').

Any other non-nil value of RANK-FUNCTION should be a function
which accepts two arguments. If the regexp does not contain any
non-shy grouping constructs, both arguments are (KEY . DATA) cons
cells, where the car is a sequence of the same type as REGEXP. If
the regexp does contain non-shy grouping constructs, both
arguments are of the form

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

  ;; run regexp query
  (dictree--query
   dict #'trie-regexp-search #'dictree-regexp-stack
   #'dictree-regexp-cache #'dictree-create-regexp-cache
   (if (and (eq (elt regexp (- (length regexp) 2)) ?.)
	    (eq (elt regexp (- (length regexp) 1)) ?*))
       nil  ; cache short REGEXP if it ends in .*
     t)     ; cache long REGEXPs otherwise
   no-cache
   regexp nil
   rank-function
   (when rank-function
     (if (functionp rank-function)
	 (dictree--wrap-regexp-rankfun rank-function)
       (dictree--wrap-regexp-rankfun
	(dictree-rank-function (if (listp dict) (car dict) dict)))))
   maxnum reverse filter resultfun))




;; ----------------------------------------------------------------
;;                      Fuzzy queries

(defun dictree-fuzzy-match
  (dict string distance
   &optional rank-function maxnum reverse filter resultfun no-cache)
  "Return matches for STRING in DICT within Lewenstein DISTANCE
\(edit distance\) of STRING along with their associated data, in
the order defined by RANKFUN, defauling to \"lexicographic\"
order. If REVERSE is non-nil, the matches are sorted in the
reverse order. Returns nil if no completions are found.

Returns a list of matches, with elements of the form:

    ((KEY . DIST) . DATA)

where KEY is a matching key from the trie, DATA its associated
data, and DIST is its Lewenstein distance \(edit distance\) from
STRING.

DICT can also be a list of dictionaries, in which case matches
are sought in all dictionaries in the list. (Note that if the
same key appears in multiple dictionaries, the alist may contain
the same key multiple times, each copy associated with the data
from a different dictionary. If you want to combine identical
keys, use a meta-dictionary; see `dictree-create-meta-dict'.)

STRING is a sequence (vector, list or string), whose elements
must be of the same type as elements of the keys stored in
DICT. If STRING is a string, it must be possible to apply
`string' to individual elements of DICT keys. The KEYs returned
in the list will be sequences of the same type as STRING.

DISTANCE must be an integer, and specifies the maximum Lewenstein
distance \(edit distances\) of matches from STRING.


If optional argument RANK-FUNCTION is the symbol `distance', the
matches are sorted according to their Lewenstein distance from
STRING. If it is t, the matches are sorted according to the
dictionary's rank-function (see `dictree-create').

Any other non-nil value of RANK-FUNCTION should be a function
which accepts two arguments, both of the form

  ((KEY . DIST) . DATA)

where KEY is a sequence from the dictionary (of the same type as
STRING), DIST is its Lewenstein distance from STRING, and DATA is
its associated data. The RANK-FUNCTION should return non-nil if
the first argument is ranked strictly higher than the second, nil
otherwise.


The optional integer argument MAXNUM limits the results to the
first MAXNUM matches. The default is to return all matches.

If the optional argument NO-CACHE is non-nil, it disables any
caching of the result.

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

  ;; run fuzzy-match query
  (dictree--query
   dict #'trie-fuzzy-match #'dictree-fuzzy-match-stack
   #'dictree-fuzzy-match-cache #'dictree-create-fuzzy-match-cache
   t no-cache  ; cache long STRINGs
   string (list distance)
   rank-function
   (when rank-function
     (cond
      ((eq rank-function 'distance) t)
      ((functionp rank-function) (dictree--wrap-fuzzy-rankfun rank-function))
      ((eq rank-function t)
       (dictree--wrap-fuzzy-rankfun
	(dictree-rank-function (if (listp dict) (car dict) dict))))))
   maxnum reverse filter resultfun))


(defun dictree-fuzzy-complete
  (dict prefix distance
   &optional rank-function maxnum reverse filter resultfun no-cache)
  "Return completion of prefixes in DICT within Lewenstein DISTANCE
\(edit distance\) of PREFIX along with their associated data, in
the order defined by RANKFUN, defauling to \"lexicographic\"
order. If REVERSE is non-nil, the matches are sorted in the
reverse order. Returns nil if no completions are found.

Returns a list of completions, with elements of the form:

    ((KEY DIST PFXLEN) . DATA)

where KEY is a matching completion from the trie, DATA its
associated data, PFXLEN is the length of the prefix part of KEY,
and DIST is its Lewenstein distance \(edit distance\) from
PREFIX.

DICT can also be a list of dictionaries, in which case matches
are sought in all dictionaries in the list. (Note that if the
same key appears in multiple dictionaries, the alist may contain
the same key multiple times, each copy associated with the data
from a different dictionary. If you want to combine identical
keys, use a meta-dictionary; see `dictree-create-meta-dict'.)

PREFIX is a sequence (vector, list or string), whose elements
must be of the same type as elements of the keys stored in
DICT. If PREFIX is a string, it must be possible to apply
`string' to individual elements of DICT keys. The KEYs returned
in the list will be sequences of the same type as PREFIX.

DISTANCE must be an integer, and specifies the maximum Lewenstein
distance \(edit distances\) of prefixes from PREFIX.


If optional argument RANK-FUNCTION is the symbol `distance', the
matches are sorted by increasing Lewenstein distance of their
prefix \(with same-distance prefixes ordered
lexicographically\). If it is t, the matches are sorted according
to the dictionary's rank-function (see `dictree-create').

Any other non-nil value of RANK-FUNCTION should be a function
that accepts two arguments, both of the form:

    ((KEY DIST PFXLEN) . DATA)

where KEY is a completion (of the same type as PREFIX), DIST is
its Lewenstein distances from PREFIX, and DATA is its associated
data. RANKFUN should return non-nil if first argument is ranked
strictly higher than the second, nil otherwise.


The optional integer argument MAXNUM limits the results to the
first MAXNUM matches. The default is to return all matches.

If the optional argument NO-CACHE is non-nil, it prevents caching
of the result. Ignored for dictionaries that do not have
fuzzy-match caching enabled.


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

  ;; run fuzzy-complete query
  (dictree--query
   dict #'trie-fuzzy-complete #'dictree-fuzzy-complete-stack
   #'dictree-fuzzy-complete-cache #'dictree-create-fuzzy-complete-cache
   nil no-cache  ; cache short PREFIXes
   prefix (list distance)
   rank-function
   (when rank-function
     (cond
      ((eq rank-function 'distance) t)
      ((functionp rank-function) (dictree--wrap-fuzzy-rankfun rank-function))
      ((eq rank-function t)
       (dictree--wrap-fuzzy-rankfun
	(dictree-rank-function (if (listp dict) (car dict) dict))))))
   maxnum reverse filter resultfun))




;; ----------------------------------------------------------------
;;                    Persistent storage

(defun dictree-save (dict &optional compilation)
  "Save dictionary DICT to its associated file.
Use `dictree-write' to save to a different file.

Optional argument COMPILATION determines whether to save the
dictionary in compiled or uncompiled form. The default is to save
both forms. See `dictree-write'.

Interactively, DICT is read from the mini-buffer."
  (interactive (list (read-dict "Dictionary: ")))
  (when (and (called-interactively-p 'any) (symbolp dict))
    (setq dict (symbol-value dict)))

  (let ((filename (dictree-filename dict)))
    ;; if dictionary has no associated file, prompt for one
    (unless (and filename (> (length filename) 0))
      (setq filename
	    (read-file-name
	     (format "Save dictionary %s to file\
 (leave blank to NOT save): "
		     (dictree-name dict))
	     nil "")))

    ;; if filename is blank, don't save
    (if (string= filename "")
	(message "Dictionary %s NOT saved" (dictree-name dict))
      ;; otherwise write dictionary to file
      (setf (dictree-filename dict) filename)
      (dictree-write dict filename t compilation))))



(defun dictree-write (dict &optional filename overwrite compilation)
  "Write dictionary DICT to file FILENAME.
Defaults to dictionary's current filename if FILENAME is not
specified (like `dictree-save').

If optional argument OVERWRITE is non-nil, no confirmation will
be asked for before overwriting an existing file.

The default is to create both compiled and uncompiled versions of
the dictionary, with extensions .elc and .el respectively (if
FILENAME has either of these extensions, they are stripped off
before proceeding). The compiled version is always used in
preference to the uncomplied version, as it loads
faster. However, only the uncompiled version is portable between
different Emacs versions.

If optional argument COMPILATION is the symbol `compiled', only
the compiled version will be created, whereas if it is the symbol
`uncompiled', only the uncompiled version will be created.

Interactively, DICT and FILENAME are read from the mini-buffer,
and OVERWRITE is the prefix argument."
  (interactive (list (read-dict "Dictionary: ")
		     (read-file-name "Write dictionary to file: "
				     nil "")
		     current-prefix-arg))
  (when (and (called-interactively-p 'any) (symbolp dict))
    (setq dict (symbol-value dict)))
  ;; default to DICT's current file, if any
  (when (or (null filename)
	    (and (called-interactively-p 'any) (string= filename "")))
    (setq filename (dictree-filename dict)))
  (if (null filename)
      (progn
	(message "Dictionary %s NOT written" (dictree-name dict))
	nil)  ; indicate dictionary wasn't written

    (let (dictname buff tmpfile)
      ;; remove any .el(c) extension from filename
      (cond
       ((and (> (length filename) 3)
	     (string= (substring filename -3) ".el"))
	(setq filename (substring filename 0 -3)))
       ((and (> (length filename) 4)
	     (string= (substring filename -4) ".elc"))
	(setq filename (substring filename 0 -4))))
      ;; create saved dictionary name from filename
      (setq dictname (file-name-nondirectory filename))

      (save-excursion
	;; create a temporary file
	(setq buff
	      (find-file-noselect
	       (setq tmpfile (make-temp-file dictname))))
	(set-buffer buff)
	;; call the appropriate write function to write the dictionary code
	(if (dictree--meta-dict-p dict)
	    (dictree--write-meta-dict-code dict dictname filename)
	  (dictree--write-dict-code dict dictname filename))
	(save-buffer)
	(kill-buffer buff))

      ;; prompt to overwrite if necessary
      (when (or overwrite
		(and
		 (or (eq compilation 'compiled)
		     (not (file-exists-p (concat filename ".el"))))
		 (or (eq compilation 'uncompiled)
		     (not (file-exists-p (concat filename ".elc")))))
		(y-or-n-p
		 (format "File %s already exists. Overwrite? "
			 (concat filename ".el(c)"))))
	(condition-case nil
	    (progn
	      ;; move the uncompiled version to its final destination
	      (unless (eq compilation 'compiled)
		(copy-file tmpfile (concat filename ".el") t))
	      ;; byte-compile and move the compiled version to its final
	      ;; destination
	      (unless (eq compilation 'uncompiled)
		(if (save-window-excursion
		      (let ((byte-compile-disable-print-circle t)
			    err)
			(setq err (byte-compile-file tmpfile))
			err))
		    (rename-file (concat tmpfile ".elc")
				 (concat filename ".elc") t)
		  (error ""))))
	  (error "Error writing dictionary. Dictionary %s NOT saved"
		 dictname))

	;; if writing to a different name, unload dictionary under old
	;; name and reload it under new one
	(setf (dictree-modified dict) nil)
	(setf (dictree-filename dict) filename)
	(unless (string= dictname (dictree-name dict))
	  (dictree-unload dict)
	  (dictree-load filename)))

      (delete-file tmpfile)
      (message "Dictionary %s saved to %s" dictname filename)
      t)  ; return t to indicate dictionary was successfully saved
    ))



(defun dictree-save-modified (&optional dict ask compilation force
					no-fail-query)
  "Save all modified dictionaries that have their autosave flag set.
Returns t if all dictionaries were successfully saved. Otherwise,
inform the user about the dictionaries which failed to save
properly, ask them whether they wish to continue anyway, and
return t or nil accordingly.

If optional argument DICT is a list of dictionaries or a single
dictionary, only save those.

If optional argument ASK is non-nil, ask for confirmation before
saving.

Optional argument COMPILATION determines whether to save the
dictionaries in compiled or uncompiled form. The default is to
save both forms. See `dictree-write'.

If optional argument FORCE is non-nil, save modified dictionaries
irrespective of their autosave flag.

If optional argument NO-FAIL-QUERY is non-nil, the user will not
be queried if a dictionary fails to save properly, and the return
value is always nil.

Interactively, FORCE is the prefix argument, and the user will not be
asked whether they wish to continue after a failed save."
  (interactive "P")

  ;; sort out arguments
  (when (and (called-interactively-p 'any) dict) (setq dict nil force t))
  (when (dictree-p dict) (setq dict (list dict)))

  ;; For each dictionary in list / each loaded dictionary, check if
  ;; dictionary has been modified. If so, save it if autosave is set or
  ;; FORCE is non-nil.
  (let (save-failures)
    (dolist (dic (if (null dict)
		     dictree-loaded-list
		   dict))
      (when (and (dictree-modified dic)
		 (or force (dictree-autosave dic))
		 (or (not ask)
		     (y-or-n-p (format "Save modified dictionary %s? "
				       (dictree-filename dic)))))
	(condition-case nil
	    (progn
	      (dictree-save dic compilation)
	      (setf (dictree-modified dic) nil))
	  (error (push dic save-failures)))))

    ;; prompt if dictionary saving failed
    (if save-failures
	(if (or (called-interactively-p 'any) no-fail-query)
	    (progn
	      (message
	       (concat
		"Error: failed to save the following modified "
		"dictionaries: "
		(mapconcat #'dictree--name save-failures ", ")))
	      nil)
	  (yes-or-no-p
	   (concat "Error: failed to save the following modified "
		   "dictionaries: "
		   (mapconcat #'dictree--name save-failures ", ")
		   "; continue anyway? ")))
      t)))


;; Add the dictree-save-modified function to the kill-emacs-hook to save
;; modified dictionaries when exiting emacs
(add-hook 'kill-emacs-query-functions 'dictree-save-modified)



;;;###autoload
(defun dictree-load (file)
  "Load a dictionary object from file FILE.
Returns the dictionary if successful, nil otherwise.

Interactively, FILE is read from the mini-buffer."
  (interactive (list (read-dict "Load dictionary: " nil nil t t)))

  ;; sort out dictionary name and file name
  (if (or (symbolp file) (dictree-p file))
      (message "Dictionary %s already loaded" (dictree-name file))

    ;; load the dictionary
    (if (not (load file t))
	;; if loading failed, throw error interactively, return nil
	;; non-interactively
	(if (called-interactively-p 'any)
	    (error "Cannot open dictionary file: %s" file)
	  nil)

      (let (dictname dict)
	(setq dictname
	      (file-name-nondirectory (file-name-sans-extension file))
	      dict (symbol-value (intern-soft dictname)))
	(if (not (dictree-p dict))
	    ;; if loading failed, throw error interactively, return nil
	    ;; non-interactively
	    (if (called-interactively-p 'any)
		(error "Error loading dictionary file: %s" file)
	      nil)

	  ;; ensure the dictionary name and file name associated with
	  ;; the dictionary match the file it was loaded from
	  (when (and (string= (file-name-nondirectory file) file)
		     (setq file
			   (locate-file file load-path load-suffixes)))
	    (setf (dictree-filename dict) file))
	  (setf (dictree-name dict) dictname)

	  ;; make sure the dictionary is in dictree-loaded-list
	  ;; (normally the lisp code in the dictionary itself should do
	  ;; this, but just to make sure...)
	  (unless (memq dict dictree-loaded-list)
	    (push dict dictree-loaded-list))
	  (message (format "Loaded dictionary %s" dictname))

	  ;; return dictionary
	  dict)))))



(defun dictree-unload (dict &optional dont-save)
  "Unload dictionary DICT.
If optional argument DONT-SAVE is non-nil, the dictionary will
NOT be saved even if its autosave flag is set.

Interactively, DICT is read from the mini-buffer, and DONT-SAVE
is the prefix argument."
  (interactive (list (read-dict "Dictionary: ")
		     current-prefix-arg))
  (when (and (called-interactively-p 'any) (symbolp dict))
    (setq dict (symbol-value dict)))

  ;; if dictionary has been modified, autosave is set and not overidden,
  ;; save it first
  (when (and (dictree-modified dict)
	     (null dont-save)
	     (or (eq (dictree-autosave dict) t)
		 (and (eq (dictree-autosave dict) 'ask)
		      (y-or-n-p
		       (format
			"Dictionary %s modified.\
 Save before unloading? "
			(dictree-name dict))))))
    (dictree-save dict))

  ;; if unloading a meta-dict, remove reference to it from constituent
  ;; dictionaries' meta-dict-list cell
  (when (dictree--meta-dict-p dict)
    (mapc
     (lambda (dic)
       (setf (dictree--meta-dict-list dic)
	     (delq dict (dictree--meta-dict-list dic))))
     (dictree--meta-dict-dictlist dict)))

  ;; remove dictionary from list of loaded dictionaries and unload it
  (setq dictree-loaded-list (delq dict dictree-loaded-list))
  ;; We used `unintern' here before, but that's too dangerous!
  (makunbound (dictree-name dict))
  (message "Dictionary %s unloaded" (dictree-name dict)))



(defun dictree--write-dict-code (dict dictname filename)
  ;; Write code for normal dictionary DICT to current buffer, giving it
  ;; the name DICTNAME and file FILENAME.
  (let (hashcode tmpdict tmptrie lookup-alist
	complete-alist regexp-alist fuzzy-match-alist fuzzy-complete-alist)

    ;; --- convert trie data ---
    ;; if dictionary doesn't use any custom save functions, write
    ;; dictionary's trie directly as is
    (setq tmptrie (dictree--trie dict))
    ;; otherwise, create a temporary trie and populate it with the
    ;; converted contents of the dictionary's trie
    (when (or (dictree--data-savefun dict)
	      (dictree--plist-savefun dict))
      (setq tmptrie
	    (trie-create-custom
	     (trie-comparison-function tmptrie)
	     :createfun (trie--createfun tmptrie)
	     :insertfun (trie--insertfun tmptrie)
	     :deletefun (trie--deletefun tmptrie)
	     :lookupfun (trie--lookupfun tmptrie)
	     :mapfun (trie--mapfun tmptrie)
	     :emptyfun (trie--emptyfun tmptrie)
	     :stack-createfun (trie--stack-createfun tmptrie)
	     :stack-popfun (trie--stack-popfun tmptrie)
	     :stack-emptyfun (trie--stack-emptyfun tmptrie)))
      (trie-mapc
       (lambda (key cell)
	 (trie-insert tmptrie key
		      (dictree--cell-create
		       (funcall (or (dictree--data-savefun dict)
				    #'identity)
				(dictree--cell-data cell))
		       (funcall (or (dictree--plist-savefun dict)
				    #'identity)
				(dictree--cell-plist cell)))))
       (dictree--trie dict))

      ;; generate code to convert contents of trie back to original form
      (setq hashcode
	    (concat
	     hashcode
	     " (trie-map\n"
	     "  (lambda (key cell)\n"
	     "     (dictree--cell-create\n"
	     (if (dictree--data-loadfun dict)
		 (concat
		  "(funcall (dictree--data-loadfun " dictname ")\n"
		  "         (dictree--cell-data cell))\n")
	       "   (dictree--cell-data cell)\n")
	     (if (dictree--plist-loadfun dict)
		 (concat
		  "(funcall (dictree--plist-loadfun " dictname ")\n"
		  "         (dictree--cell-plist cell))))\n")
	       "   (dictree--cell-plist cell)))\n")
	     " (dictree--trie " dictname "))\n")))


    ;; --- convert caches for writing to file ---
    ;; hash tables have no read syntax in older Emacsen, so we convert
    ;; them to alists for writing
    (unless (featurep 'hashtable-print-readable)
      ;; convert lookup cache hash table to alist, if it exists
      (when (dictree--lookup-cache dict)
	(maphash
	 (lambda (key val)
	   (push
	    (cons key
		  (cons (mapcar #'car (dictree--cache-results val))
			(dictree--cache-maxnum val)))
	    lookup-alist))
	 (dictree--lookup-cache dict))
	;; generate code to reconstruct the lookup hash table
	(setq hashcode
	      (concat
	       hashcode
	       "(let ((lookup-cache (make-hash-table :test #'equal))\n"
	       "      (trie (dictree--trie " dictname ")))\n"
	       "  (mapc\n"
	       "   (lambda (entry)\n"
	       "     (puthash\n"
	       "      (car entry)\n"
	       "      (dictree--cache-create\n"
	       "       (mapcar\n"
	       "        (lambda (key)\n"
	       "          (cons key (trie-member trie key)))\n"
	       "        (dictree--cache-results (cdr entry)))\n"
	       "       (dictree--cache-maxnum (cdr entry)))\n"
	       "      lookup-cache))\n"
	       "   (dictree--lookup-cache " dictname "))\n"
	       "  (setf (dictree--lookup-cache " dictname ")\n"
	       "        lookup-cache))\n")))

      ;; convert query caches, if they exist
      (dolist (cache-details
	       '((dictree--complete-cache complete-alist)
		 (dictree--regexp-cache regexp-alist)
		 (dictree--fuzzy-match-cache fuzzy-match-alist)
		 (dictree--fuzzy-complete-cache fuzzy-complete-alist)))
	(when (funcall (nth 0 cache-details) dict)
	  ;; convert hash table to alist
	  (set (nth 1 cache-details)
	       (let (alist)
		 (maphash
		  (lambda (key val)
		    (push
		     (cons key
			   (cons
			    (mapcar #'car (dictree--cache-results val))
			    (dictree--cache-maxnum val)))
		     alist))
		(funcall (nth 0 cache-details) dict))
		 alist))
	  ;; generate code to reconstruct hash table from alist
	  (setq
	   hashcode
	   (concat
	    hashcode
	    "(let ((cache (make-hash-table :test #'equal))\n"
	    "      (trie (dictree--trie " dictname ")))\n"
	    "  (mapc\n"
	    "   (lambda (entry)\n"
	    "     (puthash\n"
	    "      (car entry)\n"
	    "      (dictree--cache-create\n"
	    "       (mapcar\n"
	    "        (lambda (key)\n"
	    "          (cons key\n"
	    "                (trie-member\n"
	    "                 trie (if (stringp key) key (car key)))))\n"
	    "        (dictree--cache-results (cdr entry)))\n"
	    "       (dictree--cache-maxnum (cdr entry)))\n"
	    "      cache))\n"
	    "   (" (symbol-name (nth 0 cache-details)) " " dictname "))\n"
	    "  (setf (" (symbol-name (nth 0 cache-details)) " "
	    dictname ")\n"
	    "        cache))\n")))))


    ;; --- write to file ---
    ;; generate the structure to save
    (setq tmpdict (dictree--copy dict))
    (setf (dictree--trie tmpdict) tmptrie
	  (dictree--name tmpdict) dictname
	  (dictree--filename tmpdict) filename
	  (dictree--modified tmpdict) nil
	  (dictree--meta-dict-list tmpdict) nil)
    (unless (featurep 'hashtable-print-readable)
      (setf (dictree--lookup-cache tmpdict)         lookup-alist
	    (dictree--complete-cache tmpdict)       complete-alist
	    (dictree--regexp-cache tmpdict)         regexp-alist
	    (dictree--fuzzy-match-cache tmpdict)    fuzzy-match-alist
	    (dictree--fuzzy-complete-cache tmpdict) fuzzy-complete-alist))

    ;; write lisp code that generates the dictionary object
    (let ((print-circle t) (print-level nil) (print-length nil))
      (insert "(eval-when-compile (require 'cl))\n")
      (insert "(require 'dict-tree)\n")
      (insert "(defvar " dictname " nil \"Dictionary " dictname ".\")\n")
      (unwind-protect
	  (progn
	    ;; transform trie to print form
	    (trie-transform-for-print (dictree--trie tmpdict))
	    (insert "(setq " dictname
		    " '" (prin1-to-string tmpdict) ")\n"))
	;; if dictionary doesn't use any custom save functions, tmpdict's trie
	;; is identical to original dict, so transform it back to usable form
	;; on write error
	(unless (or (dictree--data-savefun dict)
		    (dictree--plist-savefun dict))
	  (trie-transform-from-read (dictree--trie tmpdict))))
      (insert "(trie-transform-from-read (dictree--trie "
	      dictname "))\n")
      (when hashcode (insert hashcode))
      (insert "(unless (memq " dictname " dictree-loaded-list)\n"
	      "  (push " dictname " dictree-loaded-list))\n"))))



(defun dictree--write-meta-dict-code (dict dictname filename)
  ;; Write code for meta-dictionary DICT to current buffer, giving it
  ;; the name DICTNAME and file FILENAME.
  (let (hashcode tmpdict lookup-alist
	complete-alist regexp-alist fuzzy-match-alist fuzzy-complete-alist)

    ;; --- convert caches for writing to file ---
    ;; hash tables have no read syntax in older Emacsen, so we convert
    ;; them to alists for writing
    (unless (featurep 'hashtable-print-readable)
      ;; convert lookup cache hash table to an alist, if it exists
      (when (dictree--meta-dict-lookup-cache dict)
	(maphash (lambda (key val)
		   (push (cons key (mapcar #'car val)) lookup-alist))
		 (dictree--meta-dict-lookup-cache dict))
	;; generate code to reconstruct the lookup hash table
	(setq hashcode
	      (concat
	       hashcode
	       "(let ((cache (make-hash-table :test #'equal)))\n"
	       "  (mapc (lambda (entry)\n"
	       "    (puthash (car entry) (cdr entry) cache))\n"
	       "    (dictree--meta-dict-lookup-cache " dictname "))\n"
	       "  (setf (dictree--meta-dict-lookup-cache " dictname ")\n"
	       "        cache))\n")))

      ;; convert query caches, if they exist
      (dolist (cache-details
	       '((dictree--meta-dict-complete-cache       complete-alist)
		 (dictree--meta-dict-regexp-cache         regexp-alist)
		 (dictree--meta-dict-fuzzy-match-cache    fuzzy-match-alist)
		 (dictree--meta-dict-fuzzy-complete-cache fuzzy-complete-alist)))
	(when (funcall (nth 0 cache-details) dict)
	  ;; convert hash table to alist
	  (set (nth 1 cache-details)
	       (let (alist)
		 (maphash (lambda (key val) (push (cons key val) alist))
			  (funcall (nth 0 cache-details) dict))
		 alist))
	  ;; generate code to reconstruct hash table from alist
	  (setq
	   hashcode
	   (concat
	    hashcode
	    "(let ((cache (make-hash-table :test #'equal)))\n"
	    "  (mapc (lambda (entry)\n"
	    "    (puthash (car entry) (cdr entry) cache))\n"
	    "    (" (symbol-name (nth 0 cache-details)) " "
	            dictname "))\n"
	    "  (setf (" (symbol-name (nth 0 cache-details)) " "
	                dictname ")\n"
	    "        cache))\n")))))


    ;; --- write to file ---
    ;; generate the structure to save
    (setq tmpdict (dictree--meta-dict-copy dict))
    (setf (dictree--meta-dict-name tmpdict) dictname
	  (dictree--meta-dict-filename tmpdict) filename
	  (dictree--meta-dict-modified tmpdict) nil
	  (dictree--meta-dict-meta-dict-list tmpdict) nil
	  (dictree--meta-dict-dictlist tmpdict)
	    (mapcar (lambda (dic) (intern (dictree-name dic)))
		    (dictree--meta-dict-dictlist dict)))
    (unless (featurep 'hashtable-print-readable)
      (setf (dictree--meta-dict-lookup-cache tmpdict)
	      lookup-alist
	    (dictree--meta-dict-complete-cache tmpdict)
	      complete-alist
	    (dictree--meta-dict-regexp-cache tmpdict)
	      regexp-alist
	    (dictree--meta-dict-fuzzy-match-cache tmpdict)
	      fuzzy-match-alist
	    (dictree--meta-dict-fuzzy-complete-cache tmpdict)
	      fuzzy-complete-alist))

    ;; write lisp code that generates the dictionary object
    (let ((print-circle t) (print-level nil) (print-length nil))
      (insert "(eval-when-compile (require 'cl))\n"
	      "(require 'dict-tree)\n")
      (mapc
       (lambda (dic)
	 (insert "(unless (dictree-load \"" (dictree-filename dic) "\")\n"
		 "        (error \"Failed to load dictionary \\\""
		 (dictree-name dic) "\\\" required by meta-dict \\\""
		 dictname "\\\"\"))\n"))
       (dictree--meta-dict-dictlist dict))
      (insert "(defvar " dictname " nil \"Dictionary " dictname ".\")\n"
	      "(setq " dictname " " (prin1-to-string tmpdict) ")\n"
	      "(setf (dictree--meta-dict-dictlist " dictname ")\n"
	      "      (mapcar #'eval (dictree--meta-dict-dictlist "
	                            dictname ")))\n")
      (when hashcode (insert hashcode))
      (insert "(unless (memq " dictname " dictree-loaded-list)"
	      " (push " dictname " dictree-loaded-list))\n"))))




;; ----------------------------------------------------------------
;;                Dumping and restoring contents

(defun dictree-populate-from-file
  (dict file
   &optional insert-function key-loadfun data-loadfun plist-loadfun
   balance)
  "Populate dictionary DICT from the key list in file FILE.

Each line of FILE should contain a key, either a string
\(delimited by \"\), a vector, or a list. (Use the escape
sequence \\\" to include a \" in a string.) If a line does not
contain a key, it is silently ignored.

Each line can optionally include data and a property list (in
that order) to be associated with the key. If present, these
should separated from each other and the key by whitespace.

INSERT-FUNCTION, KEY-LOAD-FUNCTION, DATA-LOAD-FUNCTION and
PLIST-LOAD-FUNCTION override the corresponding default functions
for DICT (see `dictree-create').

Interactively, DICT and FILE are read from the mini-buffer.


Technicalities:

The key, data and property list are read as lisp expressions
using `read'. The keys will be read from FILE in order, unless
BALANCE is non-nil, in which case they are read from the median
element outwards (which can help ensure efficient data structures
are created when using a trie that is not self-balancing, see
`dictree-create')."
  (interactive (list (read-dict "Dictionary: ")
		     (read-file-name "File to populate from: "
				     nil "" t)))
  (when (and (called-interactively-p 'any) (symbolp dict))
    (setq dict (symbol-value dict)))

  (if (and (called-interactively-p 'any) (string= file ""))
      (message "No file specified; dictionary %s NOT populated"
	       (dictree-name dict))

    (unless (dictree--meta-dict-p dict)
      (unless key-loadfun
	(setq key-loadfun (dictree--key-loadfun dict)))
      (unless data-loadfun
	(setq data-loadfun (dictree--data-loadfun dict)))
      (unless plist-loadfun
	(setq plist-loadfun (dictree--plist-loadfun dict))))

    (save-excursion
      (let ((buff (find-file-noselect file)))
	(set-buffer buff)

	;; insert the keys starting from the median to ensure a
	;; reasonably well-balanced tree
	(let* ((lines (count-lines (point-min) (point-max)))
	       (midpt (+ (/ lines 2) (mod lines 2)))
	       entry)
	  (message "Inserting keys in %s...(1 of %d)"
		   (dictree-name dict) lines)
	  ;; insert the median key and set the dictionary's modified
	  ;; flag
	  (if balance
	      (dictree--goto-line midpt)
	    (goto-char (point-min)))
	  (when (setq entry
		      (condition-case nil
			  (dictree--read-line key-loadfun data-loadfun
					      plist-loadfun)
			(error (error "Error reading line %d of %s"
				      midpt file))))
	    (dictree-insert dict (car entry) (nth 1 entry)
			    insert-function)
	    (setf (dictree--cell-plist
		   (dictree--lookup dict (car entry) nil))
		  (nth 2 entry)))
	  ;; insert keys successively further away from the median in
	  ;; both directions
	  (dotimes (i (1- (if balance midpt lines)))
	    (if balance
		(dictree--goto-line (+ midpt i 1))
	      (forward-line 1))
	    (when (setq entry
			(condition-case nil
			    (dictree--read-line key-loadfun data-loadfun
						plist-loadfun)
			  (error (error "Error reading line %d of %s"
					(+ midpt i 1) file))))
	      (dictree-insert dict (car entry) (nth 1 entry)
			      insert-function)
	      (setf (dictree--cell-plist
		     (dictree--lookup dict (car entry) nil))
		    (nth 2 entry)))
	    (when (= 49 (mod i 50))
	      (message "Inserting keys in %s...(%d of %d)"
		       (dictree-name dict)
		       (if balance (+ (* 2 i) 2) i)
		       lines))
	    (when balance
	      (dictree--goto-line (- midpt i 1))
	      (when (setq entry
			  (condition-case nil
			      (dictree--read-line key-loadfun data-loadfun
						  plist-loadfun)
			    (error (error "Error reading line %d of %s"
					  (- midpt i 1) file))))
		(dictree-insert dict (car entry)
				(nth 1 entry) insert-function)
		(setf
		 (dictree--cell-plist
		  (dictree--lookup dict (car entry) nil))
		 (nth 2 entry)))))

	  ;; if inserting from mid-point out, and file contains an even
	  ;; number of keys, we still have to add the last one
	  (when (and balance (= 0 (mod lines 2)))
	    (dictree--goto-line lines)
	    (when (setq entry
			(condition-case nil
			    (dictree--read-line key-loadfun data-loadfun
						plist-loadfun)
			  (error (error "Error reading line %d of %s"
					lines file))))
	      (dictree-insert dict (car entry) (nth 1 entry)
			      insert-function)
	      (setf (dictree--cell-plist
		     (dictree--lookup dict (car entry) nil))
		    (nth 2 entry))))

	  (message "Inserting keys in %s...done" (dictree-name dict)))
	(kill-buffer buff)))))



(defun dictree--read-line
  (&optional key-loadfun data-loadfun plist-loadfun)
  ;; Return a list containing the key, data (if any, otherwise nil) and
  ;; property list (ditto) at the current line of the current buffer.
  (save-excursion
    (let (key data plist)
      ;; read key
      (beginning-of-line)
      (when (setq key (read (current-buffer)))
	(when key-loadfun (setq key (funcall key-loadfun key)))
	;; if there's anything after the key, use it as data
	(unless (eq (line-end-position) (point))
	  (setq data (read (current-buffer))))
	(when data-loadfun (setq data (funcall data-loadfun data)))
	;; if there's anything after the data, use it as the property list
	(unless (eq (line-end-position) (point))
	  (setq plist (read (current-buffer))))
	(when plist-loadfun (funcall plist-loadfun plist))
	;; return what we've read
	(list key data plist)))))



(defun dictree-dump-to-buffer (dict &optional buffer type)
  "Dump keys and their associated data
from dictionary DICT to BUFFER, in the same format as that used
by `dictree-populate-from-file'. If BUFFER exists, data will be
appended to the end of it. Otherwise, a new buffer will be
created. If BUFFER is omitted, the current buffer is used.

TYPE determines the type of sequence to use to represent the
keys, and should be one of the symbols `string', `vector' or
`list'. The default is `vector'.

Note that if the data does not have a read syntax, the dumped
data can not be used to recreate the dictionary using
`dictree-populate-from-file'.

Interactively, DICT and BUFFER are read from the mini-buffer,
TYPE is always `string'."
  (interactive (list (read-dict "Dictionary: ")
		     (read-buffer
		      "Buffer to dump to (defaults to current): "
		      (buffer-name (current-buffer)))
		     'string))
  (when (and (called-interactively-p 'any) (symbolp dict))
    (setq dict (symbol-value dict)))

  ;; select the buffer, creating it if necessary
  (if buffer
      (setq buffer (get-buffer-create buffer))
    (setq buffer (current-buffer)))
  (set-buffer buffer)

  ;; move point to end of buffer and make sure it's at start of new line
  (goto-char (point-max))
  (unless (= (point) (line-beginning-position))
    (insert "\n"))

  ;; dump keys
  (message "Dumping keys from %s to %s..."
	   (dictree-name dict) (buffer-name buffer))
  (let ((count 0) (dictsize (dictree-size dict)))
    (message "Dumping keys from %s to %s...(key 1 of %d)"
	     (dictree-name dict) (buffer-name buffer) dictsize)

    ;; map dump function over dictionary
    (dictree--mapc
     (lambda (key data plist)
       (when (= 99 (mod count 100))
	 (message "Dumping keys from %s to %s...(key %d of %d)"
		  (dictree-name dict) (buffer-name buffer)
		  (1+ count) dictsize))
       (insert (prin1-to-string
		(funcall (or (dictree--key-savefun dict) #'identity)
			 key)))
       (when (setq data
		   (funcall (or (dictree--data-savefun dict) #'identity)
			    data))
	 (insert " " (prin1-to-string data)))
       (when (setq plist
		   (funcall (or (dictree--plist-savefun dict) #'identity)
			    plist))
	 (unless data (insert " nil"))
	 (insert " " (prin1-to-string plist)))
       (insert "\n")
       (setq count (1+ count)))
     dict type)  ; dictree-mapc target

    (message "Dumping keys from %s to %s...done"
	     (dictree-name dict) (buffer-name buffer)))
  (switch-to-buffer buffer))



(defun dictree-dump-to-file (dict filename &optional type overwrite)
  "Dump keys and their associated data
from dictionary DICT to a text file FILENAME, in the same format
as that used by `dictree-populate-from-file'. Prompts to overwrite
FILENAME if it already exists, unless OVERWRITE is non-nil.

TYPE determines the type of sequence to use to represent the
keys, and should be one of the symbols `string', `vector' or
`list'. The default is `vector'.

Note that if the data does not have a read syntax and no , the dumped
data can not be used to recreate the dictionary using
`dictree-populate-from-file'.

Interactively, DICT and FILE are read from the mini-buffer,
OVERWRITE is the prefix argument, and TYPE is always string."
  (interactive (list (read-dict "Dictionary: ")
		     (read-file-name "File to dump to: " nil "")))
  (when (and (called-interactively-p 'any) (symbolp dict))
    (setq dict (symbol-value dict)))

  (if (and (called-interactively-p 'any) (string= filename ""))
      (message "Dictionary %s NOT dumped" (dictree-name dict))

    ;; check if file exists, and prompt to overwrite it if necessary
    (if (and (file-exists-p filename)
	     (not overwrite)
	     (not (y-or-n-p
		   (format "File %s already exists. Overwrite? "
			   filename))))
	(message "Key dump cancelled")

      (let (buff)
	;; create temporary buffer, dump keys to it, and save to
	;; FILENAME
	(setq buff (generate-new-buffer filename))
	(save-window-excursion
	  (dictree-dump-to-buffer dict buff type)
	  (write-file filename))
	(kill-buffer buff)))))




;; ----------------------------------------------------------------
;;                     Minibuffer completion

(defvar dictree-history nil
  "History list for commands that read a dictionary name.")

(defvar dictree-loaded-history nil
  "History list for commands that read a loaded dictionary name.")


;;;###autoload
(defun read-dict
  (prompt &optional default dictlist allow-unloaded allow-unmatched)
  "Read the name of a dictionary with completion, and return it.

Prompt with PROMPT. By default, return DEFAULT. If DICTLIST is
supplied, only complete on dictionaries in that list.

If ALLOW-UNLOADED is non-nil, also complete on the names of
unloaded dictionaries (actually, on any Elisp file in the current
`load-path' restricted to subdirectories of your home directory
whose file name starts with \"dict-\"). If an unloaded dictionary
is read, the name of the Elisp file will be returned, without
extension, suitable for passing to `load-library'."

  (let (dictname paths)
    ;; when allowing unloaded dictionaries...
    (when allow-unloaded
      ;; get paths in load-path that are subdirectories of home
      ;; directory
      (dolist (d load-path)
	(when (eq (aref d 0) ?~) (push d paths)))
      ;; gather names of all Elisp libraries in this restricted
      ;; load-path
      (dolist (f (all-completions
		  "" (apply-partially #'locate-file-completion-table
				      paths (get-load-suffixes))))
	(when (and (null (file-name-directory f))
		   (and (> (length f) 5)
			(string= (substring f 0 5) "dict-"))
		   (null (file-name-extension f))
		   (not (member (file-name-sans-extension f) dictname)))
	  (push (file-name-sans-extension f) dictname))))
    ;; gather names of loaded dictionaries
    (mapc (lambda (dict)
	    (unless (or (null (dictree-name dict))
			(member (dictree-name dict) dictname))
	      (push (list (dictree-name dict)) dictname)))
	  (or dictlist dictree-loaded-list))
    ;; do completing-read
    (setq dictname (completing-read
		    prompt
		    (if allow-unmatched
			(completion-table-in-turn
			 dictname #'read-file-name-internal)
		      dictname)
		    nil (not allow-unmatched) nil
		    (if allow-unloaded
			'dictree-history
		      'dictree-loaded-history)
		    (and (dictree-p default) (dictree-name default))))
    ;; return dictionary
    (cond
     ;; if user typed a file name, return that
     ((and allow-unmatched (file-regular-p dictname)) dictname)
     ;; if user selected a loaded dictionary, return dict itself
     ((condition-case nil
	  (dictree-p (symbol-value (intern-soft dictname)))
	(void-variable nil))
      (intern-soft dictname))
     ;; if user selected an unloaded dictionary, return dict name
     ((and allow-unloaded (stringp dictname)) dictname)
     ;; if DEFAULT was specified, return that
     (default default)
     ;; should never get here!
     (t (error "Unknown error reading dictionary")))
    ))




;; ----------------------------------------------------------------
;;            Pretty-print dictionaries during edebug

;; We advise the `edebug-prin1' and `edebug-prin1-to-string' functions
;; (actually, aliases) so that they print "#<dict-tree NAME>" instead of
;; the full print form for dictionaries.
;;
;; This is because, if left to its own devices, edebug hangs for ages
;; whilst printing large dictionaries, and you either have to wait for a
;; *very* long time for it to finish, or kill Emacs entirely. (Even C-g
;; C-g fails!)
;;
;; We do this also for lists of dictionaries, since those occur quite
;; often, but not for other sequence types or deeper nested structures,
;; to keep the implementation as simple as possible.
;;
;; Since the print form of a dictionary is practically incomprehensible
;; anyway, we don't lose much by doing this. If you *really* want to
;; print dictionaries in full whilst edebugging, despite this warning,
;; disable the advice.


(eval-when-compile
  (require 'edebug)
  (require 'advice))

(defun dictree--prin1 (dict stream)
  (princ (concat "#<dict-tree \"" (dictree-name dict) "\""
		 (if (dictree--lookup-cache dict)
		     (concat " lookup "
			     (prin1-to-string
			      (hash-table-count
			       (dictree--lookup-cache dict))))
		   "")
		 (if (dictree--complete-cache dict)
		     (concat " complete "
			     (prin1-to-string
			      (hash-table-count
			       (dictree--complete-cache dict))))
		   "")
		 (if (dictree--regexp-cache dict)
		     (concat " regexp "
			     (prin1-to-string
			      (hash-table-count
			       (dictree--regexp-cache dict))))
		   "")
		 (if (dictree--fuzzy-match-cache dict)
		     (concat " fuzzy-match "
			     (prin1-to-string
			      (hash-table-count
			       (dictree--fuzzy-match-cache dict))))
		   "")
		 (if (dictree--fuzzy-complete-cache dict)
		     (concat " fuzzy-complete "
			     (prin1-to-string
			      (hash-table-count
			       (dictree--fuzzy-complete-cache dict))))
		   "")
		 ">")
	 stream))

(defun dictree--edebug-pretty-print (object)
  (cond
   ((dictree-p object)
    (concat "#<dict-tree \"" (dictree-name object) "\""
	    (if (dictree--lookup-cache object)
		(concat " lookup "
			(prin1-to-string
			 (hash-table-count
			  (dictree--lookup-cache object))))
	      "")
	    (if (dictree--complete-cache object)
		(concat " complete "
			(prin1-to-string
			 (hash-table-count
			  (dictree--complete-cache object))))
	      "")
	    (if (dictree--regexp-cache object)
		(concat " regexp "
			(prin1-to-string
			 (hash-table-count
			  (dictree--regexp-cache object))))
	      "")
	    (if (dictree--fuzzy-match-cache object)
		(concat " fuzzy-match "
			(prin1-to-string
			 (hash-table-count
			  (dictree--fuzzy-match-cache object))))
	      "")
	    (if (dictree--fuzzy-complete-cache object)
		(concat " fuzzy-complete "
			(prin1-to-string
			 (hash-table-count
			  (dictree--fuzzy-complete-cache object))))
	      "")
	    ">"))
   ((null object) "nil")
   ((let ((dlist object) (test t))
      (while (or (dictree-p (car-safe dlist))
		 (and dlist (setq test nil)))
	(setq dlist (cdr dlist)))
      test)
    (concat "(" (mapconcat (lambda (d)
			     (concat "#<dict-tree \""
				     (dictree-name d) "\">"))
			   object " ") ")"))
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
    (progn
      (cl-defmethod cl-print-object ((object dictree-) stream)
        (dictree--prin1 object stream))
      (cl-defmethod cl-print-object ((object dictree--meta-dict) stream)
        (dictree--prin1 object stream)))

  (when (fboundp 'ad-define-subr-args)
    (ad-define-subr-args 'edebug-prin1 '(object &optional printcharfun)))

  (defadvice edebug-prin1
      (around dictree activate compile preactivate)
    (let ((pretty (dictree--edebug-pretty-print object)))
      (if pretty
	  (progn
	    (prin1 pretty printcharfun)
	    (setq ad-return-value pretty))
        ad-do-it)))

  (when (fboundp 'ad-define-subr-args)
    (ad-define-subr-args 'edebug-prin1-to-string '(object &optional noescape)))

  (defadvice edebug-prin1-to-string
      (around dictree activate compile preactivate)
    (let ((pretty (dictree--edebug-pretty-print object)))
      (if pretty
	  (setq ad-return-value pretty)
        ad-do-it))))

;;;; ChangeLog:

;; 2017-08-17  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* dict-tree/dict-tree.el: Fix typo in Package-Requires.
;; 
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
;; 2012-09-11  Toby S. Cubitt  <tsc25@cantab.net>
;; 
;; 	Updated dict-tree.el package to version 0.12.8
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
;; 	Add dict-tree.el
;; 



(provide 'dict-tree)

;;; dict-tree.el ends here
