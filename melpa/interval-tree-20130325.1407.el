;;; interval-tree.el --- Interval tree data structure for 1D range queries

;; Copyright (C) 2013 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 24 Mar 2013
;; Version: 0.1
;; Package-Version: 20130325.1407
;; Package-Requires: ((dash "1.1.0"))
;; Keywords: extensions, data structure
;; URL: https://github.com/Fuco1/interval-tree

;; This file is not part of GNU Emacs.

;;; License:

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

;; This package implements a 1D range query datastructure, namely
;; interval tree. Use `interval-tree-build' to create the search
;; structure and `interval-tree-find-*' family of functions to query it.

;; See github readme at https://github.com/Fuco1/interval-tree

;;; Code:

(require 'dash)

;; interval structure
;; (:beg b :end e :value val)
(defun interval-create (beg end &optional val)
  "Create an interval from BEG to END.

Intervals can optionally hold a value VAL."
  (list :beg beg :end end :value val))

(defun interval-copy (interval)
  "Copy an interval INTERVAL.

This operation creates a shallow copy (the value is not
deep-copied)."
  (list :beg (interval-beginning interval)
        :end (interval-end interval)
        :value (interval-value interval)))

(defun interval-beginning (interval)
  "Return the beginning of INTERVAL."
  (plist-get interval :beg))

(defun interval-end (interval)
  "Return the end of INTERVAL."
  (plist-get interval :end))

(defun interval-value (interval)
  "Return the value of INTERVAL."
  (plist-get interval :value))

(defun interval-compare (int-a int-b)
  "Compare two intervals INT-A and INT-B by their beginning.

Return non-nil if beginning of INT-A < beginning of INT-B."
  (< (interval-beginning int-a) (interval-beginning int-b)))

(defun interval-compare-reverse (int-a int-b)
  "Compare two intervals INT-A and INT-B by their beginning.

Return non-nil if beginning of INT-A > beginning of INT-B."
  (> (interval-beginning int-a) (interval-beginning int-b)))

;; interval tree
;; (:left left-subtree
;;  :right right-subtree
;;  :intervals list-of-intervals-here
;;  :center integer-center)

(defun interval-tree-left (interval-tree)
  "Return the left subtree of INTERVAL-TREE."
  (plist-get interval-tree :left))

(defun interval-tree-right (interval-tree)
  "Return the right subtree of INTERVAL-TREE."
  (plist-get interval-tree :right))

(defun interval-tree-center (interval-tree)
  "Return the center value of INTERVAL-TREE.

Center is the midpoint of all the intervals contained in this node."
  (plist-get interval-tree :center))

(defun interval-tree-intervals (interval-tree)
  "Return intervals contained in this node of INTERVAL-TREE."
  (plist-get interval-tree :intervals))

(defun interval-tree-create (intervals &optional left right center)
  "Create the interval-tree node.

INTERVALS is a list of intervals contained in this node.

LEFT and RIGHT are left and right subtrees respectively.

CENTER is the midpoint of all the INTERVALS contained in this node."
  (list :left left :right right :intervals intervals :center (or center 0)))

(defun interval-tree-copy (interval-tree)
  "Copy an INTERVAL-TREE.

This creates a shallow copy of the node."
  (list :left (interval-tree-left interval-tree)
        :right (interval-tree-right interval-tree)
        :intervals (interval-tree-intervals interval-tree)
        :center (interval-tree-center interval-tree)))

(defun interval-tree-build (intervals &rest args)
  "Build the interval tree structure from a list of INTERVALS.

Accepts optional keyword arguments.

DEPTH is the maximum allowed depth of this tree.  Defaults to 32.

MINBUCKET is the number of intervals that can go in a single
node.  If the length of INTERVALS is less then this number, there
is no further splitting.  Defaults to 8.

LEFTEXTENT and RIGHTEXTENT are internal implementation arguments
and represent the left and right boundary of the INTERNAL.  This
information is used during the recursive building of the tree."
  (let ((depth (or (plist-get args :depth) 32))
        (minbucket (or (plist-get args :minbucket) 8))
        (maxbucket (or (plist-get args :maxbucket) 512))
        (leftextent (or (plist-get args :leftextend) 0))
        (rightextent (or (plist-get args :rightextend) 0)))
    (setq depth (1- depth))
    (if (or (= depth 0)
            (not (-drop minbucket intervals)))
        (interval-tree-create intervals)
      ;; this only happens with the initial call
      (when (and (= leftextent 0)
                 (= rightextent 0))
        (setq intervals (sort intervals 'interval-compare-reverse)))
      (let ((leftp 0)
            (rightp 0)
            (centerp 0)
            lefts rights ivals)
        (if (or (/= leftextent 0)
                (/= rightextent 0))
            (progn
              (setq leftp leftextent)
              (setq rightp rightextent))
          (setq leftp (interval-beginning (car (last intervals)))) ; O(n)
          (setq rightp (--reduce-from (max (interval-end it) acc) 0 intervals))) ; O(n)
        (setq centerp (/ (+ leftp rightp) 2))
        (--each intervals
          (cond
           ((< (interval-end it) centerp)       (!cons it lefts))
           ((> (interval-beginning it) centerp) (!cons it rights))
           (t                                   (!cons it ivals))))
        (interval-tree-create ivals
                              (and lefts
                                   (interval-tree-build lefts
                                                        :depth depth
                                                        :minbucket minbucket
                                                        :maxbucket maxbucket
                                                        :leftextend leftp
                                                        :rightextend centerp))
                              (and rights
                                   (interval-tree-build rights
                                                        :depth depth
                                                        :minbucket minbucket
                                                        :maxbucket maxbucket
                                                        :leftextend centerp
                                                        :rightextend rightp))
                              centerp)))))

(defun interval-tree-find-point (p itree)
  "Find all the intervals in ITREE that contain point P."
  (interval-tree-find-overlapping p p itree))

(defun interval-tree-find-overlapping (start stop itree)
  "Find all the intervals in ITREE that overlap with interval [start, stop]."
  (let ((r nil))
    (when (and (interval-tree-intervals itree)
               (not (< stop (interval-beginning (car (interval-tree-intervals itree))))))
      (--each (interval-tree-intervals itree)
        (when (and (>= (interval-end it) start)
                   (<= (interval-beginning it) stop))
          (!cons it r))))
    (when (and (interval-tree-left itree)
               (<= start (interval-tree-center itree)))
      (setq r (append r (interval-tree-find-overlapping start stop (interval-tree-left itree)))))
    (when (and (interval-tree-right itree)
               (>= stop (interval-tree-center itree)))
      (setq r (append r (interval-tree-find-overlapping start stop (interval-tree-right itree)))))
    r))

(defun interval-tree-find-contained (start stop itree)
  "Find all the intervals in ITREE that are contained in [start, stop]."
  (let ((r nil))
    (when (and (interval-tree-intervals itree)
               (not (< stop (interval-beginning (car (interval-tree-intervals itree))))))
      (--each (interval-tree-intervals itree)
        (when (and (<= (interval-end it) stop)
                   (>= (interval-beginning it) start))
          (!cons it r))))
    (when (and (interval-tree-left itree)
               (<= start (interval-tree-center itree)))
      (setq r (append r (interval-tree-find-contained start stop (interval-tree-left itree)))))
    (when (and (interval-tree-right itree)
               (>= stop (interval-tree-center itree)))
      (setq r (append r (interval-tree-find-contained start stop (interval-tree-right itree)))))
    r))

(provide 'interval-tree)

;;; interval-tree.el ends here
