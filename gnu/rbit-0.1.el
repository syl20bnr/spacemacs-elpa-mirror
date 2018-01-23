;;; rbit.el --- Red-black persistent interval trees  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: data structures, binary tree, intervals
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Self-balancing interval trees (for non-overlapping intervals, i.e.
;; similar to Emacs's text-properties (rather than overlays), but not
;; linked to any kind of buffer nor string).

;; Following Chris Okasaki's algorithm from
;; "Red-black trees in a functional setting", JFP'99.
;; https://dl.acm.org/citation.cfm?id=968578.968583&coll=DL&dl=GUIDE

;; The above article presents an elegant functional/persistent implementation
;; of insertion in red-black trees.  Here we have interval trees instead, so
;; it's a bit different and we support additional operations.  Those extensions
;; aren't nearly as well thought out as Chris's algorithm, so they actually
;; don't guarantee we preserve the 2 invariants of red-black trees :-(
;;
;; In practice, they should still usually give reasonably good algorithmic
;; properties, hopefully.
;;
;; For reference, the invariants are:
;;  1- a red node cannot have red children (local invariant).
;;  2- the left and right subtrees of a node must have the same black depth
;;     (global invariant).
;;
;; When breaking invariants, we strive to only break invariant 1, since it can
;; be more easily recovered later via local runtime checks, whereas detecting
;; invariant 2 breakage requires a complete tree traversal.

;;; Code:

(eval-when-compile (require 'cl-lib))


(cl-defstruct (rbit-tree
               (:conc-name rbit--)
               (:constructor nil)
               (:constructor rbit--node
                (black beg end val left right
                 &aux
                 ;; Invariant 1 occasionally broken!
                 ;; (_ (cl-assert
                 ;;     (or black
                 ;;         (and (or (null left) (rbit--black left))
                 ;;              (or (null right) (rbit--black right))))))
                 (_ (cl-assert (natnump black)))
                 (_ (cl-assert (< beg end)))
                 (_ (cl-assert ;; Don't really care if red nodes are balanced.
                     (or (not (or (> black 0) (and left right)))
                         (= (rbit--bdepth left) (rbit--bdepth right)))))
                 (_ (cl-assert (or (null left) (<= (rbit-max left) beg))))
                 (_ (cl-assert (or (null right) (>= (rbit-min right) end))))))
               (:copier nil))
  black         ;nil for red nodes, a natnum (the "blackness") for black nodes.
  beg end val left right)

(defun rbit--get (tree x)
  (when tree
    (while
        (cond
         ((< x (rbit--beg tree)) (setq tree (rbit--left tree)))
         ((>= x (rbit--end tree)) (setq tree (rbit--right tree)))))
    tree))

(defun rbit-get (tree x &optional default)
  "Return the value stored in TREE at position X.
For an interval BEG..END we consider that it means [BEG, END[
i.e. BEG is inside the interval, but END is not.
If X is not inside an interval in TREE, return default."
  (let ((node (rbit--get tree x)))
    (if node (rbit--val node) default)))

(defun rbit-member (tree x)
  "Return non-nil if X is within one of TREE's intervals."
  (not (not (rbit--get tree x))))

(defun rbit--make-top (node)
  (when node
    (if (> (rbit--black node) 0) node (rbit--blacken node 1))))

(defun rbit--blacken (node by)
  (cl-assert (>= by 0))
  (if (zerop by) node
    (rbit--node (+ (rbit--black node) by)
                (rbit--beg node) (rbit--end node) (rbit--val node)
                (rbit--left node) (rbit--right node))))

(defun rbit--redden (node by)
  (if (zerop by) node
    (let ((nblack (- (rbit--black node) by)))
      (cl-assert (>= nblack 0))
      (rbit--node nblack
                  (rbit--beg node) (rbit--end node) (rbit--val node)
                  (rbit--left node) (rbit--right node)))))

(defun rbit--make (black beg end val left right)
  "Make a new tree node, rebalancing locally along the way as needed."
  ;; Algorithmic properties: if left&right both have bdepth of N, then
  ;; the result has bdepth of "N + black".
  (if (zerop black)
      (or (when (and left right)
            (let ((lblack (rbit--black left))
                  (rblack (rbit--black right)))
              (or (when (and (> lblack 1) (>= lblack rblack))
                    (rbit--node rblack
                                (rbit--beg right) (rbit--end right)
                                (rbit--val right)
                                (rbit--make 0 beg end val
                                            (rbit--redden left rblack)
                                            (rbit--left right))
                                (rbit--right right)))
                  (when (> rblack 1)
                    (cl-assert (>= rblack lblack))
                    (rbit--node lblack
                                (rbit--beg left) (rbit--end left)
                                (rbit--val left)
                                (rbit--left left)
                                (rbit--make 0 beg end val
                                            (rbit--right left)
                                            (rbit--redden right lblack)))))))
          (rbit--node black beg end val left right))
    (or (if (null left)
            (when (and right (> (rbit--black right) 0))
              ;; blackness is >1 to preserve bdepth.  This can be used
              ;; elsewhere as a signal that this subtree is shallower
              ;; than expected (and hence needs rebalancing).
              (rbit--make (+ black (rbit--black right))
                          (rbit--beg right) (rbit--end right) (rbit--val right)
                          (rbit--node 0 beg end val left (rbit--left right))
                          (rbit--right right)))
          (when (zerop (rbit--black left))
            (let ((ll (rbit--left left))
                  (lr (rbit--right left)))
              (cond
               ((and ll (zerop (rbit--black ll)))
                (rbit--node 0
                            (rbit--beg left) (rbit--end left) (rbit--val left)
                            (rbit--blacken ll black)
                            (rbit--node black beg end val lr right)))
               ((and lr (zerop (rbit--black lr)))
                (rbit--node
                 0 (rbit--beg lr) (rbit--end lr) (rbit--val lr)
                 (rbit--node black
                             (rbit--beg left) (rbit--end left) (rbit--val left)
                             ll (rbit--left lr))
                 (rbit--node black beg end val (rbit--right lr) right)))))))
        (if (null right)
            (when (and left (> (rbit--black left) 0))
              (rbit--make (+ black (rbit--black left))
                          (rbit--beg left) (rbit--end left) (rbit--val left)
                          (rbit--left left)
                          (rbit--node 0 beg end val
                                      (rbit--right left) right)))
          (when (zerop (rbit--black right))
            (let ((rl (rbit--left right))
                  (rr (rbit--right right)))
              (cond
               ((and rr (zerop (rbit--black rr)))
                (rbit--node 0 (rbit--beg right) (rbit--end right)
                            (rbit--val right)
                            (rbit--node black beg end val left rl)
                            (rbit--blacken rr black)))
               ((and rl (zerop (rbit--black rl)))
                (rbit--node
                 0 (rbit--beg rl) (rbit--end rl) (rbit--val rl)
                 (rbit--node black beg end val left (rbit--left rl))
                 (rbit--node black (rbit--beg right) (rbit--end right)
                             (rbit--val right)
                             (rbit--right rl) rr)))))))
        (rbit--node black beg end val left right))))

(defun rbit--set (tree beg end val f)
  ;; Algorithmic properties: if tree has bdepth of N, then the result should
  ;; have bdepth of N as well (potentially with 2 red nodes at the root).
  (cl-assert (< beg end))
  (if (null tree) (rbit--node 0 beg end val nil nil)
    (let ((tbeg (rbit--beg tree))
          (tend (rbit--end tree)))
      (cond
       ((<= end tbeg)
        (rbit--make (rbit--black tree) tbeg tend (rbit--val tree)
                    (rbit--set (rbit--left tree) beg end val f)
                    (rbit--right tree)))
       ((<= tend beg)
        (rbit--make (rbit--black tree) tbeg tend (rbit--val tree)
                    (rbit--left tree)
                    (rbit--set (rbit--right tree) beg end val f)))
       ;; beg..end intersects with the root of tree.
       (t
        ;; FIXME: Here we don't actually guarantee the result is balanced!!
        ;; More specifically `rbit--make' can handle 2 reds along "the" spine,
        ;; but here we can cause situations where 2 reds appear
        ;; on "several spines" at the same time!
        ;; Hopefully this won't be too problematic in practice!
        (let ((left (rbit--left tree))
              (right (rbit--right tree))
              (nval (if f (funcall f val (rbit--val tree)) val)))
          (if (null f)
              (progn
                ;; Coalesce the sub-intervals.
                ;; FIXME: rbit--remove may not preserve bdepth!
                (when (< beg tbeg)
                  (setq left (rbit--remove left beg tbeg))
                  (setq tbeg beg))
                (when (< tend end)
                  (setq right (rbit--remove right tend end))
                  (setq tend end)))
            (when (< beg tbeg)
              (setq left (rbit--set left beg tbeg val f)))
            (when (< tend end)
              (setq right (rbit--set right tend end val f))))
          (cond
           ((and (< tbeg beg) (< end tend))
            (rbit--make (rbit--black tree) beg end nval
                        (rbit--set left tbeg beg (rbit--val tree) f)
                        (rbit--set right end tend (rbit--val tree) f)))
           ((< tbeg beg)
            (rbit--make (rbit--black tree) beg tend nval
                        (rbit--set left tbeg beg (rbit--val tree) f)
                        right))
           ((< end tend)
            (rbit--make (rbit--black tree) tbeg end nval
                        left
                        (rbit--set right end tend (rbit--val tree) f)))
           (t
            (rbit--make (rbit--black tree) tbeg tend nval
                        left right)))))))))

(defun rbit-set (tree beg end val &optional f)
  "Set the value of TREE to VAL between BEG and END.
If TREE already had values between BEG and END and F is non-nil,
then F is called with 2 arguments (VAL and the previous value) to
compute the resulting value."
  (rbit--make-top (rbit--set tree beg end val f)))

(defun rbit--balanced-p (tree)
  "Return black depth iff TREE obeys the red-black tree invariants."
  (if (null tree) 0
    (let ((dl (rbit--balanced-p (rbit--left tree)))
          (dr (rbit--balanced-p (rbit--right tree))))
      (cond
       ((and (numberp dl) (numberp dr))
        (if (not (= (abs dl) (abs dr)))
            'unbalanced
          (if (zerop (rbit--black tree))
              (if (or (< dl 0) (< dr 0)) 'double-red (- dl))
            (+ (rbit--black tree) (abs dl)))))
       (t (list dl dr))))))

(defun rbit--bdepth (tree)
  "Return black depth of TREE."
  (if (null tree) 0
    (let ((blackness (rbit--black tree)))
      (if (zerop blackness)
          ;; We don't really care if red nodes are balanced.
          (max (rbit--bdepth (rbit--left tree))
               (rbit--bdepth (rbit--right tree)))
        (+ blackness (rbit--bdepth (rbit--left tree)))))))

(defun rbit-do (f tree)
  "Call F on each interval in TREE.
F is called with 3 args: BEG, END, and VAL.  Its return value is ignored."
  (when tree
    (rbit-do f (rbit--left tree))
    (funcall f (rbit--beg tree) (rbit--end tree) (rbit--val tree))
    (rbit-do f (rbit--right tree))))

(defun rbit--concat (tree1 tree2)
  "Concat two trees of the same black-depth and which do not overlap."
  ;; Algorithmic properties: if `tree1' and `tree2' both have bdepth of N, then
  ;; the result should have bdepth of N as well.
  (cond
   ((null tree1) tree2)
   ((null tree2) tree1)
   (t
    (cl-assert (<= (rbit--end tree1) (rbit--beg tree2)))
    (let ((black1 (rbit--black tree1))
          (black2 (rbit--black tree2)))
      (if (<= black1 black2)
          (rbit--make black1
                      (rbit--beg tree1) (rbit--end tree1) (rbit--val tree1)
                      (rbit--left tree1)
                      (rbit--concat (rbit--right tree1)
                                    (rbit--redden tree2 black1)))
        (rbit--make black2
                    (rbit--beg tree2) (rbit--end tree2) (rbit--val tree2)
                    (rbit--concat (rbit--redden tree1 black2)
                                  (rbit--left tree2))
                    (rbit--right tree2)))))))

(defun rbit--remove (tree beg end)
  "Return TREE where BEG..END has been removed."
  ;; FIXME: `rbit--make' is designed to rebalance after an insertion,
  ;; not a deletion, so there's again no guarantee of balance.
  ;; Algorithmic properties: if `tree1' and `tree2' both have bdepth of N,
  ;; then the result should also have bdepth of N (unless it's nil).
  (when tree
    (let ((tbeg (rbit--beg tree))
          (tend (rbit--end tree))
          (left (rbit--left tree))
          (right (rbit--right tree)))
      (cond
       ((<= end tbeg)
        (let ((nleft (rbit--remove left beg end)))
          (rbit--make (if (or nleft right)
                          (rbit--black tree)
                        ;; Preserve bdepth!
                        (+ (rbit--bdepth left) (rbit--black tree)))
                      tbeg tend (rbit--val tree)
                      nleft
                      right)))
       ((<= tend beg)
        (let ((nright (rbit--remove right beg end)))
          (rbit--make (if (or left nright)
                          (rbit--black tree)
                        ;; Preserve bdepth!
                        (+ (rbit--bdepth right) (rbit--black tree)))
                      tbeg tend (rbit--val tree)
                      left
                      nright)))
       (t
        ;; beg..end intersects with the root of tree.
        (when (< beg tbeg)
          (setq left (rbit--remove left beg tbeg)))
        (when (< tend end)
          (setq right (rbit--remove right tend end)))
        (let ((black (rbit--black tree))
              ;; Non-nil if we lost bdepth info.
              (lost (and (not (or left right))
                         (or (rbit--left tree) (rbit--right tree))))
              (res
               (cond
                ((and (< tbeg beg) (< end tend))
                 (rbit--concat (rbit--set left tbeg beg (rbit--val tree) nil)
                               (rbit--set right end tend (rbit--val tree) nil)))
                ((< tbeg beg)
                 (rbit--concat (rbit--set left tbeg beg (rbit--val tree) nil)
                               right))
                ((< end tend)
                 (rbit--concat left
                               (rbit--set right end tend (rbit--val tree) nil)))
                (t
                 (rbit--concat left right)))))
          (if (and res (or (> black 0) lost))
              ;; Preserve bdepth!
              (rbit--blacken
               res (+ black (if (not lost) 0
                              (rbit--bdepth (or (rbit--left tree)
                                                (rbit--right tree))))))
            res)))))))

(defun rbit-remove (tree beg end)
  "Remove values between BEG and END from TREE."
  (rbit--make-top (rbit--remove tree beg end)))

(defun rbit-clip (tree beg end)
  "Return the part of TREE included in BEG..END."
  ;; FIXME: We don't make any effort trying to return a balanced tree :-(
  (when tree
    (let ((tbeg (rbit--beg tree))
          (tend (rbit--end tree)))
      (cond
       ((<= end tbeg) (rbit-clip (rbit--left tree) beg end))
       ((<= tend beg) (rbit-clip (rbit--right tree) beg end))
       (t
        (let* ((left (rbit--left tree))
               (right (rbit--right tree))
               (nleft (if (< beg tbeg) (rbit-clip left beg end)))
               (nright (if (< tend end) (rbit-clip right beg end))))
          (if (and (<= beg tbeg) (<= tend end)
                   (eq left nleft) (eq right nright))
              tree
            (rbit--make (rbit--black tree) (max beg tbeg) (min end tend)
                        (rbit--val tree) nleft nright))))))))

(defun rbit-min (tree)
  "Return the smallest key of TREE, if any."
  (when tree (or (rbit-min (rbit--left tree)) (rbit--beg tree))))

(defun rbit-max (tree)
  "Return the largest key of TREE, if any."
  (when tree (or (rbit-max (rbit--right tree)) (rbit--end tree))))

(defun rbit-to-list (tree)
  "Return a list of intervals, by increasing order.
Each interval is represented as (BEG END VAL)."
  (let ((res ()))
    (rbit-do (lambda (&rest args) (push args res)) tree)
    (nreverse res)))

(defun rbit-from-list (intervals &optional f)
  "Construct a tree from a list of intervals.
In case of overlap, the later intervals take precedence
or are combined with F."
  (let ((it nil))
    (pcase-dolist (`(,beg ,end ,val) intervals)
      (setq it (rbit-set it beg end val f)))
    it))

(ert-deftest rbit--test-set-1 ()
  (let ((c (lambda (x y)
             (cons x (if (listp y) y (list y))))))
    (should (equal (rbit-to-list
                    (rbit-from-list '((0 5 v0-5) (1 2 v1-2) (2 4 v2-4))))
                   '((0 1 v0-5) (1 2 v1-2) (2 4 v2-4) (4 5 v0-5))))
    (should (equal (rbit-to-list
                    (rbit-from-list '((1 2 v1-2) (2 4 v2-4) (0 5 v0-5))))
                   '((0 5 v0-5))))
    (should (equal (rbit-to-list
                    (rbit-from-list '((0 5 v0-5) (1 2 v1-2) (2 4 v2-4)) c))
                   '((0 1 v0-5) (1 2 (v1-2 v0-5))
                     (2 4 (v2-4 v0-5)) (4 5 v0-5))))
    (should (equal (rbit-to-list
                    (rbit-from-list '((1 2 v1-2) (2 4 v2-4) (0 5 v0-5)) c))
                   '((0 1 v0-5) (1 2 (v0-5 v1-2))
                     (2 4 (v0-5 v2-4)) (4 5 v0-5))))))

(ert-deftest rbit--test-clip-1 ()
  (should (equal (rbit-to-list
                  (rbit-clip
                   (rbit-from-list '((0 5 v0-5) (1 2 v1-2) (2 4 v2-4)))
                   1 4))
                 '((1 2 v1-2) (2 4 v2-4))))
  (should (equal (rbit-to-list
                  (rbit-clip
                   (rbit-from-list '((0 5 v0-5) (1 2 v1-2) (2 4 v2-4)
                                     (10 20 A) (12 15 B) (16 17 C)))
                   0 4))
                 '((0 1 v0-5) (1 2 v1-2) (2 4 v2-4)))))

;;;; ChangeLog:

;; 2018-01-22  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* rbit.el: New package
;; 


(provide 'rbit)
;;; rbit.el ends here
