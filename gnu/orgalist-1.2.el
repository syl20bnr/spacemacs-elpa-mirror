;;; orgalist.el --- Manage Org-like lists in non-Org buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2018  Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <mail@nicolasgoaziou.fr>
;; Maintainer: Nicolas Goaziou <mail@nicolasgoaziou.fr>
;; Keywords: convenience
;; Package-Requires: ((emacs "24.4"))
;; Version: 1.2

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

;; This library provides Org mode's plain lists in non-Org buffers, as
;; a minor mode.

;; More specifically, it supports syntax for numbered, unnumbered,
;; description items, checkboxes, and counter cookies.  Examples of
;; such constructs include:

;;     - item 1
;;     - item 2
;;       - [ ] checbox sub-item

;; or

;;     1. item 1
;;     2. item 2
;;        1. sub-item 1
;;        8. [@8] sub-item 8

;; and

;;     - term :: definition
;;     - term 2 :: definition 2

;; To start a list, type "- <SPC>" or "1 . <SPC>", then write the
;; contents of the item.  To create a new item, use M-<RET>.  If it
;; should be a child of the previous item, use <TAB> or M-<RIGHT>.
;; For example, "- <SPC> i t e m M-<RET> <TAB> c h i l d" produces:

;;     - item
;;       - child

;; See (info "(org) Plain Lists") and (info "(org) Checkboxes") for
;; more details about the syntax of such constructs.

;; The following features are supported:

;; - Item insertion (M-<RET>)
;; - Navigation (M-<UP>, M-<DOWN>)
;; - Indentation (M-<LEFT>, M-<RIGHT>, M-S-<LEFT>, M-S-<RIGHT>, <TAB>)
;; - Re-ordering (M-S-<UP>, M-S-<DOWN>)
;; - Toggling checkboxes (C-c C-c)
;; - Cycling bullets (C-c -)
;; - Sorting items (C-c ^)

;; The minor mode also supports filling and auto filling, when Auto
;; Fill mode is enabled.

;; Note that the bindings above are only available when point is in an
;; item (for M-<RET>, M-<UP>, M-<DOWN>) or exactly at an item.

;; The library also implements radio lists:

;; Call the `orgalist-insert-radio-list' function to insert a radio list
;; template in HTML, LaTeX, and Texinfo mode documents.  Sending and
;; receiving radio lists works is the same as for radio tables (see
;; Org manual for details) except for these differences:

;; - Orgalist minor mode must be active;
;; - Use the "ORGLST" keyword instead of "ORGTBL";
;; - `M-x orgalist-send-list' works only on the first list item.

;; Built-in translator functions are: `org-list-to-latex',
;; `org-list-to-html' and `org-list-to-texinfo'.  They use the
;; `org-list-to-generic' translator function.  See its documentation for
;; parameters for accurate customizations of lists.  Here is a LaTeX
;; example:

;;   % BEGIN RECEIVE ORGLST to-buy
;;   % END RECEIVE ORGLST to-buy
;;   \begin{comment}
;;
;;   #+ORGLST: SEND to-buy org-list-to-latex
;;   - a new house
;;   - a new computer
;;     + a new keyboard
;;     + a new mouse
;;   - a new life
;;   \end{comment}

;; `M-x orgalist-send-list' on "a new house" inserts the translated
;; LaTeX list in-between the "BEGIN RECEIVE" and "END RECEIVE" marker
;; lines.

;;; Code:

(require 'easymenu)
(require 'org-macs)
(require 'org-list)

(defvar mail-header-separator)
(defvar message-signature-separator)
(defvar orgalist-mode)


;;; Configuration variables

(defgroup orgalist nil
  "Manage plain lists in non-Org buffers."
  :tag "Orgalist"
  :group 'org)

(defcustom orgalist-context-function
  '((message-mode . orgalist-message-mode-context))
  "Alist between major modes and list context functions.
A list context function determines the boundaries of the buffer
that can contain an Org list.  When no major mode is found, or the
context function returns nil, consider the whole buffer."
  :group 'orgalist
  :type '(repeat
          (list (symbol :tag "Major mode")
                (function :tag "Function"))))

(defcustom orgalist-ordered-checkboxes nil
  "When non-nil, only tick checkboxes in order.
In this case, a checkbox can only be checked when every checkbox
before it is checked too."
  :group 'orgalist
  :type 'boolean
  :safe #'booleanp)

(defcustom orgalist-radio-list-templates
  '((latex-mode "% BEGIN RECEIVE ORGLST %n
% END RECEIVE ORGLST %n
\\begin{comment}
#+ORGLST: SEND %n org-list-to-latex
-
\\end{comment}\n")
    (texinfo-mode "@c BEGIN RECEIVE ORGLST %n
@c END RECEIVE ORGLST %n
@ignore
#+ORGLST: SEND %n org-list-to-texinfo
-
@end ignore\n")
    (html-mode "<!-- BEGIN RECEIVE ORGLST %n -->
<!-- END RECEIVE ORGLST %n -->
<!--
#+ORGLST: SEND %n org-list-to-html
-
-->\n"))
  "Templates for radio lists in different major modes.
All occurrences of %n in a template will be replaced with the name of the
list, obtained by prompting the user."
  :group 'orgalist
  :type '(repeat
          (list (symbol :tag "Major mode")
                (string :tag "Format"))))


;;; Mode specific context functions

(defun orgalist-message-mode-context ()
  "Return boundaries of message body if point is in body.
Otherwise, return nil."
  (save-excursion
    (cond ((re-search-backward message-signature-separator nil t)
           ;; After signature.
           (cons (line-beginning-position 2) (point-max)))
          ((re-search-backward
            (concat "^" (regexp-quote mail-header-separator) "\n") nil t)
           (goto-char (match-end 0))
           (cons (point)
                 (if (re-search-forward message-signature-separator nil t)
                     (match-beginning 0)
                   (point-max))))
          ((re-search-forward
            (concat "^" (regexp-quote mail-header-separator) "\n") nil t)
           ;; In header.
           nil)
          (t nil))))


;;; Internal variables

(defvar orgalist--menu nil
  "The Orgalist menu.")

(defconst orgalist--item-re
  (concat
   "^[ \t]*\\(\\(?:[-+]\\|\\(?:[0-9]+\\|[A-Za-z]\\)\\.\\)\\(?:[ \t]+\\|$\\)\\)"
   "\\(?:\\[@\\([0-9]+\\|[A-Za-z]\\)\\][ \t]*\\)?"
   "\\(?:\\(\\[[- xX]\\]\\)\\(?:[ \t]+\\|$\\)\\)?"
   "\\(?:\\(.*\\)[ \t]+::\\(?:[ \t]+\\|$\\)\\)?")
  "Match a list item and puts everything into groups:
group 1: bullet
group 2: counter
group 3: checkbox
group 4: description tag")

(defvar-local orgalist--cycling-state nil
  "Current cycling state when cycling indentation.")


;;; Internal functions

(defun orgalist--call-in-item (fun pos &rest arguments)
  "Call function FUN with buffer narrowed to item starting at POS.
Call function with ARGUMENTS.  Return the value FUN returns."
  (let* ((struct (save-excursion (goto-char pos) (orgalist--struct)))
         (next (or (org-list-has-child-p pos struct)
                   (org-list-get-item-end pos struct)))
         (fill-prefix
          (make-string (+ (length (org-list-get-bullet pos struct))
                          (org-list-get-ind pos struct))
                       ?\s)))
    (save-restriction
      (narrow-to-region pos next)
      (apply fun arguments))))

(defun orgalist--boundaries ()
  "Return buffer boundaries, as a cons cell, where lists are acceptable.
Return nil if Orgalist mode is not active."
  (and orgalist-mode
       (let ((f (cdr (assq major-mode orgalist-context-function))))
         (or (and (functionp f) (funcall f))
             (cons (point-min) (point-max))))))

(defun orgalist--at-item-p ()
  "Non-nil if point is at an item."
  (and (orgalist--boundaries)            ;check context
       (save-excursion
         (beginning-of-line)
         (looking-at-p
          "[ \t]*\\(?:[-+]\\|\\(?:[a-zA-Z]\\|[0-9]+\\)\\.\\)\\([ \t]\\|$\\)"))))

(defun orgalist--in-item-p ()
  "Return item beginning position when in a plain list, nil otherwise."
  (let ((boundaries (orgalist--boundaries)))
    (when boundaries
      (save-excursion
        (beginning-of-line)
        (let ((lim-up (car (orgalist--boundaries)))
              ;; Indentation isn't meaningful when point starts at an
              ;; empty line .
              (ind-ref (if (looking-at-p "^[ \t]*$") 10000
                         (current-indentation))))
          (if (looking-at orgalist--item-re) (point)
            ;; Detect if cursor in the middle of blank lines after a list.
            (let ((end-bounds nil))
              (when (and (setq end-bounds (org-in-regexp "^[ \t]*\n[ \t]*\n" 2))
                         (>= (point) (car end-bounds))
                         (< (point) (cdr end-bounds)))
                (goto-char (car end-bounds))
                (forward-line -1)))
            ;; Look for an item, less indented that reference line.
            (catch 'exit
              (while t
                (let ((ind (current-indentation)))
                  (cond
                   ;; This is exactly what we want.
                   ((and (looking-at orgalist--item-re) (< ind ind-ref))
                    (throw 'exit (point)))
                   ;; At upper bound of search or looking at the end
                   ;; of a previous list: search is over.
                   ((<= (point) lim-up) (throw 'exit nil))
                   ((looking-at "^[ \t]*\n[ \t]*\n") (throw 'exit nil))
                   ;; Skip blank lines.
                   ((looking-at "^[ \t]*$") (forward-line -1))
                   ;; Text at column 0 cannot belong to a list: stop.
                   ((= 0 ind) (throw 'exit nil))
                   ;; Normal text less indented than reference line,
                   ;; take it as new reference.
                   ((< ind ind-ref)
                    (setq ind-ref ind)
                    (forward-line -1))
                   (t (forward-line -1))))))))))))

(defun orgalist--struct ()
  "Return structure of list at point.

A list structure is an alist where key is point at item, and
values are:

1. indentation,
2. bullet with trailing white space,
3. bullet counter, if any,
4. checkbox, if any,
5. description tag, if any,
6. position at item end.

Thus the following list, where numbers in parens are
line beginning positions:

- [X] first item                             (1)
  1. sub-item 1                              (18)
  5. [@5] sub-item 2                         (34)
  some other text belonging to first item    (55)
- last item                                  (97)
  + tag :: description                       (109)
                                             (131)
gets the following structure:

 ((1 0 \"- \"  nil \"[X]\" nil 97)
  (18 2 \"1. \"  nil nil nil 34)
  (34 2 \"5. \" \"5\" nil nil 55)
  (97 0 \"- \"  nil nil nil 131)
  (109 2 \"+ \" nil nil \"tag\" 131))

Assume point is at an item."
  (save-excursion
    (beginning-of-line)
    (pcase-let* ((`(,lim-up . ,lim-down) (orgalist--boundaries))
                 (text-min-ind 10000)
                 (beg-cell (cons (point) (current-indentation)))
                 (itm-lst nil)
                 (itm-lst-2 nil)
                 (end-lst nil)
                 (end-lst-2 nil)
                 (struct nil)
                 (assoc-at-point
                  ;; Return association at point.
                  (lambda (ind)
                    (looking-at orgalist--item-re)
                    (let ((bullet (match-string-no-properties 1)))
                      (list (point)
                            ind
                            bullet
                            (match-string-no-properties 2) ;counter
                            (match-string-no-properties 3) ;checkbox
                            ;; Description tag.
                            (and (string-match-p "[-+*]" bullet)
                                 (match-string-no-properties 4))))))
                 (end-before-blank
                  (lambda ()
                    ;; Ensure list ends at the first blank line.
                    (skip-chars-backward " \r\t\n")
                    (min (line-beginning-position 2) lim-down))))
      ;; Read list from starting item to its beginning, and save top
      ;; item position and indentation in BEG-CELL. Also store ending
      ;; position of items in END-LST.
      (save-excursion
        (catch 'exit
          (while t
            (let ((ind (current-indentation)))
              (cond
               ((<= (point) lim-up)
                ;; At upward limit: if we ended at an item, store it,
                ;; else dismiss useless data recorded above BEG-CELL.
                ;; Jump to part 2.
                (throw 'exit
                       (setq itm-lst
                             (if (not (looking-at orgalist--item-re))
                                 (memq (assq (car beg-cell) itm-lst) itm-lst)
                               (setq beg-cell (cons (point) ind))
                               (cons (funcall assoc-at-point ind) itm-lst)))))
               ;; Looking at a list ending regexp.  Dismiss useless
               ;; data recorded above BEG-CELL.  Jump to part 2.
               ((looking-at "^[ \t]*\n[ \t]*\n")
                (throw 'exit
                       (setq itm-lst (memq (assq (car beg-cell) itm-lst)
                                           itm-lst))))
               ;; Point is at an item. Add data to ITM-LST. It may
               ;; also end a previous item: save it in END-LST. If ind
               ;; is less or equal than BEG-CELL and there is no end
               ;; at this ind or lesser, this item becomes the new
               ;; BEG-CELL.
               ((looking-at orgalist--item-re)
                (push (funcall assoc-at-point ind) itm-lst)
                (push (cons ind (point)) end-lst)
                (when (< ind text-min-ind) (setq beg-cell (cons (point) ind)))
                (forward-line -1))
               ;; Skip blank lines.
               ((looking-at "^[ \t]*$")
                (forward-line -1))
               ;; From there, point is not at an item.  Interpret
               ;; line's indentation:
               ;; - text at column 0 is necessarily out of any list.
               ;;   Dismiss data recorded above BEG-CELL.  Jump to
               ;;   part 2.
               ;; - any other case may be an ending position for an
               ;;   hypothetical item above.  Store it and proceed.
               ((= ind 0)
                (throw 'exit
                       (setq itm-lst
                             (memq (assq (car beg-cell) itm-lst) itm-lst))))
               (t
                (when (< ind text-min-ind) (setq text-min-ind ind))
                (push (cons ind (point)) end-lst)
                (forward-line -1)))))))
      ;; Read list from starting point to its end, that is until we
      ;; get out of context, or that a non-item line is less or
      ;; equally indented than BEG-CELL's cdr.  Also, store ending
      ;; position of items in END-LST-2.
      (catch 'exit
        (while t
          (let ((ind (current-indentation)))
            (cond
             ((>= (point) lim-down)
              ;; At downward limit: this is de facto the end of the
              ;; list.  Save point as an ending position, and jump to
              ;; part 3.
              (throw 'exit
                     (push (cons 0 (funcall end-before-blank)) end-lst-2)))
             ;; Looking at a list ending regexp.  Save point as an
             ;; ending position and jump to part 3.
             ((looking-at org-list-end-re)
              (throw 'exit (push (cons 0 (point)) end-lst-2)))
             ((looking-at orgalist--item-re)
              ;; Point is at an item.  Add data to ITM-LST-2. It may
              ;; also end a previous item, so save it in END-LST-2.
              (push (funcall assoc-at-point ind) itm-lst-2)
              (push (cons ind (point)) end-lst-2)
              (forward-line 1))
             ;; Skip blank lines along the way.
             ((looking-at "^[ \t]*$")
              (forward-line 1))
             ;; Ind is lesser or equal than BEG-CELL's.  The list is
             ;; over: store point as an ending position and jump to
             ;; part 3.
             ((<= ind (cdr beg-cell))
              (throw 'exit
                     (push (cons 0 (funcall end-before-blank)) end-lst-2)))
             ;; Else, if ind is lesser or equal than previous item's,
             ;; this is an ending position: store it.  In any case,
             ;; skip block or drawer at point, and move to next line.
             (t
              (when (<= ind (nth 1 (car itm-lst-2)))
                (push (cons ind (point)) end-lst-2))
              (forward-line 1))))))
      (setq struct (nconc itm-lst (cdr (nreverse itm-lst-2))))
      (setq end-lst (nconc end-lst (cdr (nreverse end-lst-2))))
      ;; Associate each item to its end position.
      (dolist (item struct)
        (pcase-let ((`(,pos ,ind . ,_) item))
          ;; Remove end candidates behind current item.
          (while (<= (cdar end-lst) pos) (pop end-lst))
          ;; Add end position to item assoc.
          (let ((old-end (nthcdr 6 item))
                (new-end (assoc-default ind end-lst '<=)))
            (if old-end
                (setcar old-end new-end)
              (setcdr item (append (cdr item) (list new-end)))))))
      ;; Return STRUCT.
      struct)))

(defun orgalist--goto-following-item (previous?)
  "Move point to next item in current list.
When PREVIOUS? is non-nil, move it to the previous item instead.
Throw an error if the move is not possible."
  (let* ((struct (orgalist--struct))
         (prevs (org-list-prevs-alist struct))
         (next (funcall (if previous? #'org-list-get-prev-item
                          #'org-list-get-next-item)
                        (line-beginning-position) struct prevs)))
    (unless next
      (error (if previous? "On first item" "On last item")))
    (goto-char next)))

(defun orgalist--move-item (up?)
  "Move item at point down.
When UP? is non-nil, move it up instead.  Throw an error if the
move is not possible."
  (let* ((col (current-column))
         (item (line-beginning-position))
         (struct (orgalist--struct))
         (next-item (funcall (if up? #'org-list-get-prev-item
                               #'org-list-get-next-item)
                             item struct (org-list-prevs-alist struct))))
    (unless next-item
      (user-error (if up? "Cannot move this item further up"
                    "Cannot move this item further down")))
    (setq struct
          (org-list-swap-items (if up? next-item item)
                               (if up? item next-item)
                               struct))
    (unless up?
      (let ((prevs (org-list-prevs-alist struct)))
        (goto-char (org-list-get-next-item item struct prevs))))
    (org-list-write-struct struct (org-list-parents-alist struct))
    (move-to-column col)))

(defun orgalist--auto-fill ()
  "Auto fill function.
Return nil outside of a list or in a blank line.  This function is
meant to be used as a piece of advice on `auto-fill-function'."
  (unless (org-match-line "^[ \t]*$")
    (let ((item? (orgalist--in-item-p)))
      (when item?
        (orgalist--call-in-item normal-auto-fill-function item?)
        t))))

(defun orgalist--fill-item (justify)
  "Fill item as a paragraph.

If JUSTIFY is non-nil, justify as well.

Return nil outside of a list.  This function is meant to be used
as a piece of advice on `fill-paragraph-function'."
  (let ((item? (orgalist--in-item-p)))
    (when item?
      (orgalist--call-in-item #'fill-paragraph item? justify)
      t)))

(defun orgalist--cycle-indentation ()
  "Cycle levels of indentation of an empty item.

The first run indents the item, if applicable.  Subsequent runs
outdent it at meaningful levels in the list.

This function is meant to be used as a piece of advice on
`indent-line-function'."
  (when (orgalist--at-item-p)
    (let ((struct (orgalist--struct)))
      (if (>= (progn (org-match-line orgalist--item-re) (match-end 0))
              (save-excursion
                (goto-char (org-list-get-item-end
                            (line-beginning-position) struct))
                (skip-chars-backward " \r\t\n")
                (point)))
          ;; If the item is not empty, do not indent.
          'noindent
        (let ((ind (org-list-get-ind (line-beginning-position) struct))
              (bullet (org-trim (buffer-substring (line-beginning-position)
                                                  (line-end-position)))))
          (setq this-command 'orgalist--cycle-indentation)
          ;; When in the middle of the cycle, try to outdent first.  If
          ;; it fails, and point is still at initial position, indent.
          ;; Else, re-create it at its original position.
          (if (eq last-command 'orgalist--cycle-indentation)
              (cond
               ((ignore-errors (org-list-indent-item-generic -1 t struct)))
               ((and (= ind (car orgalist--cycling-state))
                     (ignore-errors (org-list-indent-item-generic 1 t struct))))
               (t (delete-region (line-beginning-position) (line-end-position))
                  (indent-to-column (car orgalist--cycling-state))
                  (insert (cdr orgalist--cycling-state) " ")
                  ;; Break cycle.
                  (setq this-command 'identity)))
            ;; If a cycle is starting, remember indentation and bullet,
            ;; then try to indent.  If it fails, try to outdent.
            (setq orgalist--cycling-state (cons ind bullet))
            (cond
             ((ignore-errors (org-list-indent-item-generic 1 t struct)))
             ((ignore-errors (org-list-indent-item-generic -1 t struct)))
             (t 'noindent))))))))

(defun orgalist--while-at-item (cmd)
  "Return CMD when point is at a list item."
  (when (orgalist--at-item-p) cmd))

(defun orgalist--while-in-item (cmd)
  "Return CMD when point is in a list item."
  (when (orgalist--in-item-p) cmd))


;;; Bindings and menu

(defconst orgalist--maybe-previous
  '(menu-item "" orgalist-previous-item :filter orgalist--while-in-item))

(defconst orgalist--maybe-next
  '(menu-item "" orgalist-next-item :filter orgalist--while-in-item))

(defconst orgalist--maybe-insert
  '(menu-item "" orgalist-insert-item :filter orgalist--while-in-item))

(defconst orgalist--maybe-move-up
  '(menu-item "" orgalist-move-item-up :filter orgalist--while-at-item))

(defconst orgalist--maybe-move-down
  '(menu-item "" orgalist-move-item-down :filter orgalist--while-at-item))

(defconst orgalist--maybe-outdent
  '(menu-item "" orgalist-outdent-item :filter orgalist--while-at-item))

(defconst orgalist--maybe-indent
  '(menu-item "" orgalist-indent-item :filter orgalist--while-at-item))

(defconst orgalist--maybe-outdent-tree
  '(menu-item "" orgalist-outdent-item-tree :filter orgalist--while-at-item))

(defconst orgalist--maybe-indent-tree
  '(menu-item "" orgalist-indent-item-tree :filter orgalist--while-at-item))

(defconst orgalist--maybe-cycle-bullet
  '(menu-item "" orgalist-cycle-bullet :filter orgalist--while-at-item))

(defconst orgalist--maybe-check
  '(menu-item "" orgalist-check-item :filter orgalist--while-at-item))

(defconst orgalist--maybe-sort
  '(menu-item "" orgalist-sort-items :filter orgalist--while-at-item))

(defconst orgalist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<up>") orgalist--maybe-previous)
    (define-key map (kbd "M-<down>") orgalist--maybe-next)
    (define-key map (kbd "M-RET") orgalist--maybe-insert)
    (define-key map (kbd "M-S-<up>") orgalist--maybe-move-up)
    (define-key map (kbd "M-S-<down>") orgalist--maybe-move-down)
    (define-key map (kbd "M-<left>") orgalist--maybe-outdent)
    (define-key map (kbd "M-<right>") orgalist--maybe-indent)
    (define-key map (kbd "M-S-<left>") orgalist--maybe-outdent-tree)
    (define-key map (kbd "M-S-<right>") orgalist--maybe-indent-tree)
    (define-key map (kbd "C-c -") orgalist--maybe-cycle-bullet)
    (define-key map (kbd "C-c C-c") orgalist--maybe-check)
    (define-key map (kbd "C-c ^") orgalist--maybe-sort)
    map))

(easy-menu-define orgalist--menu
  orgalist-mode-map
  "Menu used when Orgalist mode is active."
  '("Orgalist" :visible orgalist-mode
    "----"
    ["Insert item" orgalist-insert-item :active (orgalist--in-item-p)]
    "----"
    ["Check item" orgalist-check-item :active (orgalist--at-item-p)]
    ["Cycle bullet" orgalist-cycle-bullet :active (orgalist--at-item-p)]
    "----"
    ["Previous item" orgalist-previous-item :active (orgalist--in-item-p)]
    ["Next item" orgalist-next-item :active (orgalist--in-item-p)]
    ["Move item up" orgalist-move-item-up :active (orgalist--at-item-p)]
    ["Move item down" orgalist-move-item-gdown :active (orgalist--at-item-p)]
    "---"
    ["Indent item" orgalist-indent-item :active (orgalist--at-item-p)]
    ["Indent tree" orgalist-indent-item-tree :active (orgalist--at-item-p)]
    ["Outdent item" orgalist-outdent-item :active (orgalist--at-item-p)]
    ["Outdent tree" orgalist-outdent-item-tree :active (orgalist--at-item-p)]
    "---"
    ["Sort items" orgalist-sort-items :active (orgalist--at-item-p)]))


;;; Minor mode definition

;;;###autoload
(define-minor-mode orgalist-mode
  "Toggle the minor mode `orgalist-mode'.

This mode is for using Org mode plain lists commands in other
major modes.

key             binding
---             -------
M-<RET>         `orgalist-insert-item'
M-<UP>          `orgalist-previous-item'
M-<DOWN>        `orgalist-next-item'
M-S-<UP>        `orgalist-move-item-up'
M-S-<DOWN>      `orgalist-move-item-down'
M-<LEFT>        `orgalist-outdent-item'
M-<RIGHT>       `orgalist-indent-item'
M-S-<LEFT>      `orgalist-outdent-item-tree'
M-S-<RIGHT>     `orgalist-indent-item-tree'
C-c -           `orgalist-cycle-bullet'
C-c ^           `orgalist-sort-items'
C-c C-c         `orgalist-check-item'"
  :lighter " olst"
  (cond
   (orgalist-mode
    (when (derived-mode-p 'org-mode)
      (user-error "Cannot activate Orgalist mode in an Org buffer"))
    (setq-local org-list-allow-alphabetical t)
    (setq-local org-list-automatic-rules nil)
    (setq-local org-list-demote-modify-bullet nil)
    (setq-local org-list-two-spaces-after-bullet-regexp nil)
    (add-function :before-until (local 'auto-fill-function)
                  #'orgalist--auto-fill)
    (add-function :before-until
                  (local 'fill-paragraph-function)
                  #'orgalist--fill-item)
    (add-function :before-until
                  (local 'indent-line-function)
                  #'orgalist--cycle-indentation))
   (t
    (remove-function (local 'auto-fill-function) #'orgalist--auto-fill)
    (remove-function (local 'fill-paragraph-function) #'orgalist--fill-item)
    (remove-function (local 'indent-line-function)
                     #'orgalist--cycle-indentation))))


;;; Public functions

(defun orgalist-previous-item ()
  "Move to the beginning of the previous item.
Throw an error when at first item."
  (interactive)
  (let ((item (orgalist--in-item-p)))
    (unless item (user-error "Not in a list"))
    (goto-char item)
    (orgalist--goto-following-item t)))

(defun orgalist-next-item ()
  "Move to the beginning of the next item.
Throw an error when at last item."
  (interactive)
  (let ((item (orgalist--in-item-p)))
    (unless item (user-error "Not in a list"))
    (goto-char item)
    (orgalist--goto-following-item nil)))

(defun orgalist-insert-item (&optional checkbox)
  "Insert a new item at the current level.

If cursor is before first character after bullet of the item, the
new item will be created before the current one.

If CHECKBOX is non-nil, add a checkbox next to the bullet."
  (interactive "P")
  (let ((item? (orgalist--in-item-p)))
    (unless item? (user-error "Not in a list"))
    (let* ((struct (save-excursion (goto-char item?) (orgalist--struct)))
           (prevs (org-list-prevs-alist struct))
           ;; If we're in a description list, ask for the new term.
           (desc
            (and (eq 'descriptive (org-list-get-list-type item? struct prevs))
                 " :: ")))
      (setq struct (org-list-insert-item (point) struct prevs checkbox desc))
      (org-list-write-struct struct (org-list-parents-alist struct))
      (looking-at orgalist--item-re)
      (goto-char (if (and (match-beginning 4)
                          (save-match-data
                            (string-match "\\." (match-string 1))))
                     (match-beginning 4)
                   (match-end 0)))
      (when desc (backward-char 1)))))

(defun orgalist-move-item-down ()
  "Move the item at point down, i.e. swap with following item.
Sub-items (items with larger indentation) are considered part of
the item, so this really moves item trees."
  (interactive)
  (unless (orgalist--at-item-p) (user-error "Not in a list"))
  (orgalist--move-item nil))

(defun orgalist-move-item-up ()
  "Move the item at point up, i.e. swap with previous item.
Sub-items (items with larger indentation) are considered part of
the item, so this really moves item trees."
  (interactive)
  (unless (orgalist--at-item-p) (user-error "Not in a list"))
  (orgalist--move-item t))

(defun orgalist-cycle-bullet ()
  "Cycle through the different itemize/enumerate bullets.
This cycle the entire list level through the sequence:

   `-'  ->  `+'  ->  `1.'  ->  `a.'"
  (interactive)
  (unless (orgalist--at-item-p) (user-error "Not in a list"))
  (save-excursion
    (beginning-of-line)
    (let* ((struct (orgalist--struct))
           (parents (org-list-parents-alist struct))
           (prevs (org-list-prevs-alist struct))
           (list-beg (org-list-get-first-item (point) struct prevs))
           (bullet (org-list-get-bullet list-beg struct))
           (current (cond ((string-match "[A-Za-z]\\." bullet) "a.")
                          ((string-match "\\." bullet) "1.")
                          (t (org-trim bullet))))
           (bullet-list
            (append '("-" "+")
                    ;; Description items cannot be numbered.
                    (unless (org-at-item-description-p) '("1."))
                    (unless (org-at-item-description-p) '("a."))))
           (new (or (cadr (member current bullet-list))
                    (car bullet-list))))
      ;; Use a short variation of `org-list-write-struct' as there's
      ;; no need to go through all the steps.
      (let ((old-struct (copy-tree struct)))
        (org-list-set-bullet list-beg struct (org-list-bullet-string new))
        (org-list-struct-fix-bul struct prevs)
        (org-list-struct-fix-ind struct parents)
        (org-list-struct-apply-struct struct old-struct)))))

(defun orgalist-outdent-item ()
  "Outdent a local list item, but not its children."
  (interactive)
  (unless (orgalist--at-item-p) (user-error "Not in a list"))
  (org-list-indent-item-generic -1 t (orgalist--struct)))

(defun orgalist-indent-item ()
  "Indent a local list item, but not its children."
  (interactive)
  (unless (orgalist--at-item-p) (user-error "Not in a list"))
  (org-list-indent-item-generic 1 t (orgalist--struct)))

(defun orgalist-outdent-item-tree ()
  "Outdent a local list item including its children."
  (interactive)
  (unless (orgalist--at-item-p) (user-error "Not in a list"))
  (org-list-indent-item-generic -1 nil (orgalist--struct)))

(defun orgalist-indent-item-tree ()
  "Indent a local list item including its children."
  (interactive)
  (unless (orgalist--at-item-p) (user-error "Not in a list"))
  (org-list-indent-item-generic 1 nil (orgalist--struct)))

(defun orgalist-check-item (&optional arg)
  "Toggle the checkbox in the current line.

With prefix ARG, add or remove checkboxes.  With double prefix,
set checkbox to [-].

In any case, fix indentation, bullets and checkboxes in the list
at point."
  (interactive "P")
  (unless (orgalist--at-item-p) (user-error "Not in a list"))
  (save-excursion
    (beginning-of-line)
    (catch :repair-only
      (let* ((regexp (concat "[ \t]*\\(?:[-+]\\|\\(?:[a-zA-Z]\\|[0-9]+\\)\\.\\)"
                             "\\(?:[ \t]+\\[@\\(?:[a-zA-Z]\\|[0-9]+\\)\\]\\)?"
                             "[ \t]+\\(\\[[- xX]\\]\\)"))
             (struct (orgalist--struct))
             (struct-copy (copy-tree struct))
             (parents (org-list-parents-alist struct))
             (prevs (org-list-prevs-alist struct))
             (current (and (looking-at regexp) (match-string 1)))
             (new
              (cond
               ((equal arg '(16)) "[-]")
               ((equal arg '(4)) (if current nil "[ ]"))
               ((not current)
                (throw :repair-only (org-list-write-struct struct parents)))
               ((member current '("[X]" "[x]")) "[ ]")
               (t "[X]"))))
        (org-list-set-checkbox (point) struct new)
        (when (org-list-struct-fix-box
               struct parents prevs orgalist-ordered-checkboxes)
          (org-list-write-struct struct-copy parents)
          (error "Checkbox blocked because of unchecked box"))
        (org-list-struct-apply-struct struct struct-copy)))))

(defvar org-after-sorting-entries-or-items-hook)

(defun orgalist-sort-items (with-case sorting-type)
  "Sort list items.

The cursor may be at any item of the list that should be sorted.
Sublists are not sorted.

Comparing entries ignores case by default.  However, with an
optional argument WITH-CASE, the sorting considers case as well.

The command prompts for the SORTING-TYPE, which needs to be
a character among ?n ?N ?a ?A ?x ?X.  Here is the detailed meaning
of each character:

n   Numerically, by converting the beginning of the item to a number.
a   Alphabetically.  Only the first line of item is checked.
x   By \"checked\" status of a check list.

Capital letters reverse the sort order."
  (interactive
   (list current-prefix-arg
         (progn
           (message "Sort plain list: [a/A]lpha  [n/N]umeric  [x/X]checked:")
           (read-char-exclusive))))
  (unless (memq sorting-type '(?a ?A ?n ?N ?x ?X))
    (user-error "Invalid sorting type"))
  (let ((org-after-sorting-entries-or-items-hook nil))
    (org-sort-list with-case sorting-type)))

(defun orgalist-insert-radio-list ()
  "Insert a radio list template appropriate for current major mode."
  (interactive)
  (let* ((e (or (cl-assoc-if #'derived-mode-p orgalist-radio-list-templates)
                (error "No radio list setup defined for %S" major-mode)))
         (name (read-string "List name: "))
         (txt (replace-regexp-in-string "%n" name (nth 1 e) t t)))
    (unless (bolp) (insert "\n"))
    (save-excursion (insert txt))))

(defun orgalist-send-list (&optional maybe)
  "Send a transformed version of this list to the receiver position.
With argument MAYBE, fail quietly if no transformation is defined
for this list."
  (interactive)
  (catch 'exit
    (unless (orgalist--at-item-p) (error "Not at a list item"))
    (save-excursion
      (let ((case-fold-search t))
        (re-search-backward "^[ \t]*#\\+ORGLST:" nil t)
        (unless (looking-at
                 "[ \t]*#\\+ORGLST:[ \t]+SEND[ \t]+\\(\\S-+\\)[ \t]+\\([^ \t\n]+\\)")
          (if maybe (throw 'exit nil)
            (error "Don't know how to transform this list")))))
    (let* ((name (regexp-quote (match-string 1)))
           (transform (intern (match-string 2)))
           (bottom-point
            (save-excursion
              (re-search-forward
               "\\(\\\\end{comment}\\|@end ignore\\|-->\\)" nil t)
              (match-beginning 0)))
           (top-point
            (progn
              (re-search-backward "#\\+ORGLST" nil t)
              (re-search-forward (org-item-beginning-re) bottom-point t)
              (match-beginning 0)))
           (plain-list (save-excursion
                         (goto-char top-point)
                         (org-list-to-lisp))))
      (unless (fboundp transform)
        (error "No such transformation function %s" transform))
      (let ((txt (funcall transform plain-list)))
        ;; Find the insertion(s) place(s).
        (save-excursion
          (goto-char (point-min))
          (let ((receiver-count 0)
                (begin-re (format "BEGIN +RECEIVE +ORGLST +%s\\([ \t]\\|$\\)"
                                  name))
                (end-re (format "END +RECEIVE +ORGLST +%s\\([ \t]\\|$\\)"
                                name)))
            (while (re-search-forward begin-re nil t)
              (cl-incf receiver-count)
              (let ((beg (line-beginning-position 2)))
                (unless (re-search-forward end-re nil t)
                  (user-error "Cannot find end of receiver location at %d" beg))
                (beginning-of-line)
                (delete-region beg (point))
                (insert txt "\n")))
            (cond
             ((> receiver-count 1)
              (message "List converted and installed at receiver locations"))
             ((= receiver-count 1)
              (message "List converted and installed at receiver location"))
             (t (user-error "No valid receiver location found")))))))))

;;;; ChangeLog:

;; 2018-05-01  Nicolas Goaziou  <mail@nicolasgoaziou.fr>
;; 
;; 	orgalist: Fix location of "orgalist.el ends here" cookie
;; 
;; 2018-05-01  Nicolas Goaziou  <mail@nicolasgoaziou.fr>
;; 
;; 	orgalist: Improve menu and a docstring
;; 
;; 	* orgalist.el (orgalist--cycle-indentation): Improve docstring.
;; 	(orgalist--menu): Improve menu.
;; 
;; 2018-05-01  Nicolas Goaziou  <mail@nicolasgoaziou.fr>
;; 
;; 	orgalist: Bump to version 1.2
;; 
;; 2018-05-01  Nicolas Goaziou  <mail@nicolasgoaziou.fr>
;; 
;; 	orgalist: Add non-nil `sentence-end-double-space' file local variable
;; 
;; 2018-05-01  Nicolas Goaziou  <mail@nicolasgoaziou.fr>
;; 
;; 	orgalist: Improve documentation
;; 
;; 2018-05-01  Nicolas Goaziou  <mail@nicolasgoaziou.fr>
;; 
;; 	orgalist: Replace `orgalist--indentation' with `current-indentation'
;; 
;; 	* orgalist.el (orgalist--indentation): Remove function.
;; 	(orgalist--in-item-p):
;; 	(orgalist--struct): Use `current-indentation'.
;; 
;; 2018-05-01  Nicolas Goaziou  <mail@nicolasgoaziou.fr>
;; 
;; 	orgalist: Improve indentation cycling
;; 
;; 	* orgalist.el (orgalist--cycle-indentation): New function.
;; 	(orgalist--while-at-empty-item): Remove function.
;; 	(orgalist--maybe-cycle-indentation): Remove variable.
;; 	(orgalist-mode-map): Remove hard-coded <TAB> binding.
;; 	(orgalist-mode): Advise `indent-line-function' with
;; 	`orgalist--cycle-indentation'.	Improve docstring.
;; 	(orgalist-cycle-indentation): Remove function.
;; 
;; 2018-05-01  Nicolas Goaziou  <mail@nicolasgoaziou.fr>
;; 
;; 	orgalist: Simplify auto fill
;; 
;; 	* orgalist.el (orgalist--last-advised-function): Remove variable.
;; 	(orgalist--set-auto-fill): Remove function.
;; 	(orgalist-mode): Directly advise `auto-fill-function' instead of relying
;; 	on a post command hook.
;; 
;; 2018-05-01  Nicolas Goaziou  <mail@nicolasgoaziou.fr>
;; 
;; 	orgalist: Fix filling items
;; 
;; 	* orgalist.el (orgalist--call-in-item): Allow arguments.  Take into
;; 	 consideration indentation of current item.  Improve docstring.
;; 	(orgalist--auto-fill): Improve docstring.  Return a non-nil value when 
;; 	taking care of the auto filling.
;; 	(orgalist--fill-item): New function.
;; 	(orgalist--maybe-fill): Remove variable.
;; 	(orgalist-mode-map): Do not bind "M-q".
;; 	(orgalist-mode): Advise `fill-paragraph-function'.
;; 	(orgalist-fill-item): Remove function.
;; 
;; 2018-04-30  Nicolas Goaziou  <mail@nicolasgoaziou.fr>
;; 
;; 	orgalist: Bump to version 1.1
;; 
;; 2018-04-30  Nicolas Goaziou  <mail@nicolasgoaziou.fr>
;; 
;; 	orgalist: Only cycle indentation on empty items
;; 
;; 	* orgalist.el (orgalist--while-at-empty-item): New function.
;; 	(orgalist--maybe-cycle-indentation): Use new function.
;; 
;; 	Elsewhere, fallback to standard TAB behavior.
;; 
;; 2018-04-30  Nicolas Goaziou  <mail@nicolasgoaziou.fr>
;; 
;; 	orgalist: Silence byte-compiler
;; 
;; 2018-04-30  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* orgalist.el: Only autoload the minor mode
;; 
;; 	Require Emacs≥24.4 since we use add-function.
;; 	(org-after-sorting-entries-or-items-hook): Declare.
;; 
;; 2018-04-30  Nicolas Goaziou  <mail@nicolasgoaziou.fr>
;; 
;; 	Second version, sent to emacs-devel
;; 
;; 2018-04-30  Nicolas Goaziou  <mail@nicolasgoaziou.fr>
;; 
;; 	Initial version, sent to emacs-orgmode list
;; 



(provide 'orgalist)

;; Local Variables:
;; sentence-end-double-space: t
;; End:

;;; orgalist.el ends here
