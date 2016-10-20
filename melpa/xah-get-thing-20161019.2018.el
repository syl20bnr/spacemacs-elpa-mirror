;;; xah-get-thing.el --- get thing or selection at point.

;; Copyright © 2011-2015 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Version: 2.0.1
;; Package-Version: 20161019.2018
;; Created: 22 May 2015
;; Keywords: extensions, lisp, tools
;; URL: http://ergoemacs.org/emacs/elisp_get-selection-or-unit.html

;; This file is not part of GNU Emacs.

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; This package provides functions similar to `thing-at-point' of `thingatpt.el'.

;; The functions are:

;; xah-get-bounds-of-thing
;; xah-get-bounds-of-thing-or-region
;; xah-get-thing-at-point
;; xah-get-thing-at-cursor (deprecated)
;; xah-get-thing-or-selection (deprecated)

;; They get “thing” independent of syntax table, so you always get same thing regardless what's current major mode.

;; Call describe-function on them to read the inline doc.
;; Home page: http://ergoemacs.org/emacs/elisp_get-selection-or-unit.html

;;; Install:

;; To install manually, place this file in the directory 〔~/.emacs.d/lisp/〕.
;; Then, add the following in your emacs lisp init:
;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; Then, in elisp code where you want to use it, add
;; (require 'xah-get-thing)

;;; HISTORY

;; 2015-05-22 changes won't be logged here anymore, unless incompatible ones.
;; version 1.0, 2015-05-22 was {unit-at-cursor, get-selection-or-unit} from xeu_elisp_util.el


;;; Code:

(defun xah-get-bounds-of-thing (*unit )
  "Return the boundary of *UNIT under cursor.

Return a cons cell (START . END).

*UNIT can be:

• 'word — sequence of 0 to 9, A to Z, a to z, and hyphen.

• 'glyphs — sequence of visible glyphs. Useful for file name, URL, …, anything doesn't have white spaces in it.

• 'line — delimited by “\\n”. (captured text does not include “\\n”.)

• 'block — delimited by empty lines or beginning/end of buffer. Lines with just spaces or tabs are also considered empty line. (captured text does not include a ending “\\n”.)

• 'buffer — whole buffer. (respects `narrow-to-region')

• 'filepath — delimited by chars that's USUALLY not part of filepath.

• 'url — delimited by chars that's USUALLY not part of URL.

• a vector [beginRegex endRegex] — The elements are regex strings used to determine the beginning/end of boundary chars. They are passed to `skip-chars-backward' and `skip-chars-forward'. For example, if you want paren as delimiter, use [\"^(\" \"^)\"]

This function is similar to `bounds-of-thing-at-point'.
The main difference are:

• This function's behavior does not depend on syntax table. e.g. for *units 「'word」, 「'block」, etc.

• 'line always returns the line without end of line character, avoiding inconsistency when the line is at end of buffer.

• Support certain “thing” such as 'glyphs that's a sequence of chars. Useful as file path or url in html links, but do not know which before hand.

• Some “thing” such 'url and 'filepath considers strings that at USUALLY used for such. The algorithm that determines this is different from thing-at-point.

Version 2016-10-18"
  (let (p1 p2)
    (save-excursion
      (cond
       ( (eq *unit 'word)
         (let ((wordcharset "-A-Za-z0-9ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿ"))
           (skip-chars-backward wordcharset)
           (setq p1 (point))
           (skip-chars-forward wordcharset)
           (setq p2 (point))))

       ( (eq *unit 'glyphs)
         (progn
           (skip-chars-backward "[:graph:]")
           (setq p1 (point))
           (skip-chars-forward "[:graph:]")
           (setq p2 (point))))

       ((eq *unit 'buffer)
        (progn
          (setq p1 (point-min))
          (setq p2 (point-max))))

       ((eq *unit 'line)
        (progn
          (setq p1 (line-beginning-position))
          (setq p2 (line-end-position))))
       ((eq *unit 'block)
        (progn
          (if (re-search-backward "\n[ \t]*\n" nil "move")
              (progn (re-search-forward "\n[ \t]*\n")
                     (setq p1 (point)))
            (setq p1 (point)))
          (if (re-search-forward "\n[ \t]*\n" nil "move")
              (progn (re-search-backward "\n[ \t]*\n")
                     (setq p2 (point)))
            (setq p2 (point)))))

       ((eq *unit 'filepath)
        (let (p0)
          (setq p0 (point))
          ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
          (skip-chars-backward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
          (setq p1 (point))
          (goto-char p0)
          (skip-chars-forward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
          (setq p2 (point))))

       ((eq *unit 'url)
        (let (p0
              ;; (-delimitors "^ \t\n,()[]{}<>〔〕“”\"`'!$^*|\;")
              (-delimitors "!\"#$%&'*+,-./0123456789:;=?@ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~"))
          (setq p0 (point))
          (skip-chars-backward -delimitors) ;"^ \t\n,([{<>〔“\""
          (setq p1 (point))
          (goto-char p0)
          (skip-chars-forward -delimitors) ;"^ \t\n,)]}<>〕\"”"
          (setq p2 (point))))

       ((vectorp *unit)
        (let (p0)
          (setq p0 (point))
          (skip-chars-backward (elt *unit 0))
          (setq p1 (point))
          (goto-char p0)
          (skip-chars-forward (elt *unit 1))
          (setq p2 (point))))))

    (cons p1 p2 )))

(defun xah-get-bounds-of-thing-or-region (*unit)
  "Same as `xah-get-bounds-of-thing', except when (use-region-p) is t, return the region boundary instead.
Version 2016-10-18"
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (xah-get-bounds-of-thing *unit)))

(defun xah-get-thing-at-point (*unit)
  "Same as `xah-get-bounds-of-thing', but return the string.
Version 2016-10-18T02:31:36-07:00"
  (let ( (-bds (xah-get-bounds-of-thing *unit)) )
    (buffer-substring-no-properties (car -bds) (cdr -bds))))

(defun xah-get-thing-at-cursor (*unit)
  "Same as `xah-get-bounds-of-thing', except this returns a vector [text a b], where text is the string and a b are its boundary.

Version 2016-10-18T00:23:52-07:00"
  (let* (
         (-bds (xah-get-bounds-of-thing *unit))
         (-p1 (car -bds)) (-p2 (cdr -bds)))
    (vector (buffer-substring-no-properties -p1 -p2) -p1 -p2 )))

(make-obsolete 'xah-get-thing-at-cursor 'xah-get-thing-at-point "2016-10-18")

(defun xah-get-thing-or-selection (*unit)
  "Same as `xah-get-bounds-of-thing-or-region', except returns a vector [text a b], where text is the string and a b are its boundary."
  (interactive)
  (let* (
         (-bds (xah-get-bounds-of-thing-or-region *unit))
         (-p1 (car -bds)) (-p2 (cdr -bds)))
    (vector (buffer-substring-no-properties -p1 -p2) -p1 -p2 )))

(make-obsolete 'xah-get-thing-or-selection 'xah-get-bounds-of-thing-or-region "2016-10-18")

(provide 'xah-get-thing)

;; Local Variables:
;; coding: utf-8
;; End:

;;; xah-get-thing.el ends here
