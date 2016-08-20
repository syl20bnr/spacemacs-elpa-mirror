;;; xah-get-thing.el --- get thing or selection at point.

;; Copyright © 2011-2015 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Version: 1.0.1
;; Package-Version: 20150712.1430
;; Created: 22 May 2015
;; Keywords: extensions, lisp, tools
;; URL: http://ergoemacs.org/emacs/elisp_get-selection-or-unit.html

;; This file is not part of GNU Emacs.

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; This package provides functions similar to `thing-at-point' of `thingatpt.el'.

;; The functions are:

;; xah-get-thing-at-cursor
;; xah-get-thing-or-selection

;; They get “thing” independent of syntax table, so you always get same thing regardless what's current major mode.

;; xah-get-thing-or-selection get text selection when there's active region.

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

(defun xah-get-thing-at-cursor (φunit)
  "Return the string and boundary of ΦUNIT under cursor.
Returns vector [text a b], where text is the string and a b are its boundary.

ΦUNIT can be:

• 'word — sequence of hyphen, 0 to 9, A to Z, a to z, and other chars such as àáâãäå.

• 'glyphs — sequence of visible glyphs. Useful for file name, URL, …, anything doesn't have white spaces in it.

• 'line — delimited by “\\n”. (captured text does not include “\\n”.)

• 'block — delimited by empty lines or beginning/end of buffer. Lines with just spaces or tabs are also considered empty line. (captured text does not include a ending “\\n”.)

• 'buffer — whole buffer. (respects `narrow-to-region')

• 'filepath — delimited by chars that's USUALLY not part of filepath.

• 'url — delimited by chars that's USUALLY not part of URL.

• a vector [beginRegex endRegex] — The elements are regex strings used to determine the beginning/end of boundary chars. They are passed to `skip-chars-backward' and `skip-chars-forward'. For example, if you want paren as delimiter, use [\"^(\" \"^)\"]

Example usage:
 (setq bds (xah-get-thing-at-cursor 'line))
 (setq inputstr (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )

This function is similar to `thing-at-point' and `bounds-of-thing-at-point'.
The main differences are:

• This function returns the text and the 2 boundaries as a vector in one shot.

• 'line always returns the line without end of line character, avoiding inconsistency when the line is at end of buffer.

• This function's behavior does not depend on syntax table. e.g. for units 「'word」, 「'block」, etc."
  (let (p1 p2)
    (save-excursion
      (cond
       ( (eq φunit 'word)
         (let ((wordcharset "-A-Za-z0-9ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿ"))
           (skip-chars-backward wordcharset)
           (setq p1 (point))
           (skip-chars-forward wordcharset)
           (setq p2 (point))))

       ( (eq φunit 'glyphs)
         (progn
           (skip-chars-backward "[:graph:]")
           (setq p1 (point))
           (skip-chars-forward "[:graph:]")
           (setq p2 (point))))

       ((eq φunit 'buffer)
        (progn
          (setq p1 (point-min))
          (setq p2 (point-max))))

       ((eq φunit 'line)
        (progn
          (setq p1 (line-beginning-position))
          (setq p2 (line-end-position))))
       ((eq φunit 'block)
        (progn
          (if (re-search-backward "\n[ \t]*\n" nil "move")
              (progn (re-search-forward "\n[ \t]*\n")
                     (setq p1 (point)))
            (setq p1 (point)))
          (if (re-search-forward "\n[ \t]*\n" nil "move")
              (progn (re-search-backward "\n[ \t]*\n")
                     (setq p2 (point)))
            (setq p2 (point)))))

       ((eq φunit 'filepath)
        (let (p0)
          (setq p0 (point))
          ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
          (skip-chars-backward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
          (setq p1 (point))
          (goto-char p0)
          (skip-chars-forward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
          (setq p2 (point))))

       ((eq φunit 'url)
        (let (p0
              ;; (ξdelimitors "^ \t\n,()[]{}<>〔〕“”\"`'!$^*|\;")
              (ξdelimitors "!\"#$%&'*+,-./0123456789:;=?@ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~"))
          (setq p0 (point))
          (skip-chars-backward ξdelimitors) ;"^ \t\n,([{<>〔“\""
          (setq p1 (point))
          (goto-char p0)
          (skip-chars-forward ξdelimitors) ;"^ \t\n,)]}<>〕\"”"
          (setq p2 (point))))

       ((vectorp φunit)
        (let (p0)
          (setq p0 (point))
          (skip-chars-backward (elt φunit 0))
          (setq p1 (point))
          (goto-char p0)
          (skip-chars-forward (elt φunit 1))
          (setq p2 (point))))))

    (vector (buffer-substring-no-properties p1 p2) p1 p2 )))

(defun xah-get-thing-or-selection (φunit)
  "Return the string and boundary of text selection or ΦUNIT under cursor.

Returns a vector [text a b], where text is the string and a b are its boundary.

If `use-region-p' is true, then text is the region.  Else, it depends on the ΦUNIT, which can be 'word 'line 'block 'buffer 'filepath 'url …. See `xah-get-thing-at-cursor' for detail.

Example usage:
(progn
    (setq boundary (xah-get-thing-or-selection 'block))
    (setq
     input-str (elt boundary 0)
     p1 (elt boundary 1)
     p2 (elt boundary 2)))"
  (interactive)
  (if (use-region-p)
      (let ((p1 (region-beginning)) (p2 (region-end)))
        (vector (buffer-substring-no-properties p1 p2) p1 p2 ))
    (xah-get-thing-at-cursor φunit)))

(provide 'xah-get-thing)

;; Local Variables:
;; coding: utf-8
;; End:

;;; xah-get-thing.el ends here
