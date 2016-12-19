;;; xah-replace-pairs.el --- Multi-pair find/replace in strings and region.  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2010-2016, by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Version: 2.2.2
;; Package-Version: 20161218.2147
;; Created: 17 Aug 2010
;; Package-Requires: ((emacs "24.1"))
;; Keywords: lisp, tools, find replace
;; URL: http://ergoemacs.org/emacs/elisp_replace_string_region.html

;; This file is not part of GNU Emacs.

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; This package provides elisp functions that do find/replace with multiple pairs of strings. and guarantees that earlier find/replace pair does not effect later find/replace pairs.

;; The functions are:

;; xah-replace-pairs-region
;; xah-replace-pairs-in-string
;; xah-replace-regexp-pairs-region
;; xah-replace-regexp-pairs-in-string
;; xah-replace-pairs-region-recursive
;; xah-replace-pairs-in-string-recursive

;; Call `describe-function' on them for detail.

;; Or, see home page at
;; http://ergoemacs.org/emacs/elisp_replace_string_region.html

;; If you like it, please support by Buy Xah Emacs Tutorial
;; http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html
;; Thanks.

;;; History:

;; 2015-04-28 major rewrite. This package was xfrp_find_replace_pairs
;; version 1.0, 2010-08-17. First version.


;;; Code:

(defun xah-replace-pairs-region (*begin *end *pairs &optional *report-p *hilight-p)
  "Replace multiple *PAIRS of find/replace strings in region *BEGIN *END.

*PAIRS is a sequence of pairs
 [[findStr1 replaceStr1] [findStr2 replaceStr2] …]
each element or entire argument can be list or vector.

Find strings case sensitivity depends on `case-fold-search'. You can set it locally, like this: (let ((case-fold-search nil)) …)

The replacement are literal and case sensitive.

Once a subsring in the buffer is replaced, that part will not change again.  For example, if the buffer content is “abcd”, and the *pairs are a → c and c → d, then, result is “cbdd”, not “dbdd”.

*REPORT-P is t or nil. If t, it prints each replaced pairs, one pair per line.

Returns a list, each element is a vector [position findStr replaceStr].

Note: the region's text or any string in *PAIRS is assumed to NOT contain any character from Unicode Private Use Area A. That is, U+F0000 to U+FFFFD. And, there are no more than 65534 pairs.
Version 2016-10-05"
  (let (
        (-unicodePriveUseA #xf0000)
        (-i 0)
        (-tempMapPoints '())
        (-changeLog '()))
    (progn
      ;; generate a list of Unicode chars for intermediate replacement. These chars are in  Private Use Area.
      (setq -i 0)
      (while (< -i (length *pairs))
        (push (char-to-string (+ -unicodePriveUseA -i)) -tempMapPoints)
        (setq -i (1+ -i))))
    (save-excursion
      (save-restriction
        (narrow-to-region *begin *end)
        (progn
          ;; replace each find string by corresponding item in -tempMapPoints
          (setq -i 0)
          (while (< -i (length *pairs))
            (goto-char (point-min))
            (while (search-forward (elt (elt *pairs -i) 0) nil t)
              (replace-match (elt -tempMapPoints -i) t t))
            (setq -i (1+ -i))))
        (progn
          ;; replace each -tempMapPoints by corresponding replacement string
          (setq -i 0)
          (while (< -i (length *pairs))
            (goto-char (point-min))
            (while (search-forward (elt -tempMapPoints -i) nil t)
              (push (vector (point)
                            (elt (elt *pairs -i) 0)
                            (elt (elt *pairs -i) 1)) -changeLog)
              (replace-match (elt (elt *pairs -i) 1) t t)
              (when *hilight-p
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)))
            (setq -i (1+ -i))))))

    (when (and *report-p (> (length -changeLog) 0))
      (mapc
       (lambda (-x)
         (princ -x)
         (terpri))
       (reverse -changeLog)))

    -changeLog
    ))

(defun xah-replace-pairs-in-string (*str *pairs)
  "Replace string *STR by find/replace *PAIRS sequence.
Returns the new string.
This function is a wrapper of `xah-replace-pairs-region'. See there for detail."
  (with-temp-buffer
    (insert *str)
    (xah-replace-pairs-region 1 (point-max) *pairs)
    (buffer-string)))

(defun xah-replace-regexp-pairs-region (*begin *end *pairs &optional *fixedcase-p *literal-p *hilight-p)
  "Replace regex string find/replace *PAIRS in region.

*BEGIN *END are the region boundaries.

*PAIRS is
 [[regexStr1 replaceStr1] [regexStr2 replaceStr2] …]
It can be list or vector, for the elements or the entire argument.

The optional arguments *FIXEDCASE-P and *LITERAL-P is the same as in `replace-match'.
If *hilight-p is true, highlight the changed region.

Find strings case sensitivity depends on `case-fold-search'. You can set it locally, like this: (let ((case-fold-search nil)) …)
Version 2016-10-05"
  (save-restriction
    (narrow-to-region *begin *end)
    (mapc
     (lambda (-x)
       (goto-char (point-min))
       (while (search-forward-regexp (elt -x 0) (point-max) t)
         (replace-match (elt -x 1) *fixedcase-p *literal-p)
         (when *hilight-p
           (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight))))
     *pairs)))

(defun xah-replace-regexp-pairs-in-string (*str *pairs &optional *fixedcase-p *literal-p)
  "Replace string *STR recursively by regex find/replace pairs *PAIRS sequence.

This function is a wrapper of `xah-replace-regexp-pairs-region'. See there for detail.

See also `xah-replace-pairs-in-string'."
  (with-temp-buffer
    (insert *str)
    (goto-char (point-min))
    (xah-replace-regexp-pairs-region (point-min) (point-max) *pairs *fixedcase-p *literal-p)
    (buffer-string)))

(defun xah-replace-pairs-region-recursive (*begin *end *pairs)
  "Replace multiple *PAIRS of find/replace strings in region *BEGIN *END.

This function is similar to `xah-replace-pairs-region', except that the replacement is done recursively after each find/replace pair.  Earlier replaced value may be replaced again.

For example, if the input string is “abcd”, and the pairs are a → c and c → d, then, the result is “dbdd”, not “cbdd”.

Find strings case sensitivity depends on `case-fold-search'. You can set it locally, like this: (let ((case-fold-search nil)) …)

The replacement are literal and case sensitive."
  (save-restriction
    (narrow-to-region *begin *end)
    (mapc
     (lambda (x)
       (goto-char (point-min))
       (while (search-forward (elt x 0) (point-max) 'NOERROR)
         (replace-match (elt x 1) t t)))
     *pairs)))

(defun xah-replace-pairs-in-string-recursive (*str *pairs)
  "Replace string *STR recursively by find/replace pairs *PAIRS sequence.

This function is is a wrapper of `xah-replace-pairs-region-recursive'. See there for detail."
  (with-temp-buffer
    (insert *str)
    (goto-char (point-min))
    (xah-replace-pairs-region-recursive (point-min) (point-max) *pairs)
    (buffer-string)))

(provide 'xah-replace-pairs)

;;; xah-replace-pairs.el ends here
