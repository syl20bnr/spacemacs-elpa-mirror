;;; dictcc.el --- Look up translations on dict.cc  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Marten Lienen
;; Copyright (C) 2015 Raimon Grau
;;
;; Author: Marten Lienen <marten.lienen@gmail.com>
;; Version: 0.1.1
;; Package-Version: 20151221.357
;; Keywords: convenience
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (s "1.0") (dash "2.0") (helm "1.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Look up translations on dict.cc. You then pick one of them through a helm
;; interface and it is inserted at point.

;;; Code:

(require 'dash)
(require 's)
(require 'helm)
(require 'cl-lib)

(defgroup dictcc ()
  "Look up translations on dict.cc."
  :group 'convenience
  :group 'external
  :prefix "dictcc-")

(defcustom dictcc-candidate-width 30
  "Maximum length of a translation candidate."
  :type 'integer
  :group 'dictcc)

(defcustom dictcc-source-lang "en"
  "Source language."
  :type 'string
  :group 'dictcc)

(defcustom dictcc-destination-lang "de"
  "Destination language."
  :type 'string
  :group 'dictcc)

(cl-defstruct dictcc--translation text tags)

(defun dictcc--translation-from-cell (cell)
  "Create a dictcc--translation from the contents of CELL."
  (let ((words) (tags-nodes))
    ;; Split nodes in words and tag nodes
    (dolist (child (cddr cell))
      (when (listp child)
        (cl-case (car child)
          ('dfn (setq tags-nodes (cons child tags-nodes)))
          ('var (setq tags-nodes (cons child tags-nodes)))
          ('a
           (let ((inner-tag (cl-caddr child)))
             (if (and (listp inner-tag) (eq 'kbd (car inner-tag)))
                 (setq tags-nodes (cons child tags-nodes))
               (setq words (cons (dictcc--tag-to-text child) words))))))))
    (setq words (reverse words)
          tags-nodes (reverse tags-nodes))

    ;; Search for tags in a string form, so that we can more easily find tags,
    ;; that are split across tags (multi-word tags).
    (let* ((tag-strings (mapcar #'dictcc--tag-to-text tags-nodes))
           (tag-string (s-join " " tag-strings))
           (tags (dictcc--tags-from-string tag-string)))
      (make-dictcc--translation :text (s-join " " words)
                                :tags tags))))

(defun dictcc--tags-from-string (string)
  "Extract a list of tags from STRING.

This is implemented as a deterministic finite automaton, because
Emacs does not like my regexps."
  (let ((state 'initial) (pos 0) (start 0) (len (length string))
        (matches))
    (while (< pos len)
      (let ((char (aref string pos)))
        (cl-case state
          ('initial
           (cl-case char
             (?\s (setq pos (1+ pos)))  ; Eat up whitespace
             ((?\[ ?\{)
              (setq state 'pair
                    pos (1+ pos)
                    start pos))
             (t (setq state 'word
                      start pos))))
          ('word
           (cl-case char
             (?\s                       ; Space
              (setq matches (cons (substring string start pos) matches)
                    pos (1+ pos)
                    state 'initial))
             (t (setq pos (1+ pos)))))
          ('pair
           (cl-case char
             ((?\] ?\})
              (setq matches (cons (substring string start pos) matches)
                    pos (1+ pos)
                    state 'initial))
             (t (setq pos (1+ pos))))))))

    ;; Create a last match if the automaton halted in a matching state
    (when (or (eq state 'word) (eq state 'pair))
      (setq matches (cons (substring string start pos) matches)))

    (reverse matches)))

(defun dictcc--translation-to-string (translation)
  "Generate a string representation of TRANSLATION."
  (concat (dictcc--translation-text translation)
          " "
          (s-join " "
                  (mapcar (lambda (tag) (concat "[" tag "]"))
                          (dictcc--translation-tags translation)))))

(defun dictcc--request-url (query)
  "Generate a URL for QUERY."
  (format "http://%s%s.dict.cc/?s=%s"
          dictcc-source-lang
          dictcc-destination-lang
          (url-encode-url query)))

(defun dictcc--request (query)
  "Send the request to look up QUERY on dict.cc."
  (let ((buffer (current-buffer)))
    (url-retrieve (dictcc--request-url query)
                  (lambda (_log)
                    (let ((translations (dictcc--parse-http-response)))
                      (save-excursion
                        (switch-to-buffer buffer)
                        (dictcc--select-translation query translations)))))))

(defun dictcc--parse-http-response ()
  "Parse the HTTP response into a list of translation pairs."
  (search-forward "\n\n")
  (let* ((doc (libxml-parse-html-region (point) (point-max)))
         (rows (dictcc--find-translation-rows doc))
         (translations (mapcar #'dictcc--extract-translations rows)))
    translations))

(defun dictcc--find-translation-rows (doc)
  "Find all translation table rows in DOC.

At the moment they are of the form `<tr id='trXXX'></tr>'."
  (let ((rows nil)
        (elements (list doc)))
    (while elements
      (let ((element (pop elements)))
        (when (listp element)
          (let* ((tag (car element))
                 (attributes (cadr element))
                 (children (cddr element))
                 (id (cdr (assq 'id attributes)))
                 (is-translation
                  (and (eq tag 'tr)
                       (stringp id)
                       (string-equal (substring id 0 2) "tr"))))
            (if is-translation
                (push element rows)
              (dolist (child children)
                (push child elements)))))))
    rows))

(defun dictcc--extract-translations (row)
  "Extract translation texts from table ROW."
  (let* ((cells (cddr row)))
    (cons (dictcc--translation-from-cell (nth 1 cells))
          (dictcc--translation-from-cell (nth 2 cells)))))

(defun dictcc--tag-to-text (tag)
  "Concatenate the string contents of TAG and its children."
  (if (stringp tag)
      tag
    (let* ((children (cddr tag))
           (texts (mapcar #'dictcc--tag-to-text children)))
      (s-join "" texts))))

(defun dictcc--insert-source-translation (pair)
  "Insert the source translation of the selected PAIR."
  (insert (dictcc--translation-text (car pair))))

(defun dictcc--insert-destination-translation (pair)
  "Insert the destination translation of the selected PAIR."
  (insert (dictcc--translation-text (cdr pair))))

(defun dictcc--candidate (pair)
  "Generate the candidate pair for a PAIR of translations."
  (let* ((format-string (format "%%-%ds -- %%%ds"
                                dictcc-candidate-width
                                dictcc-candidate-width))
         (source (dictcc--cap-string (dictcc--translation-to-string (car pair))))
         (destination (dictcc--cap-string (dictcc--translation-to-string (cdr pair))))
         (text (format format-string source destination)))
    (cons text pair)))

(defun dictcc--cap-string (string)
  "Cut the STRING if it is too long."
  (if (> (length string) dictcc-candidate-width)
      (substring string 0 dictcc-candidate-width)
    string))

(defun dictcc--select-translation (query translations)
  "Select one from TRANSLATIONS and insert it into the buffer."
  (let* ((candidates (mapcar #'dictcc--candidate translations))
         (source `((name . ,(format "Translations for «%s»" query))
                   (candidates . ,candidates)
                   (action . ,(helm-make-actions
                               (format "Insert %s translation" dictcc-source-lang)
                               #'dictcc--insert-source-translation
                               (format "Insert %s translation" dictcc-destination-lang)
                               #'dictcc--insert-destination-translation)))))
    (helm :sources (list source))))

;;;###autoload
(defun dictcc (query)
  "Search dict.cc for QUERY and insert a result at point."
  (interactive "sQuery: \n")
  (dictcc--request query))

(provide 'dictcc)
;;; dictcc.el ends here
