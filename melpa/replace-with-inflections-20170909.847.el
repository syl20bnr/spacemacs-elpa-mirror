;;; replace-with-inflections.el --- Inflection aware `query-replace'

;; Copyright (c) 2017 Akinori MUSHA
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;; Author: Akinori MUSHA <knu@iDaemons.org>
;; URL: https://github.com/knu/replace-with-inflections.el
;; Package-Version: 20170909.847
;; Created: 7 Seq 2017
;; Version: 0.2.2
;; Package-Requires: ((string-inflection "1.0.5") (inflections "1.1"))
;; Keywords: matching

;;; Commentary:
;;
;; This package currently provides the following function:
;;
;; * `query-replace-names-with-inflections'
;;
;; Tis is an inflection aware version of `query-replace'.  For
;; example, replacing "foo_bar" with "baz_quux" will also replace
;; "foo_bars" with "baz_quuxes", "FooBar" with "BazQuux", "FOO_BAR"
;; with "BAZ_QUUX", and so on.
;;
;; Read the docstring for details.
;;
;; For the term "inflection", refer to the following packages which this
;; library depends on:
;;
;; * inflections: URL `https://github.com/eschulte/jump.el'
;; * string-inflection: URL `https://github.com/akicho8/string-inflection'
;;
;; Here's my suggested settings:
;;
;;   (define-key search-map "n" 'query-replace-names-with-inflections)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'string-inflection)
(require 'inflections)

;; monkey-patch these until https://github.com/eschulte/jump.el/pull/13 is merged
(defadvice inflection-singularize-string
    (around save-match-data activate)
  (save-match-data ad-do-it))
(defadvice inflection-pluralize-string
    (around save-match-data activate)
  (save-match-data ad-do-it))

(defun replace-with-inflections--format-string-like (str model-str)
  "Format STR like MODEL-STR."
  (cond
   ((or (string-inflection-word-p model-str)
        (string-inflection-underscore-p model-str))
    (string-inflection-underscore-function str))
   ((string-inflection-upcase-p model-str)
    (string-inflection-upcase-function str))
   ((string-inflection-camelcase-p model-str)
    (string-inflection-camelcase-function str))
   ((string-inflection-lower-camelcase-p model-str)
    (string-inflection-lower-camelcase-function str))
   ((string-inflection-kebab-case-p model-str)
    (string-inflection-kebab-case-function str))
   (t
    str)))

(defun replace-with-inflections--singularize-string (str)
  (let* ((underscore (string-inflection-underscore-function str))
         (singular (replace-regexp-in-string "[^_]+\\'"
                                             #'inflection-singularize-string
                                             underscore)))
    (replace-with-inflections--format-string-like singular str)))

(defun replace-with-inflections--pluralize-string (str)
  (let* ((underscore (string-inflection-underscore-function str))
         (plural (replace-regexp-in-string "[^_]+\\'"
                                           #'inflection-pluralize-string
                                           underscore)))
    (replace-with-inflections--format-string-like plural str)))

;;;###autoload
(defun query-replace-names-with-inflections (from-string to-string &optional delimited start end)
  "\
Interactively replace various forms of FROM-STRING with those of TO-STRING.

Occurences of FROM-STRING in any of the underscore, upcase,
camelcase, lower-camelcase or kebab case forms will match, and
each replacement will be TO-STRING transformed to match the form
of the one matched.  If the pluralities of FROM-STRING and
TO-STRING match, both singular and plural forms of the
FROM-STRING variations will be replace with the corresponding
forms of TO-STRING.

For example, replacing \"foo_bar\" with \"baz_quux\" will also
replace \"foo_bars\" with \"baz_quuxes\", \"FooBar\" with
\"BazQuux\", \"FOO_BAR\" with \"BAZ_QUUX\", and so on.

For the term \"inflection\", refer to the following packages
which this library depends on:

* inflections: URL `https://github.com/eschulte/jump.el'
* string-inflection: URL `https://github.com/akicho8/string-inflection'

Third arg DELIMITED (prefix arg if interactive), if non-nil,
means replace only matches that are surrounded by symbol
boundaries.

Fourth and fifth arg START and END (active region if interactive)
specify the region to operate on."
  (interactive
   (let ((common
	  (query-replace-read-args
	   (concat "Query replace"
		   (if current-prefix-arg " symbol" " name")
		   (if (use-region-p) " in region" ""))
	   nil)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   (if (use-region-p) (region-beginning))
	   (if (use-region-p) (region-end)))))
  (let* ((from-singular (replace-with-inflections--singularize-string from-string))
         (from-plural (replace-with-inflections--pluralize-string from-string))
         (from-singular-p (string= from-string from-singular))
         (from-plural-p (string= from-string from-plural))
         (to-singular (replace-with-inflections--singularize-string to-string))
         (to-plural (replace-with-inflections--pluralize-string to-string))
         (to-singular-p (string= to-string to-singular))
         (to-plural-p (string= to-string to-plural))
         ;; If the pluraliries of FROM-STRING and TO-STRING do not
         ;; seem to match, disable support for number inflections.
         (number-inflection-p (or (and from-singular-p to-singular-p)
                                  (and from-plural-p to-plural-p)))
         (from-singular (if number-inflection-p from-singular from-string))
         (to-singular (if number-inflection-p to-singular to-string))
         (string-inflection-functions '(string-inflection-underscore-function
                                        string-inflection-upcase-function
                                        string-inflection-camelcase-function
                                        string-inflection-lower-camelcase-function
                                        string-inflection-kebab-case-function))
         (from-singulars (mapcar #'(lambda (func) (funcall func from-singular))
                                 string-inflection-functions))
         (from-plurals (if number-inflection-p
                           (mapcar #'(lambda (func) (funcall func from-plural))
                                   string-inflection-functions)))
         (regexp (regexp-opt (append from-plurals from-singulars)
                             (if delimited 'symbols t)))
         (re-singulars (concat "\\`" (regexp-opt from-singulars t) "\\'"))
         (orig-query-replace-descr (symbol-function 'query-replace-descr)))
    (letf (((symbol-function 'query-replace-descr)
            (lambda (string)
              (funcall orig-query-replace-descr
                       (if (string-equal string regexp)
                           (match-string 1) ;; show the matched name instead of the regexp pattern
                         string)))))
      (query-replace-regexp-eval regexp
                                 `(let ((matched (match-string 1)))
                                    (replace-with-inflections--format-string-like
                                     (if (string-match-p re-singulars matched)
                                         to-singular to-plural)
                                     matched))
                                 nil start end))))

(provide 'replace-with-inflections)
;;; replace-with-inflections.el ends here
