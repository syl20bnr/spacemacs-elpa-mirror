;;; www-synonyms.el --- insert synonym for a word  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Bernhard Specht

;; Author: Bernhard Specht <bernhard@specht.net>
;; Keywords: lisp
;; Package-Version: 20160812.1329
;; Version: 0.0.2
;; Package-Requires: ((request "0.2.0") (cl-lib "0.5"))

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

;; Lookup and insert synonyms for many different languages
;; Key for lookup authentication is needed: you can get it here: http://thesaurus.altervista.org/mykey
;; Internet connection is required
;; There are more packages for synonyms.  Why should I use this one?
;; More languages than other packages are supported at this time:
;;  - english (uk and us)
;;  - german
;;  - italian
;;  - french
;;  - spanish
;;  - russian
;;  - norwegian
;;  - portuguese
;;  - slovakian
;;  - romanian
;; Why should I use another one?
;; A stable internet connection is required

;;; Code:

(require 'request)
(require 'cl-lib)
(require 'json)

(defvar www-synonyms-lang "en_US")
(defvar www-synonyms-key "")

(defun www-synonyms--get-bounds ()
  "Get bounds of current region or symbol."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (bounds-of-thing-at-point 'symbol)))

(defun www-synonyms--format-candidates (response)
  "Parse synonyms from parse web json RESPONSE."
  (mapcar (lambda (c) (cons c (replace-regexp-in-string "\s*(.*?).*?" "" c)))
          (car (mapcar (lambda (res) (split-string (cdr (car (cdr (car res)))) "|"))
                       (cdr (assoc 'response response))))))

;;;###autoload
(defun www-synonyms-change-lang ()
  "Change language via LANG-PREFIX that synonyms are found for."
  (interactive)
  (completing-read "Language Prefix:" '(it_IT fr_FR de_DE en_US el_GR es_ES no_NO pt_PT ro_RO ru_RU sk_SK)))

(defun www-synonyms-insert-synonym ()
  "Insert/replace word with synonym."
  (interactive)
  (let* ((bounds (www-synonyms--get-bounds))
         (word   (when bounds
                     (buffer-substring-no-properties (car bounds) (cdr bounds)))))
    (setq word (read-string "Word: " word))
    (request
     "http://thesaurus.altervista.org/thesaurus/v1"
     :params `(("key"      . ,www-synonyms-key)
               ("language" . ,www-synonyms-lang)
               ("word"     . ,word)
               ("output"   . "json"))
     :parser 'json-read
     :sync t
     :error (cl-function (lambda (&key error-thrown &allow-other-keys)
                           (if (equal '(error http 403) error-thrown)
                               (message
                                "key: '%s' probably incorrect. Get new one from: 'http://thesaurus.altervista.org/mykey'"
                                www-synonyms-key)
                             (let ((lang-of-prefix '(("it_IT" . "italian")
                                                     ("fr_FR" . "french")
                                                     ("de_DE" . "german")
                                                     ("en_US" . "english (us)")
                                                     ("el_GR" . "english (gr)")
                                                     ("es_ES" . "spanish")
                                                     ("no_NO" . "norwegian")
                                                     ("pt_PT" . "portuguese")
                                                     ("ro_RO" . "romanian")
                                                     ("ru_RU" . "russian")
                                                     ("sk_SK" . "slovakian"))))
                               (message "no synonyms found in language: '%s'" (cdr (assoc www-synonyms-lang lang-of-prefix)))))))
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (let ((candidate (completing-read "Synonym:" (www-synonyms--format-candidates data)))
                       (bounds (www-synonyms--get-bounds)))
                   (when candidate
                     (when bounds
                       (delete-region (car bounds) (cdr bounds)))
                     (insert candidate))))))))

(provide 'www-synonyms)

;;; www-synonyms.el ends here
