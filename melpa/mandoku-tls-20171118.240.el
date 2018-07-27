;;; mandoku-tls.el --- A tool to access the TLS database
;; -*- coding: utf-8 -*-
;; created [2016-02-10T21:02:54+0900]
;;
;; Copyright (c) 2016-2017 Christian Wittern
;;
;; Author: Christian Wittern <cwittern@gmail.com>
;; URL: https://github.com/mandoku/mandoku-tls
;; Package-Version: 20171118.240
;; Version: 0.1
;; License: GPL v3, or any later version
;; Keywords: convenience
;; Package-Requires: ((emacs "24.4") (mandoku "20170301") (github-clone "0.2") (hydra "0.13.6") (helm "1.7.0") (org "9.0") (helm-charinfo "20170601"))
;; This file is not part of GNU Emacs.


;;; Commentary:
;; 

(require 'mandoku)
(require 'org)
(require 'mandoku-dict)
(require 'mandoku-annot)
(require 'hydra)
(require 'helm-charinfo)
(require 'subr-x)

;;; Code:

(defgroup mandoku-tls nil
  "Emacs interface to the TLS database."
  :group 'external)

(defconst mandoku-tls-root-path
  (or mandoku-base-dir
      (replace-in-string (file-name-directory (or byte-compile-current-file
                                             load-file-name)))))
(defcustom mandoku-tls-lexicon-path
  (concat mandoku-tls-root-path "tls/lexicon/")
  "Path to TLS lexicon."
  :type 'string
  :group 'mandoku-tls)

(defcustom mandoku-tls-text-path
  mandoku-text-dir
  "Path to TLS texts, this should generally be the same as the `mandoku-text-dir`."
  :type 'string
  :group 'mandoku-tls)

(defcustom mandoku-tls-chant-path
  (concat mandoku-tls-root-path "chant/")
  "Path to the CHANT texts, this should generally be on the same level as the TLS lexicon."
  :type 'string
  :group 'mandoku-tls)

(defcustom mandoku-tls-concept-template
  (concat mandoku-tls-lexicon-path "core/concept-template.org")
  "Filename for template for concepts."
  :type 'string
  :group 'mandoku-tls)
  
(defcustom mandoku-tls-types
  '("tls"
    "syn-func"  ;; syntactic functions
    "sem-feat"  ;; semantic features
    )
  "List of types known in `tls'."
  :type '(repeat :tag "List of tls types" string)
  :group 'mandoku-tls)

(defvar mandoku-tls-punc-regex "\\([　-㏿＀-￯]\\)")

;; core lexicon
(defvar mandoku-tls-zhu-path (concat mandoku-tls-lexicon-path "notes/zhu/")
  "The 'zhu' notes compiled from the TLS texts are stored here.")
(defvar mandoku-tls-zhu-tab-buffer nil
  "This is the file for writing out the index of zhu, the name will usually be the same as given in `mandoku-tls-syn-word-locs`.")
(defvar mandoku-tls-syn-word-locs (concat mandoku-tls-lexicon-path "index/swl.txt")
  "The syntactical word locations, an index to the notes in notes/zhu/*.zhu.")
;(defvar mandoku-tls-syn-words (concat mandoku-tls-lexicon-path "core/syntactic-words.org") "")
(defvar mandoku-tls-syn-func-org (concat mandoku-tls-lexicon-path "core/syn-func.org")
  "Syntactic functions in TLS.")
(defvar mandoku-tls-syn-func-index (concat mandoku-tls-lexicon-path "index/syn-func.txt")
  "Index to syntactic functions.")
(defvar mandoku-tls-sem-feat-org (concat mandoku-tls-lexicon-path "core/sem-feat.org")
  "Semantic features in TLS.")
(defvar mandoku-tls-rhet-dev-org (concat mandoku-tls-lexicon-path "core/rhet-dev.org")
  "Rhetorical devices in TLS.")
(defvar mandoku-tls-word-rel-org (concat mandoku-tls-lexicon-path "core/word-rel.org")
  "Word relations in TLS.")
(defvar mandoku-tls-comp-types-org (concat mandoku-tls-lexicon-path "core/compound-types.org")
  "Compound types in TLS.")
(defvar mandoku-tls-syllables-org (concat mandoku-tls-lexicon-path "core/syllables.org") "Syllables.")
(defvar  mandoku-tls-syllables-index (concat mandoku-tls-lexicon-path "index/syllables.txt") "Index to syllables.")
(defvar mandoku-tls-taxonomy-file nil)
(defvar mandoku-tls-initialized-p nil "Say whether TLS has been initialized.")


(defvar mandoku-tls-swl nil
  "Hash table of syntax word locations.")
(defvar mandoku-tls-character-tab nil "Hash table for character radical and strokecount.")
(defvar mandoku-tls-words nil
  "Hash table of syntax words.")
(defvar mandoku-tls-concepts nil
  "Hash table index for concepts.")
(defvar mandoku-tls-concept-words nil
  "Hash table for words in concepts.")
(defvar mandoku-tls-pos-info nil
  "Hash table of POS info, such as syntactic function or semantic feature occurrences in `mandoku-tls-words'.")
(defvar mandoku-tls-syn-func-tab nil
  "Hash table of syn-func file for helm.")
(defvar mandoku-tls-syllables-tab nil "Syllables.")
(defvar mandoku-tls-syllables-uuid nil "Syllables by uuid.")
(defvar mandoku-tls-inp-ok-p t "Are we allowed to input?")
(defvar mandoku-tls-inp nil)
(defvar mandoku-tls-current-char "逳")
(defvar mandoku-tls-chant-titles nil "Title table for CHANT titles.")

(defun mandoku-tls-read-characters-tab ()
  "Read the character table."
  (setq mandoku-tls-character-tab (make-hash-table :test 'equal))
  (let ((ct (concat mandoku-tls-lexicon-path "core/characters.tab")))
  (when (file-exists-p ct)
      (with-current-buffer (find-file-noselect ct)
	(dolist (l (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))
	  (let* ((tmp (split-string l "\t"))
		 (char (format "%c" (string-to-number (nth 0 tmp) 16))))
	    (puthash char (format "(%s,%s/%s)" (nth 2 tmp) (nth 3 tmp) (nth 4 tmp) )
		     mandoku-tls-character-tab)))))))

(defun mandoku-tls-read-syllables-org ()
  "Read the syllables table."
  (setq mandoku-tls-syllables-tab (make-hash-table :test 'equal))
  (setq mandoku-tls-syllables-uuid (make-hash-table :test 'equal))
  (message "Starting to read the syllables.")
  (if (file-exists-p mandoku-tls-syllables-index )
      (with-current-buffer (find-file-noselect mandoku-tls-syllables-index )
	(dolist (l (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))
	  (let* (syll
		 (tmp (split-string l "\t"))
		 (char (nth 0 tmp)))
            (setq syll (plist-put syll :char char))
	    (setq syll (plist-put syll :uuid (nth 1 tmp)))
	    (setq syll (plist-put syll :pinyin (nth 2 tmp)))
	    (setq syll (plist-put syll :oc (nth 3 tmp)))
	    (setq syll (plist-put syll :mc (nth 4 tmp)))
	    (setq syll (plist-put syll :fqshang (nth 5 tmp)))
	    (setq syll (plist-put syll :fqxia (nth 6 tmp)))
	    (setq syll (plist-put syll :diao (nth 7 tmp)))
	    (setq syll (plist-put syll :gloss (nth 8 tmp)))
            (puthash (nth 1 tmp) syll mandoku-tls-syllables-uuid)
	    (if (gethash char mandoku-tls-syllables-tab)
		(puthash char (cons syll (gethash char mandoku-tls-syllables-tab)) mandoku-tls-syllables-tab)
	      (puthash char (cons syll nil) mandoku-tls-syllables-tab))))
	(kill-buffer))
    (when (file-exists-p mandoku-tls-syllables-org)
      (with-current-buffer (find-file-noselect mandoku-tls-syllables-org t)
	(fundamental-mode)
	(goto-char (point-min))
	(while (search-forward "* =SYLLABLE=" nil t)
	  (mandoku-tls-get-sylable-item))
	(kill-buffer))))
  (when (and (not (file-exists-p mandoku-tls-syllables-index ))
	     mandoku-tls-syllables-tab)
    (with-current-buffer (find-file-noselect mandoku-tls-syllables-index )
      (maphash (lambda (k vv)
		 (dolist (v vv)
		 (insert
		  (format "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n"
			  k
			  (plist-get v :uuid)
			  (plist-get v :pinyin)
			  (plist-get v :oc)
			  (plist-get v :mc)
			  (plist-get v :fqshang)
			  (plist-get v :fqxia)
			  (plist-get v :diao)
			  (plist-get v :gloss)))))
	       mandoku-tls-syllables-tab)
      (save-buffer)
      (kill-buffer)))
  (message "Finished reading the syllables."))


(defun mandoku-tls-get-sylable-item ()
  "."
  (save-excursion
  (let* ((hw (split-string (mandoku-tls-get-special-line-rest (point) "= ") ))
	 (ch (car hw))
	(uuid (mandoku-tls-get-special-line-rest (save-excursion (search-forward "CUSTOM_ID:" nil t)) ": "))
	syll cat
	beg end )
    (message ch)
    (setq syll (plist-put syll :pinyin (cadr hw )))
    (setq syll (plist-put syll :uuid uuid))
    (search-forward "GLOSS" nil t)
    (forward-line)
    (setq syll (plist-put syll :gloss (mandoku-trim-and-star (thing-at-point 'line t))))
    (search-forward "FANQIE-SHANGZI:" nil t)
    (setq syll (plist-put syll :fqshang (mandoku-tls-get-special-line-rest (point) ":\t") ))
    (search-forward "FANQIE-XIAZI:" nil t)
    (setq syll (plist-put syll :fqxia (mandoku-tls-get-special-line-rest (point) ":\t")))
    (search-forward " CATEGORIES" nil t)
    (forward-line)
    (while (looking-at " - ")
      (setq cat (concat cat (thing-at-point 'line t)))
      (when (looking-at " - 調:")
        (setq syll (plist-put syll :diao (mandoku-tls-get-special-line-rest (point) ":\t"))))
      (forward-line))
    (setq syll (plist-put syll :cat cat))
    (search-forward "YUNDIANWANG-RECONSTRUCTIONS" nil t)
    (search-forward "PAN-WUYUN:" nil t)
    (setq syll (plist-put syll :mc (mandoku-tls-get-special-line-rest (point) ":\t")))
    (search-forward " OLD-CHINESE" nil t)
    (search-forward " OC:" nil t)
    (setq syll (plist-put syll :oc (mandoku-tls-get-special-line-rest (point) ":\t")))
    (puthash uuid syll mandoku-tls-syllables-uuid)
    (dolist (char (mapcar 'char-to-string (string-to-list ch)))
      (if (gethash char mandoku-tls-syllables-tab)
	  (puthash char (cons syll (gethash char mandoku-tls-syllables-tab)) mandoku-tls-syllables-tab)
	(puthash char (cons syll nil) mandoku-tls-syllables-tab))))))

(defun mandoku-tls-read-syn-func-org ()
  "Read the syntactic functions table."
  (setq mandoku-tls-syn-func-tab (make-hash-table :test 'equal))
  (message "Starting to read the syntactical functions.")
  ;(measure-time
  (if (file-exists-p mandoku-tls-syn-func-index )
      (with-current-buffer (find-file-noselect mandoku-tls-syn-func-index )
	(dolist (l (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n$$\n"))
	  (let* ((tmp (split-string l "\t")))
	    (when (> (length tmp) 3)
	      (puthash (nth 0 tmp) (list :name (nth 1 tmp) :def (nth 2 tmp) :pointers (read (nth 3 tmp))) mandoku-tls-syn-func-tab))))
	(kill-buffer))
    (when (file-exists-p mandoku-tls-syn-func-org)
      (let (pos)
	(with-current-buffer (find-file-noselect mandoku-tls-syn-func-org t)
	  (fundamental-mode)
	  (goto-char (point-min))
	  (while (search-forward "* =SYNTACTIC FUNCTION=" nil t)
	    (setq pos (match-end 0))
	    (mandoku-tls-get-syn-func-item)
	    (goto-char pos))
	(kill-buffer)))))
  (when (and (not (file-exists-p mandoku-tls-syn-func-index ))
	     mandoku-tls-syn-func-tab)
    (with-current-buffer (find-file-noselect mandoku-tls-syn-func-index )
      (maphash (lambda (k v)
		 (insert
		  (format "%s\t%s\t%s\t%S\n$$\n"
			  k
			  (plist-get v :name)
			  (plist-get v :def)
			  (plist-get v :pointers)
			  )))
		  mandoku-tls-syn-func-tab)
		 (save-buffer)
		 (kill-buffer)))
  (message "Finished reading the syntactical functions."))

(defun mandoku-tls-get-special-line-rest (p sp)
  "Avoid using the corresponding org function.  Argument P The value to be split.  SP is the split-value."
  (save-excursion
    (goto-char p)
    (mandoku-trim-and-star (cadr (split-string (thing-at-point 'line t) sp)))))

(defun mandoku-tls-get-syn-func-item ()
  "This extracts the syn-func from an entry in the syn-func.org file and adds it to mandoku-tls-syn-func-tab."
  (save-excursion
    (let ((cp (point))
	  (hw (mandoku-tls-get-special-line-rest (point) "= "))
	  (uuid (mandoku-tls-get-special-line-rest (save-excursion (search-forward "CUSTOM_ID:" nil t)) ": "))
	  (eot (org-end-of-subtree))
	  beg end def pointers)
      (goto-char cp)
      (search-forward "** DEFINITION" nil t)
      (next-line 1)
      (setq beg (point))
      (search-forward "\n*" nil t)
      (setq end (- (match-beginning 0) 1))
      (setq def  (mandoku-trim-and-star (buffer-substring-no-properties beg end)))
    ;; get pointers
      (goto-char cp)
      (when (search-forward "* TAXONOMY" eot t)
	(setq pointers (mandoku-tls-read-synlink-list)))
      (puthash uuid (list :name hw :def def :pointers pointers) mandoku-tls-syn-func-tab)))
  )
;))

(defun mandoku-tls-read-synlink-list ()
  "This is called with point at the beginning of a list of links."
  (save-excursion
    (let ((p (point))
	  (limit (org-end-of-subtree))
	  m1 m3
	  l )
      (goto-char p)
      (while (re-search-forward org-bracket-link-regexp limit t)
	(setq m1 (org-match-string-no-properties 1))
	(setq m3 (org-match-string-no-properties 3))
	(push (cons (substring m1 1) m3) l))
      (reverse l))))
  
(defun mandoku-tls-read-zhu-to-swl-file ()
  "We set mandoku-tls-zhu-tab-file to a file we want to write to, then call `mandoku-tls-read-zhu-dir'."
  (setq mandoku-tls-zhu-tab-buffer (find-file-noselect mandoku-tls-syn-word-locs))
  (with-current-buffer mandoku-tls-zhu-tab-buffer
    (erase-buffer))
  (mandoku-tls-read-zhu-dir))

(defun mandoku-tls-read-zhu-dir (&optional dir)
  "This reads all files in the zhu directory into a hashtable.
The reading is done by mandoku-tls-read-zhu-file.
Optional argument DIR Use DIR instead of the default directory."
  (let ((d (or dir mandoku-tls-zhu-path)))
    (unless mandoku-tls-zhu-tab-buffer
      (setq mandoku-tls-swl (make-hash-table :test 'equal)))
    (measure-time
     (dolist (f (directory-files d t "\\.zhu$"))
       (message f)
       (mandoku-tls-read-zhu-file f)))))


(defun mandoku-tls-read-zhu-file (file)
  ".
Argument FILE The file to be read."
  (when (file-exists-p file)
    (with-current-buffer (find-file-noselect file)
      (org-map-entries 'mandoku-tls-get-zhu-item "+LEVEL<=2" nil)
      (kill-buffer))))

(defun mandoku-tls-get-zhu-item-debug ()
  "First find the concepts with not uuid."
  (while (search-forward "*** " nil t)
    (unless (org-entry-get (point) "CONCEPT")
      (message (format "%s %s" (buffer-file-name)  (point))))))

(defun mandoku-tls-get-zhu-item ()
  (let ((hl (substring-no-properties (org-get-heading t t)))
	(p (point))
	(src (org-entry-get (point) "SRC"))
	(date (org-entry-get (point) "DATE"))
	(loc  (split-string (org-entry-get (point) "LOCATION") " / "))
	(end (org-end-of-subtree))
	concept line char
	)
    (goto-char p)
    (while (search-forward "*** " end t)
      (when (setq concept (org-entry-get (point) "CONCEPT"))
	(setq concept (car (split-string concept  " / ")))
	(setq char (substring-no-properties (org-get-heading t t)))
	;(message concept)
	(setq line (plist-put line :line (concat "[[mandoku:" (car loc) "][" (cadr loc) "]]\t" hl)))
	(setq line (plist-put line :date date))
	(setq line (plist-put line :src src))
	;; we either load in the table or write to file
	(if mandoku-tls-zhu-tab-buffer
	    (with-current-buffer mandoku-tls-zhu-tab-buffer
	      (insert
	       (format "%s\t%s##%s##%s##%s##%s\n"
		       concept (string-join loc "##") date src char hl
		       )))
	  (if (gethash concept mandoku-tls-swl)
	      (puthash concept (cons line (gethash concept mandoku-tls-swl))  mandoku-tls-swl)
	    (puthash concept (cons line nil)  mandoku-tls-swl))))
    )))

(defun mandoku-tls-read-swl ()
  "Read the syntactic word locations table."
  (setq mandoku-tls-swl (make-hash-table :test 'equal))
  (when (file-exists-p mandoku-tls-syn-word-locs)
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8)
	    textid)
	(insert-file-contents mandoku-tls-syn-word-locs) ;
	(goto-char (point-min))
	(while (re-search-forward "^\\(uuid[^\t]+\\)\t\\([^
]+\\)$" nil t)
	  (let* ((uuid (match-string 1))
		(rest (match-string 2))
		(loc (split-string rest "##"))
		(textid (car loc))
		(title (cadr loc))
		(date (nth 2 loc))
		(src (nth 3 loc))
		line
		tchar)
	    (setq line (plist-put line :line (concat "[[mandoku:" textid "][" title "]]\t" (nth 5 loc))))
	    (setq line (plist-put line :date date))
	    (setq line (plist-put line :src src))
	    (if (gethash uuid mandoku-tls-swl)
		(puthash uuid (cons line (gethash uuid mandoku-tls-swl))  mandoku-tls-swl)
	      (puthash uuid (cons line nil)  mandoku-tls-swl))
	    ))))))

;; this is obsolete and not used anymore, see mandoku-tls-read-concepts
;; (defun mandoku-tls-read-synwords ()
;;   "read the syntactic words table"
;;   (setq mandoku-tls-words (make-hash-table :test 'equal))
;;   (setq mandoku-tls-pos-info (make-hash-table :test 'equal))
;;   (when (file-exists-p mandoku-tls-syn-words)
;;     (with-temp-buffer
;;       (let ((coding-system-for-read 'utf-8)
;; 	    textid)
;; 	(insert-file-contents mandoku-tls-syn-words) ;
;; 	(goto-char (point-min))
;; 	(while (re-search-forward "^\\([^
;; ]+\\)\t\\([^\t
;; ]+\\)\t\\([^\t
;; ]+\\)$" nil t)
;; 	  (let ((concept (match-string 2))
;; 		(posdef (match-string 3))
;; 		(charpin (split-string (match-string 1) " / "))
;; 		tchar type)
;; 	    (mandoku-tls-add-to-swl-hash concept posdef charpin)
;; 	    ;; there could be duplicates? remove them
;; 	    ))))))

(defun mandoku-tls-add-to-swl-hash (concept posdef charpin)
  (dolist (c (delq nil (delete-dups charpin)))
    (setq tchar (car (split-string c "[ @]")))
    (if (< 0 (length tchar))
	(if (gethash tchar mandoku-tls-words)
	    (puthash tchar (cons (list (cadr (split-string c "[ @]")) concept posdef) (gethash tchar mandoku-tls-words))  mandoku-tls-words)
	  (puthash tchar (cons (list (cadr (split-string c "[ @]")) concept posdef) nil) mandoku-tls-words))))
  ;; pos-info
  (when (string-match "tls:" posdef)
  (while (string-match "\\[\\[tls:\\([^:]+\\)::#\\([^\]]+\\)\\]" posdef (match-end 0))
    (setq type (match-string 1 posdef))
    (setq tchar (match-string 2 posdef))
    (when (member type mandoku-tls-types)
      (if (gethash tchar mandoku-tls-pos-info)
					;(puthash tchar (cons (list :type type (replace-in-string (mapconcat 'identity charpin " ") "@" " ") concept posdef) (gethash tchar mandoku-tls-pos-info))  mandoku-tls-pos-info)
	  (puthash tchar (cons (list (replace-in-string (mapconcat 'identity charpin " ") "@" " ") concept posdef) (gethash tchar mandoku-tls-pos-info))  mandoku-tls-pos-info)
	(puthash tchar (cons (list (replace-in-string (mapconcat 'identity charpin " ") "@" " ") concept posdef) nil)  mandoku-tls-pos-info)
	(message tchar)
	)))))

(defun mandoku-tls-read-concepts ()
  "Read the concept files into the hash table."
  ;; all these hashtables are populated here from the concepts files
  (setq mandoku-tls-concepts (make-hash-table :test 'equal))
  (setq mandoku-tls-concept-words (make-hash-table :test 'equal))
  (setq mandoku-tls-pos-info (make-hash-table :test 'equal))
  (setq mandoku-tls-words (make-hash-table :test 'equal))
  (dolist (file (directory-files (concat mandoku-tls-lexicon-path "concepts") nil ".*org$" ))
    (message file)
    (mandoku-tls-read-concept-file (concat mandoku-tls-lexicon-path "concepts/" file)))
  (mandoku-tls-concepts-add-uplink))

;;[2017-04-20T15:41:32+0900] TODO: also need to add up-links to those in "N/A" that do not have an uplink yet...
(defun mandoku-tls-concepts-add-uplink ()
  "Now add the uplink to those concepts referenced in \"NARROW CONCEPTS\"."
  (maphash (lambda (k v)
	     (dolist (p (cdr (assoc "TAXONOMY" (plist-get v :pointers))))
	       (setq h (gethash p mandoku-tls-concepts))
	       (if h
		   (if (plist-get h :up-tax)
		       (puthash p (plist-put h :up-tax (cons k (plist-get h :up-tax))) mandoku-tls-concepts)
		     (puthash p (plist-put h :up-tax (cons k nil)) mandoku-tls-concepts)))))
	       mandoku-tls-concepts)
  (maphash (lambda (k v)
	     (dolist (p (cdr (assoc "MERENOMY" (plist-get v :pointers))))
	       (setq h (gethash p mandoku-tls-concepts))
	       (if h
		   (if (plist-get h :up-met)
		       (puthash p (plist-put h :up-met (cons k (plist-get h :up-met))) mandoku-tls-concepts)
		     (puthash p (plist-put h :up-met (cons k nil)) mandoku-tls-concepts)))))
	       mandoku-tls-concepts)
)

(defun mandoku-tls-read-concept-file (&optional file)
  "Read one concept FILE into the hash tables.
When called without argument, use the current buffer file."
  (setq file (or file (buffer-file-name)))
  (when (file-exists-p file)
    (with-temp-buffer
      ;(display-buffer (current-buffer))
      (let ((coding-system-for-read 'utf-8)
	    (name (file-name-sans-extension (file-name-nondirectory file)))
	    pointers words plist h)
	(insert-file-contents file)
	(org-mode)
	(goto-char (point-min))
	(set (make-local-variable 'concept-name) name)
	(when (search-forward "\n* =CONCEPT=" nil t)
	  (setq plist (plist-put plist :synonyms (delq nil (org--property-local-values "SYNONYM" nil))))
	  (when (search-forward "\n** POINTERS" nil t)
	    (org-next-visible-heading 1)
	    (while (= 3 (org-outline-level))
	      (push (mandoku-tls-read-subtree-list) pointers)
	      (org-next-visible-heading 1)))
	  (goto-char (point-min))
	  (when (search-forward "\n** WORDS" nil t)
	    (while  (search-forward "\n*** " nil t)
	      (let* ((hd (org-get-heading))
		     (char (car (split-string hd)))
		     (hw (mapcar
			  (lambda (str)
			    (mandoku-trim-and-star (car (split-string str "("))))
			  (split-string hd " / ")))
		     (uuid (org-entry-get (point) "CUSTOM_ID"))
		     (gyids (org-entry-get (point) "GY_IDS"))
		     (comment-start-skip ";+ *")
                     (cnt 0)
                     gy
                     )
                (dolist (h hw)
                  (push (cons (car (split-string h))
                              (if gyids
                                  (nth cnt (split-string gyids))
                                "no_gyid"))
                        words)
                  (setq cnt (+ 1 cnt)))
		;; this adds the sw to the hashtable
		(set (make-local-variable 'concept-word) hw)
		(org-map-entries 'mandoku-tls-add-to-hash "+LEVEL==4" 'tree)
		(puthash uuid (list name char) mandoku-tls-concept-words)
	      )))
	  ;; and finally add the data to the hashtable
	  (setq plist (plist-put plist :pointers (reverse pointers)))
	  (setq plist (plist-put plist :words words))
	  (setq h (gethash name mandoku-tls-concepts))
	  (setq plist (plist-put plist :up-tax (plist-get h :up-tax)))
	  (setq plist (plist-put plist :up-met (plist-get h :up-met)))
	  (puthash name plist mandoku-tls-concepts))
	(kill-buffer)))))

(defun mandoku-tls-add-to-hash ()
  (let* ((uuid (org-entry-get (point) "CUSTOM_ID"))
	;; this assumes that the heading is current.
	;; TODO: write a tls-lint function that checks this kind of stuff
	(posdef (org-get-heading))
	(concept (format "[[tls:concept:%s::#%s][%s]]" concept-name uuid concept-name))
	(line (format "%s\t%s\t%s" (mapconcat 'identity concept-word " / ") concept posdef)))
    (when uuid
      (mandoku-tls-add-to-swl-hash concept posdef concept-word))
  ))

(defun mandoku-tls-read-subtree-list ()
  (save-excursion
    (let ((p (point))
	  (limit (org-end-of-subtree))
	  l )
      (goto-char p)
      (push (org-get-heading) l)
      (while (re-search-forward "tls:concept:\\([^\]]+\\)\]" limit t)
	(push (org-match-string-no-properties 1) l))
      (reverse l))))

(defun mandoku-tls-add-uplink-to-concepts ()
  "Read the mandoku-tls-concept table and add the uplinks to the concepts mentioned."
  (unless mandoku-tls-initialized-p
    (mandoku-tls-initialize))
  (let (l1 l2)
    (maphash (lambda (k v)
	       (when (plist-get v :up-tax)
		 (setq x (plist-get v :up-tax))
		 (setq l1 (cons (list k x) l1 )))) mandoku-tls-concepts)
    (dolist (e l1)
      (message (car e))
      (with-current-buffer (find-file-noselect (concat mandoku-tls-lexicon-path "/concepts/" (car e) ".org") t)
	(when (search-forward "\n** POINTERS" nil t)
	  (org-insert-subheading '(4))
	  ;; wrong, I need to differentiate between types of pointers here!
	  (insert "KIND OF\n")
	  (dolist (f (delete-dups (cadr e)))
	    (insert (format  " - [[tls:concept:%s][%s]]\n" f f)))
	  (save-buffer)
	  (kill-buffer))))
  ;;met
    (maphash (lambda (k v)
	       (when (plist-get v :up-met)
		 (setq x (plist-get v :up-met))
		 (setq l2 (cons (list k x) l2 )))) mandoku-tls-concepts)
    (dolist (e l2)
      (message (car e))
      (with-current-buffer (find-file-noselect (concat mandoku-tls-lexicon-path "/concepts/" (car e) ".org") t)
	(when (search-forward "\n** POINTERS" nil t)
	  (org-insert-subheading '(4))
	  ;; wrong, I need to differentiate between types of pointers here!
	  (insert "PART OF\n")
	  (dolist (f (delete-dups (cadr e)))
	    (insert (format  " - [[tls:concept:%s][%s]]\n" f f)))
	  (save-buffer)
	  (kill-buffer)))))
)

(defun mandoku-tls-helm-concepts-candidates ()
  "Helm source for concepts."
  (let (l x)
    (maphash (lambda (k v)
	       (push k l)
	       (when (plist-get v :synonyms)
		 (dolist (x (plist-get v :synonyms))
		   (push (concat x "->" k) l))))
	        mandoku-tls-concepts)
    (sort l 'string-lessp)
  )
  )

(defun mandoku-tls-helm-tree-candidates (concept)
  "Helm source for CONCEPT tree."
  (let (l x)
    (setq x concept)
    (push x l)
    (while (gethash x mandoku-tls-concepts)
      (setq x (car (plist-get (gethash x mandoku-tls-concepts) :up)))
      (push x l)
      )
					;(sort (delq nil  l) 'string-lessp)
    
    (delq nil l))
  )

(defvar mandoku-tls-helm-source
  '((name . "TLS Concepts")
    (fuzzy-match . t)
    (multimatch . t)
    (candidates . mandoku-tls-helm-concepts-candidates)
    (action . (("Open" . (lambda (candidate)
			   (setq mandoku-tls-concept (or (cadr (split-string candidate "->")) candidate))
			   (find-file
			    (concat mandoku-tls-lexicon-path "concepts/" mandoku-tls-concept ".org") t)
			   (message "%s" candidate)))
	       ("Copy link" . (lambda (candidate)
				(setq mandoku-tls-concept (or (cadr (split-string candidate "->")) candidate))
				(kill-new (concat "[[tls:concept:"  mandoku-tls-concept "][" mandoku-tls-concept "]]"))
				))))))

(defun mandoku-tls-concepts-helm ()
  (interactive)
  (unless mandoku-tls-initialized-p
    (mandoku-tls-initialize))
  (helm :sources (list mandoku-tls-helm-source
		       (helm-build-dummy-source "New concept:"
			 :action '(("Create new concept:" . mandoku-tls-create-new-annot))))))

(defhydra hydra-tls-view (:color blue :exit t)
  "TLS functions\n"
  ("c" mandoku-tls-concepts-helm "Lookup TLS concepts by name\n")
  ("g" mandoku-tls-grep "Search for selected word in the text files\n")
  ("r" mandoku-tls-select-readings "TLS: Select readings\n")
  ("s" mandoku-tls-syn-func-helm "Lookup TLS syntactic functions\n" )
  ("w" mandoku-tls-show-words "TLS: Show attributions for this word.\n")
;  ("a" mandoku-tls-make-attribution "TLS: Make new attribution\n" )
;  ("n" mandoku-tls-new-swl "TLS: Assosiate new annotation with location in text\n" )
  ("z" mandoku-tls-new-syntactic-word "TLS: Make new syntactic word" )
;  ("i" mandoku-tls-insert-new-annot "TLS insert new annotation\n" )
)

(defhydra hydra-tls-dict (:color blue :exit t)
  "TLS functions in dictionary mode\n"
  ("h" mandoku-tls-dict-immediate-capture "Attribute citation in HYDCD: Put cursor before the + !" ))
;(define-key mandoku-dict-mode-map (kbd "<f8>") 'hydra-tls-dict/body)
(define-key mandoku-dict-mode-map (kbd "<f8>") 'hydra-tls/body)
   
(defhydra hydra-tls (:color blue :exit t)
  "TLS functions\n"
  ("c" mandoku-tls-concepts-helm "Lookup TLS concepts by name\n" )
  ("g" mandoku-tls-grep "Search for selected word in the text files\n" )
  ("n" mandoku-tls-new-concept-maybe "TLS: Add word to concept (maybe create new one)\n" )
  ("r" mandoku-tls-select-readings "TLS: Select readings\n")
  ("l" mandoku-tls-procline "TLS: Lookup all characters/terms on line\n" )
  ("v" mandoku-tls-procline-short "TLS: Overview of concepts for characters on line\n" )
  ("s" mandoku-tls-syn-func-helm "Lookup TLS syntactic functions\n" )
  ("a" mandoku-tls-add-attribution-for-char "Add attribution for selected word")
;  ("a" mandoku-tls-make-attribution "TLS: Make new attribution\n" )
;  ("n" mandoku-tls-new-swl "TLS: Assosiate new annotation with location in text\n" )
;  ("z" mandoku-tls-new-syntactic-word "TLS: Make new syntactic word\n" )
;  ("i" mandoku-tls-insert-new-annot "TLS insert new annotation\n")
)

(defun mandoku-tls-helm-syn-func-candidates ()
  "Helm source for syntactic function."
  (let (l x)
    (maphash (lambda (k v)
	       (when (plist-get v :name)
		 (setq l (cons (list (plist-get v :name) k) l)))
	       ) mandoku-tls-syn-func-tab)
   (sort l 'mandoku-tls-sort-by-car)
   ))

(defun mandoku-tls-sort-by-car (s1 s2)
  (string-lessp (car s1) (car s2))
  )

(defun mandoku-tls-syn-func-helm ()
  (interactive)
  (let ((mandoku-tls-syn-func-source
	 '((name . "TLS Syntactic functions")
	   (fuzzy-match . t)
	   (candidates . mandoku-tls-helm-syn-func-candidates)
	   (action . (("Open" . (lambda (candidate)
			       (org-open-file
				(concat mandoku-tls-lexicon-path "core/syn-func.org") t nil
				(concat "#" (car candidate)))))
		      ("Copy link" . (lambda (candidate)
				    (kill-new (concat "[[tls:syn-func:#" (car candidate) "]["
						      (plist-get (gethash (car candidate) mandoku-tls-syn-func-tab) :name) "]]" )))))
		
		   ))))
    (helm :sources '(mandoku-tls-syn-func-source)
	  )))


;;;###autoload
(defun mandoku-tls-initialize ()
  (interactive)
  (if (file-exists-p mandoku-tls-lexicon-path)
      (unless mandoku-tls-initialized-p
	(message "Reading the TLS database.  This will take a moment.")
	(mandoku-tls-read-swl)
	(mandoku-tls-read-characters-tab)
					;(mandoku-tls-read-synwords)
	(mandoku-tls-read-concepts)
	(mandoku-tls-read-syn-func-org)
	(mandoku-tls-read-syllables-org)
	(setq mandoku-tls-initialized-p t)
	(message "Finished reading the TLS database.  Ready to go!"))
    (when (yes-or-no-p
	   "TLS database not found. Do you want to get it now? ")
      (mandoku-clone-repo "tls-kr/tls-org" (concat mandoku-tls-root-path "tls"))
      (mandoku-tls-initialize)
	))
)

(defun mandoku-tls-show-words (&optional uuid-hw)
  "Show the words related to the current heading.
Optional argument UUID-HW Use a different uuid than the one at point."
  (interactive)
  (if (or uuid-hw (looking-at "\\*\\*\\*\\* "))
      (let ((uuid (or (car uuid-hw) (org-entry-get (point) "CUSTOM_ID")))
	    (hw (or (cadr uuid-hw) (concat (if (= 4 (org-outline-level))
			    (save-excursion
			      (outline-up-heading 1)
			      (car (split-string (org-get-heading))) ))
			" "
			(org-get-heading))))
	    (result-buffer (get-buffer-create "*TLS Words*"))
	    (the-buf (current-buffer)))
	(set-buffer result-buffer)
	(setq buffer-file-name nil)
	(read-only-mode 0)
	(erase-buffer)
	(insert "* " (if hw hw "") "\n")
	(dolist (el (gethash uuid mandoku-tls-pos-info))
	  (mandoku-tls-print-elements el))
	(dolist (el (gethash uuid mandoku-tls-swl))
	  (mandoku-tls-print-elements (plist-get el :line)))
	(insert "\n")
	(mandoku-dict-mode)
	(hide-sublevels 2)
	(goto-char (point-min))
	(switch-to-buffer-other-window result-buffer t))
    (user-error "Please move to a syntactic word to look up the attributions")))

(defun mandoku-tls-procline-short (&optional short inp)
  (interactive)
  (let ((current-prefix-arg 4))
    (call-interactively #'mandoku-tls-procline)))

(defun mandoku-tls-procline (&optional inp)
  (interactive
   (if (use-region-p)
       (list (buffer-substring-no-properties (region-beginning) (region-end))))
   )
  (unless mandoku-tls-initialized-p
    (mandoku-tls-initialize))
  (when (derived-mode-p 'mandoku-view-mode)
    (setq mandoku-position (mandoku-position-at-point-internal))
    (set-marker mandoku-position-marker (point))
    )
  (let ((inp (mandoku-remove-punct-and-markup (or inp (mandoku-get-line))))
	(short (or current-prefix-arg nil))
	(result-buffer (get-buffer-create "*Dict Result*"))
	(the-buf (current-buffer))
	(pos    (mandoku-position-at-point))
	i j tmp res pron tp)
    (set-buffer result-buffer)
    (setq mandoku-tls-inp inp)
    (setq buffer-file-name nil)
    (read-only-mode 0)
    (erase-buffer)
    (insert "* " (if pos pos "") " " inp "\n")
    (setq i 0)
    (while (< i (length inp))
      (setq j (+ i 1))
      (setq tmp (substring inp i j))
      (setq res (gethash tmp mandoku-tls-words))
      (while (and (< j (length inp)) res)
	(setq j (+ j 1))
	(setq tmp (substring inp i j))
	(setq res (gethash tmp mandoku-tls-words))
	)
      (unless res
	(setq tmp (substring inp i (-  j 1)))
	(setq res (gethash tmp mandoku-tls-words)))
      (if  (< 0 (length tmp))
	  (progn
	    (mandoku-tls-print-tls-word tmp short)
	    (insert "\n")
      ))
      (setq i (+ i 1)))
    (mandoku-dict-mode)
    (define-key mandoku-dict-mode-map "<F8> n" 'mandoku-tls-new-swl)
    (hide-sublevels 2)
    (goto-char (point-min))
    (switch-to-buffer-other-window result-buffer t)))

(defun mandoku-tls-readings-format (txx)
  "Format the readings plist.
Argument TXX The plist from which the readings are extracted."
  (concat
   (plist-get txx :pinyin) " (OC: "
   (plist-get txx :oc) " MC: "
   (plist-get txx :mc) ")  "
   (plist-get txx :fqshang)
   (plist-get txx :fqxia)
   "切 "
   (plist-get txx :diao)
   " 廣韻：【"
   (plist-get txx :gloss)
   " 】"))

(defun mandoku-tls-get-readingsline (word)
  "Loop through WORD and return a list of lines that contain pinyin/oc/mc/fq/gloss."
  (let ((wl (mapcar 'char-to-string (string-to-list word))) l tx)
    (dolist (w wl)
      (setq tx (gethash w mandoku-tls-syllables-tab))
      (dolist (txx tx)
        (push (concat
               w " "
               (mandoku-tls-readings-format txx)
               " 】") l)))
    (nreverse l)))
    
(defun mandoku-tls-print-tls-word (char &optional short)
  (let* (
         (wlist (gethash char mandoku-tls-words))
         (concepts (delete-dups (mapcar (lambda (x)
                                         (cadr (car (mandoku-tls-re-seq (cadr x)))))
				       wlist)))
         (gyh (make-hash-table :test 'equal))
         gconcepts phon
        )
    (dolist (c concepts)
      (push c (gethash (cdr (assoc char (plist-get (gethash c mandoku-tls-concepts) :words))) gyh )))
    (maphash (lambda (k v)
               (push k gconcepts)) gyh)
    (dolist (g (mapcar (lambda (x)
                         (plist-get x :uuid))
                       (gethash char mandoku-tls-syllables-tab)))
      (insert "\n** " char " ")
      (when (setq phon (gethash g mandoku-tls-syllables-uuid))
        (insert (mandoku-tls-readings-format phon)))
     (dolist (c (sort (gethash g gyh) 'string<))
      (insert "\n*** " c
	      (if (not short)
		  (string-join (mapcar (lambda (x)
				     (when (string-match c (cadr x))
				       (concat "\n**** " (string-join x " ") "\n" ))
				     )
				       wlist) "") "")
	      
	      ))
     (insert "\n")
     )))

(defun mandoku-tls-make-attribution (&optional att)
  "This makes an attribution for the current line.
Optional argument ATT The attribution to use when called from a lisp function."
  (interactive)
  (let* ((inp (or mandoku-tls-inp
		  (mandoku-remove-punct-and-markup
		   (mandoku-get-line))))
	 mandoku-tls-concept
	 (start (- (string-to-number (read-from-minibuffer (concat inp ": Please enter position of first character to use: ") "1" nil )) 1))
	 (slength (string-to-number (read-from-minibuffer (concat inp ": Please enter number of characters to use: ") "1" nil )))
	 (word (substring inp start (+ start slength)))
	 ;;
	 concept readings)
    (helm :sources '(mandoku-tls-helm-source
		     (helm-build-dummy-source "New concept:"
			 :action '(("Create new concept:" . mandoku-tls-create-new-annot)))))
    (find-file
     (concat mandoku-tls-lexicon-path "concepts/" mandoku-tls-concept ".org") t)
    (hide-sublevels 3)
    (unless (search-forward word nil t)
      (goto-char (point-max)))
    ;; more characters!
    (dolist (w (string-to-list word))
      (helm-charinfo (format "%c" w))
      (push (cadr(split-string helm-charinfo-selected)) readings))
    (insert "\n*** "
	    word
	    " "
	    (mapconcat 'identity (reverse readings) " ")
	    "\n")
    (mandoku-tls-new-uuid-for-heading)
    (mandoku-tls-new-syntactic-word)))

(defun mandoku-tls-new-swl ()
  "Create a new syntactic word location.  If necessary, also create concept, syntactic word or syntactic function."
  (interactive)
  (if (and (derived-mode-p 'mandoku-dict-mode)
	     (looking-at "\\*\\*\\*\\* "))
  (let ((hw (org-get-heading))
	char
	swl-line)
    (if (string-match "tls:concept:[^#-]+#uuid" hw)
	(progn
	  (setq char (save-excursion
		  (outline-up-heading 2)
		  (org-get-heading)))
	  (setq swl-line
		(concat char " "
			(replace-regexp-in-string " \\([NV]\\) " "\t\\1 "
						  (replace-regexp-in-string "\\[\\[tls:concept" "\t[[tls:concept" hw))				 ))
	  (mandoku-tls-insert-new-annot swl-line)
      
	  )
      ))
  (message "Annotation not possible at this point.")
  ))

(defun mandoku-tls-new-concept-maybe ()
  (interactive)
  (let ((s (thing-at-point 'word)))
	(setq s (read-string "TLS | Enter new concept to create: " s))
	(mandoku-tls-new-concept s)))

(defun mandoku-tls-new-concept (con)
  (interactive "sTLS | Enter new concept to create: ")
  (let* ((concept (upcase con))
	 (fp (concat mandoku-tls-lexicon-path "concepts/" (upcase concept) ".org"))
	 (word (if (derived-mode-p 'org-mode)
		   (save-excursion
		     (search-backward "\n** ")
		     (goto-char (match-end 0))
		     (org-get-heading))
		 nil))
	 pron )
    (when word
      (if (string-match "OC" word)
	  (let ((pp (gethash (substring word 0 1)  mandoku-tls-syllables-tab)))
	    (dolist (p pp)
	      (when (string-match (plist-get p :gloss) word)
		(push p pron)))
	    (setq word (substring word 0 1)))
	(dolist (c (string-to-list word))
	  (push (mandoku-tls-select-readings (char-to-string c)) pron))))
    (mandoku-tls-create-new-annot concept word pron)))

(defun mandoku-tls-create-new-annot (concept &optional word pron)
  "If concept does not exist, create one. If we have a word and pron, add them."
  (interactive)
  (let* ((fp (concat mandoku-tls-lexicon-path "concepts/" (upcase concept) ".org"))
	 (new (not (file-exists-p fp))))
    (with-current-buffer (find-file-other-window fp)
      (when new
	(insert-file-contents mandoku-tls-concept-template)
	(goto-char (point-min))
	(search-forward "=CONCEPT= ")
	(goto-char (match-end 0))
	(insert (upcase concept))
	(search-forward ":CUSTOM_ID: ")
	(goto-char (match-end 0))
	(insert (mandoku-tls-id-new)))
      (when word
	(goto-char (point-min))
	(search-forward "** WORDS")
	(org-end-of-subtree)
	(org-show-subtree)
	(forward-line -1)
	(when (looking-at "bibli")
	    (forward-line -1))
	(insert "\n*** " word)
	(when pron
	  (insert " ")
	  (dolist (p pron)
	    (insert (format "%s" (plist-get p :pinyin))))
	  (insert " (OC:")
	  (dolist (p pron)
	    (insert (format "%s " (plist-get p :oc))))
	  (insert "MC: ")
	  (dolist (p pron)
	    (insert (format "%s " (plist-get p :mc))))
	  (insert ")"))
	(insert "\n")
	(mandoku-tls-new-uuid-for-heading)
	;;TODO: add the machine-readable part of the word definition
	(org-set-property "CHAR+" (mapconcat
				   (lambda (x)
				     (concat (char-to-string x) (gethash (char-to-string x) mandoku-tls-character-tab)))
				    (string-to-list word) " "))
	(org-set-property "GY_IDS+" (mapconcat (lambda (x) (plist-get x :uuid)) pron " "))
	(org-set-property "PY+" (mapconcat (lambda (x) (plist-get x :pinyin)) pron " "))
	(org-set-property "OC+" (mapconcat (lambda (x) (plist-get x :oc)) pron " "))
	(org-set-property "MC+" (mapconcat (lambda (x) (plist-get x :mc)) pron " "))
	)
      (when new
	(goto-char (point-min))
	(search-forward "** DEFINITION")
	(forward-line 1)
	(save-buffer)
	(mandoku-tls-read-concept-file fp)
	(message "Please enter the definition"))
      concept)))

(defun mandoku-tls-can-add-word-p ()
  "See if we can add a syntactic word here."
  (save-excursion
    (let ((p (point)))
      (goto-char (point-min))
      (search-forward "** WORDS" nil t)
      (> p (point)))))

(defun mandoku-tls-new-syntactic-word (&optional syn-func def)
  "For an existing Lexeme, we add a syntactic word (usage instance)."
  (interactive)
  (when (derived-mode-p 'mandoku-tls-view-mode)
    (if (mandoku-tls-can-add-word-p)
	(if syn-func
	    (let ((name (plist-get (gethash (car syn-func) mandoku-tls-syn-func-tab) :name))
		  (def (or def (read-string "TLS | Definition of new syntactic word: "))))
	      (org-back-to-heading)
	      (outline-up-heading (- (car (org-heading-components)) 4))
	      (if (org-entry-get (point) "CHAR" )
		  (progn
		    (org-show-subtree)
		    (org-end-of-subtree)
		    (insert "\n**** " (upcase (substring name 0 1)) " [[tls:syn-func::#" (car syn-func)  "][" name  "]] / " def)
		    (mandoku-tls-new-uuid-for-heading)
		    (search-forward ":END:")
		    (insert "\n****** DEFINITION\n" def "\n")
		    (insert "\n****** NOTES\n"))))
	  (helm :sources '((name . "TLS Syntactic functions")
			   (candidates . mandoku-tls-helm-syn-func-candidates)
			   (action . (("Select" . mandoku-tls-new-syntactic-word))))))
      (user-error "Please move to the WORD to which you want to add a syntactic word"))))
  
(defun mandoku-tls-new-uuid-for-heading ()
  (interactive)
  (unless (org-entry-get (point) "CUSTOM_ID" nil)
  (org-set-property "CUSTOM_ID" (mandoku-tls-id-new))))

(defun mandoku-tls-compose-new-annot()
  (let ((concept
	 (replace-in-string
	  (save-excursion
	    (goto-char (point-min))
	    (search-forward "=CONCEPT= ")
	    (org-get-heading))
	  "=CONCEPT= " ""))
	(char (save-excursion
		  (outline-up-heading 1)
		  (org-get-heading)))
	)
    (mandoku-string-remove-all-properties (format "%s\t[[tls:concept:%s::#%s][%s]]\t%s" char concept
	    (org-entry-get (point) "CUSTOM_ID" nil)
	    concept (org-get-heading)))
  ))

(defun mandoku-tls-insert-new-annot (&optional ann)
  "Insert the annotation."
  (interactive)
  (let ((ann (or ann (mandoku-tls-compose-new-annot)))
	(case-fold-search t))
    (with-current-buffer (marker-buffer mandoku-position-marker)
      (goto-char (marker-position mandoku-position-marker))
      (mandoku-tls-insert-new-annot-1 ann)
    (save-buffer))))

(defun mandoku-tls-insert-new-annot-1 (ann)
  "Really insert the annotation."
  (forward-line)
  (beginning-of-line)
  (if (looking-at ":zhu:")
      (progn
        (re-search-forward ":end:")
        (beginning-of-line)
        (insert ann
                "\n" )
        (previous-line))
    (progn
      (insert ":zhu:\n \n:end:\n")
      (previous-line 2)
      (beginning-of-line)
      (insert ann)))
  (beginning-of-line)
  (deactivate-mark))

(defun mandoku-tls-print-elements (list)
  "Print each element of LIST on a line of its own."
  (insert "
*** ")
  (if (listp list)
      (insert (mapconcat 'identity list " "))
    (insert list))
  (insert "\n"))
;  (setq list (cdr list)))


;; add the swl
(defun mandoku-tls-swl-tab-change (state)
  (interactive)
  (cond
   ((and (eq major-mode 'mandoku-dict-mode)
	     (memq state '(children subtree)))
    (save-excursion
      (ignore-errors
      (let ((uuid (car (split-string (cadr (split-string  (org-get-heading) "#")) "\\]\\["))))
	(forward-line)
	(if (looking-at "\n")
	    (dolist (i (gethash uuid mandoku-tls-swl))
	      (insert (concat (plist-get i :line) "\n")))
	    (message uuid))))))))

(add-hook 'org-cycle-hook 'mandoku-tls-swl-tab-change)

;; add link type tls
(org-add-link-type "tls" 'mandoku-tls-follow-link)
;(add-hook 'org-store-link-functions 'org-dic-store-link)


(defun mandoku-tls-follow-link (link)
  "Follow the tls link."
; links for concept, syn-func, sem-feat
  (let* ((type (car (split-string link ":")))
	 (rest (cdr (split-string link ":")))
	 )
    (cond
     ((equal type "concept")
      (message type)
      (org-open-file (concat mandoku-tls-lexicon-path "concepts/" (car rest) ".org") t nil (nth 2 rest))
      )
     ((or (equal type "syn-func") (equal type "sem-feat") (equal type "rhet-dev" ))
      (message type)
      (org-open-file (concat mandoku-tls-lexicon-path "core/" type ".org") t nil (cadr  rest))
      )
     ((equal type "text")
      (message type)
      (org-open-file
       (concat mandoku-tls-text-path "/" (car rest) "/" (car rest) "_"
	       (car (split-string (cadr rest) "-")) ".txt") t nil (cadr  rest))))))

;;* font lock for org-ref

(defcustom mandoku-tls-colorize-links
  t
  "When non-nil, change colors of links."
  :type 'boolean
  :group 'mandoku-tls)


(defcustom mandoku-tls-cite-color
  "forest green"
  "Color of cite like links."
  :type 'string
  :group 'mandoku-tls)


(defcustom mandoku-tls-ref-color
  "dark red"
  "Color of ref like links."
  :type 'string
  :group 'mandoku-tls)


(defcustom mandoku-tls-label-color
  "dark magenta"
  "Color of label links."
  :type 'string
  :group 'mandoku-tls)

(defvar mandoku-tls-grep-split-line-regexp "^\\([[:lower:][:upper:]]?:?.*?\\):\\([0-9]+\\):\\(.*\\)")

(defvar mandoku-tls-re
  (concat "\\(" (mapconcat
                 (lambda (x)
		   (replace-regexp-in-string "\*" "\\\\*" x))
                 mandoku-tls-types "\\|") ":\\)"
                 "\\([a-zA-Z0-9-_:\\./]+,?\\)+")
  "Regexp for cite links.")


(defvar mandoku-tls-label-re
  "label:\\([a-zA-Z0-9-_:]+,?\\)+"
  "Regexp for label links.")


(defvar mandoku-tls-ref-re
  "\\(eq\\)?ref:\\([a-zA-Z0-9-_:]+,?\\)+"
  "Regexp for ref links.")


(defface mandoku-tls-cite-face
  `((t (:inherit org-link :foreground ,mandoku-tls-cite-color)))
  "Color for cite-like links in org-ref.")


(defface mandoku-tls-label-face
  `((t (:inherit org-link :foreground ,mandoku-tls-label-color)))
  "Color for ref links in org-ref.")


(defface mandoku-tls-ref-face
  `((t (:inherit org-link :foreground ,mandoku-tls-ref-color)))
  "Face for ref links in org-ref.")

;; Add a new translation to current paragraph, or edit it if existing

(defun mandoku-tls-skip-line (&optional n)
  "Skip to the next buffer line (not text line!) ahead or back, ignoring drawers."
  (let* ((skip (or n 1))
	 (step (if (< 0 skip) 1 -1)))
    (forward-line step)
    (cond
     ((looking-at ":end")
      (if (= step 1)
	  (forward-line step)
	(progn (search-backward ":zhu")
	       (forward-line step))))
     ((looking-at ":zhu")
      (if (= step 1)
	  (progn (search-forward ":end:")
		 (forward-line step))
	(forward-line step))))))

(defun mandoku-tls-find-para ()
  "Get the bounds of the current paragraph, in the Mandoku sense.
A paragraph is bounded by empty lines, headlines, but not
interrupted by drawers, these are considered part of the
paragraph."
  (save-excursion
    (let* ((max 20)
	   (p (point))
	   (beg (let ((cnt 0))
		  (while (and (< cnt max)
			      (not (looking-at "\s*\n")))
		    (setq cnt (+ 1 cnt))
		    (mandoku-tls-skip-line -1))
		  (point)))
	   
	   (end (let ((cnt 0))
		  (goto-char p)
		  (while (and (< cnt max)
			      (not (looking-at "\s*\n")))
		    (setq cnt (+ 1 cnt))
		    (mandoku-tls-skip-line 1))
		  (point))))
      (list beg end))
    ))

(defun mandoku-tls-add-trans ()
  "Add a new translation to current paragraph, or edit it if there is one already."
  (let ((para (mandoku-tls-find-para))
	(trans-buffer (get-buffer-create "*Mandoku TLS Edit Translation*"))
	(header (car header-line-format))
	(org-ellipsis "")
	(maxc 0)
	zhu ov trans line ml)
    ;; maybe I should do this as part of mandoku-view-mode
    (add-to-invisibility-spec '(mandoku-tls-hide-drawer . t))
;    (narrow-to-region (car para) (cadr para))
    (goto-char (point-min))
    ;; hide zhu
    (while (search-forward ":zhu" nil t)
      (goto-char (match-beginning 0))
      (setq zhu (org-element--current-element (point-max)))
      (overlay-put (make-overlay
		    (plist-get (car (cdr zhu)) :begin)
		    (plist-get (car (cdr zhu)) :end))
		   'invisible 'mandoku-tls-hide-drawer)
      (goto-char (plist-get (car (cdr zhu)) :end)))
    (goto-char (point-min))
    (while (not (eobp))
      (setq line (or (cadr (split-string (thing-at-point 'line) "\t"))
		     "\n"))
      (setq ml (point-marker))
      (push (cons ml line) trans )
      (when  (search-forward "\t" (line-end-position) t)
	(goto-char (- (point) 1))
	(kill-line)
	(when (> (current-column) maxc)
	  (setq maxc (current-column)))
	(goto-char (line-beginning-position)))
      (next-line 1))
    (set-window-buffer (split-window-horizontally (+ maxc 5))  trans-buffer)
    (with-current-buffer trans-buffer
      ;; maybe define a minor mode for this? Set the cancel and edit keys to the appropriate commands.
      
      (setq header-line-format
	    (concat header
		    (substitute-command-keys
		     " Edit, then exit with `\\[org-edit-src-exit]' or abort with \
`\\[org-edit-src-abort]'")))
      (erase-buffer)
      (dolist (l (nreverse trans))
	(insert (cdr l))))
    ))

;;

(defun mandoku-tls-id-new ()
  "Get the id in a compatible way."
  (replace-in-string (downcase (org-id-new "uuid")) ":" "-"))

;; maintenance and exploring

(defun mandoku-tls-make-concepts-report ()
  "Read the mandoku-tls-concept table and create a hierarchical report of the concepts mentioned."
  (unless mandoku-tls-initialized-p
    (mandoku-tls-initialize))
  (let ((w "*") l n)
    (maphash (lambda (k v)
	       (if (plist-get v :up-tax)
		   (setq l (cons (list k (plist-get v :up-tax) ) l ))
		 (push k n))
	       ) mandoku-tls-concepts)
    ;; first find all elements with no uplink
    (with-current-buffer (find-file-noselect (concat mandoku-tls-lexicon-path "/core/tls-concepts.org") t)
      (erase-buffer)
      (dolist (e (reverse n))
;	(message (car e))
	(mandoku-tls-subtree-insert e w)
	;(kill-buffer)
	)
    (save-buffer)
    )))

;;; tls-view-mode

(defvar mandoku-tls-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f8>") 'hydra-tls-view/body)
;    (define-key map "e" 'view-mode)
;    (define-key map "a" 'redict-get-line)
         map)
  "Keymap for mandoku-tls-view mode."
)

;(define-key mandoku-tls-view-mode-map (kbd "<f8>") 'hydra-tls-view/body)

(define-derived-mode mandoku-tls-view-mode org-mode "mandoku-tls-view"
  "a mode to view TLS concept files
  \\{mandoku-tls-view-mode-map}"
  (setq case-fold-search nil)
  (local-unset-key [menu-bar Org])
  (local-unset-key [menu-bar Tbl])
  (easy-menu-add mandoku-tls-menu mandoku-tls-view-mode-map)
;  (add-hook 'after-save-hook 'mandoku-add-to-for-commit-list nil t)
  (set (make-local-variable 'org-startup-folded) 'nofold)
  (add-hook 'after-save-hook 'mandoku-tls-read-concept-file nil t)
)

(easy-menu-define mandoku-tls-menu mandoku-tls-view-mode-map "TLS menu"
  '("TLS"
    ("Concepts"
     ["Show concept tree" mandoku-toggle-visibility t])
    ))


;;;

(defun mandoku-tls-startpage ()
  "Display overview information about the TLS Database."
  (unless mandoku-tls-initialized-p
    (mandoku-tls-initialize))
  (with-current-buffer (get-buffer-create "*TLS Overview*")
    (erase-buffer)
    (insert "#+TITLE: TLS\n")
    (insert "\n* TLS Database overview\n")
    (insert (format "\n - Number of concepts: %d\n" (hash-table-count mandoku-tls-concepts)))
    (insert (format "\n - Number of syntactic words: %d\n" (hash-table-count mandoku-tls-words)))
    (insert (format "\n - Number of annotations: %d\n" (hash-table-count mandoku-tls-swl)))
    (insert "\n* Database access\n")
    (insert "    For access to the database and a menu of options,
    please press the F8 key or [[elisp:hydra-tls/body()][click here]].\n
    If you use the link, press 'C' (for concepts) or 'S' for syntactic
    functions.")
    (if (file-exists-p mandoku-tls-taxonomy-file)
	(insert (format "\n    [[file:%s][Click here to open the taxonomy file]]" mandoku-tls-taxonomy-file)
		))
    (mandoku-tls-view-mode)
    (setq org-confirm-elisp-link-function nil)
    )
  )

  
(defun mandoku-tls-subtree-insert (e w)
  (let ((w (concat w "*")) x)
    (when (file-exists-p (concat mandoku-tls-lexicon-path "concepts/" e ".org"))
      (insert (format "\n%s [[tls:concept:%s][%s]]" w e e "\n" ))
;      (insert (format "\n%s %s %d" w e (length (plist-get (gethash e mandoku-tls-concepts) :words)) "\n" ))
;      (insert (format "\n%s %s %s" w e (mapconcat 'identity (plist-get (gethash e mandoku-tls-concepts) :words) " ") "\n" ))
      (setq x (car (plist-get (gethash e mandoku-tls-concepts) :pointers)))
      (when (equal (car x) "TAXONOMY")
	(dolist (f (cdr x))
	  (mandoku-tls-subtree-insert f w))))))

;;

(defun mandoku-tls-duplicate-up ()
  (let ((tmp))
    (maphash (lambda (k v)
	       (when (> (length (plist-get v :up)) 2)
		 (setq tmp (plist-get v :up))
		   (message (format "%s: %s" k
				    (mapconcat 'identity (delete-dups tmp ) ",")))
		 )) mandoku-tls-concepts)))

;; (defun mandoku-tls-hash-to-list (hashtable)
;;   "Return a list that represent the HASHTABLE."
;;   (let (myList)
;;     (maphash (lambda (kk vv) (setq myList (cons (list (plist-get vv :name) kk ) myList))) hashtable)
;;     myList
;;   )
;; )


(defun mandoku-tls-number-of-subentries (&optional pos match scope level)
  "Return number of subentries for entry at POS.
MATCH and SCOPE are the same as for `org-map-entries', but
SCOPE defaults to 'tree.
By default, all subentries are counted; restrict with LEVEL."
  (interactive)
  (save-excursion
    (goto-char (or pos (point)))
    ;; If we are in the middle ot an entry, use the current heading.
    (org-back-to-heading t)
    (let ((maxlevel (when (and level (org-current-level))
                      (+ level (org-current-level)))))
;      (message "%s subentries"
               (1- (length
                    (delq nil
                          (org-map-entries
                           (lambda ()
                             ;; Return true, unless below maxlevel.
                             (or (not maxlevel)
                                 (<= (org-current-level) maxlevel)))
                           match (or scope 'tree))))))))

(defun mandoku-tls-zhu-find-src ()
  (let (src date (pt (point)))
    (save-excursion
      (search-backward "# src" nil t)
      (setq src (or (cadr (split-string (thing-at-point 'line t) ": "))
                    "--\n"))
      (goto-char pt)
      (search-backward "# dating" nil t)
      (setq date (if (looking-at "# dating")
                     (cadr (split-string  (thing-at-point 'line t) ": "))
                   "--\n")))
    (list src date)))

(defun mandoku-tls-zhu-write-tls-zhu ()
  "Write all zhu from the text in local files."
  (let ((textlist (sort (mandoku-list-local-texts) 'string<))
        files)
    (dolist (text textlist)
      (message (concat "Processing " text " .."))
      (setq files (directory-files (concat mandoku-text-dir (substring text 0 4) "/" text) t "\\.txt"))
      (dolist (file files)
        (mandoku-tls-zhu-write-to-file file)))))

(defun mandoku-tls-zhu-write-to-file (&optional file)
  "Write to file, optional n indicates a 'notes first' format."
  (interactive)
  (let* (beg end tl pos notes src-date page line ln tnote char
	     (default-directory (file-name-directory file))
	    (commit (magit-rev-verify "HEAD"))
	    (fn (file-name-sans-extension (file-name-nondirectory (or file (buffer-file-name)))))
            (title (replace-regexp-in-string "(正文)" "" (car (split-string (gethash (car (split-string fn "_")) mandoku-titles) "-"))))
	    outb )
    (with-temp-buffer
      (insert-file-contents file)
      ;; only bother if there are annotations
      (when (re-search-forward mandoku-annot-start nil t)
	(goto-char (point-min))
	(setq outb (find-file-noselect (concat mandoku-tls-zhu-path fn ".zhu" )))
	(with-current-buffer outb
        (erase-buffer)
        (goto-char (point-min))
        (insert "# -*- mode: mandoku-view; -*-
#+TITLE: Notes for " title  " " fn "
#+DATE: " (format-time-string "%Y-%m-%dT%T%z\n" (current-time))
"#+PROPERTY: COMMIT " commit "\n"
))
      (while (re-search-forward mandoku-annot-start nil t)
        (setq beg (point))
        (setq tl (mandoku-annot-targetline))
        (setq pos (cadr tl))
        (setq page (cadr (split-string (nth 2 pos) "-")))
        (setq line (nth 3 pos))
	(setq char  (int-to-string (nth 4 pos)))
        (search-forward mandoku-annot-end)
        (setq end (point))
        (setq src-date (mandoku-tls-zhu-find-src))
        (setq notes (mandoku-annot-collect beg end))
        (with-current-buffer outb
          (setq ln (replace-regexp-in-string "\\([　-㏿！-￮]\\)" ""
                                             (replace-regexp-in-string "\\(?:<[^>]*>\\)?¶?" "" (car tl))))
          (insert "** "
                  ln
                  ":PROPERTIES:"
                  "\n:SRC: " (car src-date)
                  ":DATE: " (cadr src-date)
                  ":LOCATION: "
                  (format "%s:%s%2.2d:%s::%s / %s\n"
			  fn page line char (substring (car tl) 0 2) title)
                  ":CHARPOS: " char
                  "\n:END:\n"
                  )
          (dolist (note notes)
	    (setq tnote (string-join (nbutlast (split-string note " /" )) " / "))
            (insert "*** " (car (split-string note "\t")) "\n"
                    ":PROPERTIES:\n"
                    (mapconcat
                     (lambda (x)
                       (let (a)
			 ;; don't bother if there is no concept in the note
			 (when (string-match "concept:" tnote)
			   (setq a (split-string (car x) "::#"))
			   (if (equal "concept" (nth 1 (split-string (car a) ":")))
			       (format ":%s: %s / %s" (upcase (nth 1 (split-string (car a) ":"))) (cadr a) (cadr x))
			     (format ":%s: %s" (upcase (nth 1 (split-string (car a) ":"))) (cadr a))))))
		       (mandoku-tls-re-seq  tnote )
		       "\n")
                    "\n:CHARPOS: " (int-to-string (+ (nth 4 pos) (or (string-match (car (split-string tnote "@" ))  ln) 0)))
                    "\n:END:\n")
            (insert note "\n")))
        )
    (with-current-buffer outb
      (save-buffer)
      (kill-buffer)
    )))))

(defun mandoku-tls-re-seq (string)
  "Get a list of all regexp matches in a string, n is the matchstring, if relevant."
  (save-match-data
    (let ((pos 0)
          m1 m3
          matches)
      (while (string-match org-bracket-link-regexp string pos)
        (setq m1 (match-string 1 string))
        (setq m3 (match-string 3 string))
        (setq pos (match-end 0))
        (when (string-match "tls:" m1)
          (push (list
                 m1
                 m3)
                matches)))
     (reverse matches))))

;; helm for syntactic word selection
;; build the data, select candidate, access the data

(defun mandoku-tls-add-attribution-for-char (begin end)
  (interactive "r")
  (if (not (region-active-p))
      (message "Please select a character to annotate.")
    (let* ((ch (buffer-substring-no-properties begin end))
	   (candx (mandoku-tls-sw-candidates ch)))
    (helm :sources (list (helm-build-sync-source (format "Syntactic words for %s" ch)
                     :action '(("Add new attribution" . mandoku-tls-insert-new-annot-1)
			       ("Go to definition" . (lambda (candidate)
						       (mandoku-tls-follow-link
							(if (string-match "tls:\\(concept:[^]]+\\)" candidate)
							    (match-string 1 candidate)))))
			       ("Show attributed words" . (lambda (candidate)
							    (mandoku-tls-show-words
							     (when (string-match "\\([^@]+\\)[^#]+#\\([^]]+\\)" candidate)
							       (list (match-string 2 candidate)
								     (match-string 1 candidate))))))
                               ("Copy to clipboard" . (lambda (candidate)
                                                        (kill-new candidate))))
		     :candidates  candx)
                         (helm-build-dummy-source
                              "New syntactic word"
                             :action '(("Create new syntactic word:" . mandoku-tls-new-syntactic-word))))
	  :buffer "*TLS Syntactic words2*")
    )))

;  (mandoku-tls-add-attribution-helm (buffer-substring-no-properties begin end))))

(defun mandoku-tls-sw-candidates (ch)
  "Offer SW for selected word, add attribution."
  (let ((clist (gethash ch mandoku-tls-words))
         l)
    (dolist (c clist)
      (let* ((concept (cadr (car (mandoku-tls-re-seq (nth 1 c)))))
            (conc-uuid (cadr (split-string (car (car (mandoku-tls-re-seq (nth 1 c)))) ":#")))
            (syn-fun (cadr (car (mandoku-tls-re-seq (nth 2 c)))))
            (sem-feat (or (cadr (cadr (mandoku-tls-re-seq (nth 2 c)))) ""))
            (def (cadr (split-string (nth 2 c) " / ")))
            (len (length (gethash conc-uuid mandoku-tls-swl))))
        (push (cons (format "%s %3.3d %-20.20s %-30.30s  %-10.10s %-10.10s %-.30s"  ch len  (car c)  concept syn-fun sem-feat def )
                    (format "%s@%s %s" ch (car c) (mapconcat 'identity (cdr c) " ")))
              l)))
    (nreverse (sort l 'mandoku-tls-sort-by-car))))

;; (defun mandoku-tls-sw-helm ()
;;   (interactive)
;;     (helm :sources (helm-build-sync-source (format "Syntactic words for %s" ch)
;;                      :action '(("New attribution" . mandoku-tls-insert-new-annot-1)
;;                                ("Copy to clipboard" . (lambda (candidate)
;;                                                         (kill-new candidate))))
;; 		     :candidates
;; 	  :buffer "*TLS Syntactic words*")
;; ))

;; directly access the char
(defun mandoku-tls-syll-info (&optional ch)
  "We override the function defined in helm-charinfo."
  (unless mandoku-tls-syllables-tab
    (mandoku-tls-read-syllables-org))
  (let ((char (or ch mandoku-tls-current-char ))
	l x d fq)
    (dolist (v (gethash char mandoku-tls-syllables-tab))
      (setq d (or (plist-get v :gloss) ""))
      (setq fq (or (concat "" (plist-get v :fqshang) "" (plist-get v :fqxia)  "切") "" ))
      (push (cons (format "%s  %-8s %-8s %-8s %s　%s %s"
		    char
		    (plist-get v :pinyin)
		    (plist-get v :oc)
		    (plist-get v :mc)
		    fq
		    (plist-get v :diao)
		    d
		    )
		  (list v)
		  ;; (list (plist-get v :pinyin)
		  ;; 	(plist-get v :oc)
		  ;; 	(plist-get v :mc)
		  ;; 	(plist-get v :uuid)
		  ;; 	)
		  )
		  l))
    (nreverse l)
    ))

(defun mandoku-tls-result (candidate)
  (let* ((current-char (car (split-string candidate)))
	 (data (mandoku-tls-syll-info current-char)))
  (kill-new (plist-get (car (cdr (assoc (helm-get-selection) data))) :uuid ))))


(defun mandoku-tls-select-readings (&optional pat)
  "Get the readings for the character."
  (interactive)
  (let* ((mandoku-tls-current-char (or pat (char-to-string (char-after))))
	 (source `((name . "Select lexeme")
		   (candidates . , (mapcar 'car (mandoku-tls-syll-info)))
		   (action . (("Copy pinyin" . (lambda (can)
						 (kill-new (cadr (split-string can)))))
			      ("Copy readings" . (lambda (can)
						   (let ((cc (split-string can)))
						     (kill-new (format "%s %s (OC:%s MC:%s)" (car cc) (cadr cc)
							     (nth 2 cc) (nth 3 cc))))))
			      ("Readings uuid" .  mandoku-tls-result))
			   ))))
  (helm :sources source
	:buffer "*helm dictionary*"
	:input pat)))

(defun mandoku-tls-fix-taxonomy ()
  "Should be obsolete now."
  (let (x beg end
	(case-fold-search nil))
    (goto-char (point-min))
    (while (re-search-forward "@@@" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "\n\\(.*\\)\t" nil t)
      (beginning-of-line)
      (setq x (match-string 1))
      (replace-match "\n")
      (cond
       ((string-match "[IVXC]+\\."  x)      (insert "*** " ))
       ((string-match "[A-Z]+\\."  x)       (insert "**** " ))
       ((string-match "[0123456789]+\\."  x)(insert "***** " ))
       ((string-match "[a-z]+)"  x)         (insert "****** " ))
       ((string-match "[a-z]+\\."  x)       (insert "****** " ))
       ((string-match "([0123456789]+)"  x) (insert "******* " ))
       (t (insert "-x-x-" x "\t"))))
    (goto-char (point-min))
    (while (re-search-forward "^\\(.\\)$" nil t)
      (replace-match "* \\1"))
    (goto-char (point-min))
    (while (re-search-forward "\\([A-Z ]+\\)" nil t)
      (unless (gethash (match-string 1) mandoku-tls-concepts)
	(push (match-string 1) missing)
  ))
))

(defun mandoku-tls-tax-add-concepts ()
  "For empty 1 level entries: add concepts."
  (let (st hw)
;    (goto-char (point-min))
    (while (search-forward "\n* ")
      (org-copy-subtree)
      (setq st   (mandoku-trim-and-star (replace-regexp-in-string "[0-9]+" ""   (current-kill 0))))
      (when (< (length st) 5)
	(setq hw (substring-no-properties (org-get-heading t t)))
        (forward-line 1)
	(dolist (tmp (mapcar 'char-to-string (string-to-list hw)))
	  (mandoku-tls-print-tls-word tmp t)
	  )
  ))))

(defun mandoku-tls-find-missing-concepts (&optional bound)
  (let (missing ms hc concepts char
	(case-fold-search nil))
    (goto-char (point-min))
    (while (re-search-forward "^\\* \\(.\\)" nil t)
      (setq hc (cadr (org-element-at-point)))
      (setq char (plist-get hc :raw-value))
      (setq concepts (delete-dups (mapcar (lambda (x)
					    (cadr (car (mandoku-tls-re-seq (cadr x)))))
					  (gethash char mandoku-tls-words))))
      (while (re-search-forward "\\([A-Z ]+\\)" (plist-get hc :end) t)
	(setq ms (mandoku-trim-and-star (match-string 1)))
	(when (< 0 (length ms))
	  (unless (member ms concepts)
	    (push (cons ms (plist-get hc :raw-value)) missing)))))
      missing))

(defun mandoku-tls-grep (&optional beg end)
  (interactive)
  (if (not (use-region-p))
      (mandoku-tls-grep-candidates-1
       (read-string "TLS text search -- please enter a search term: "))
    (let ((beg (or beg (region-beginning)))
	  (end (or end (region-end))))
  (mandoku-tls-grep-candidates-1 (buffer-substring-no-properties beg end)))))


(defun mandoku-tls-grep-candidates-1 (search-string)
  (message "Searching..., please wait.")
  (require 'hi-lock)
  (let ((coding-system-for-read 'utf-8)
	(coding-system-for-write 'utf-8)
    ;; bzgrep -n -H -e "於愛" ~/krp/text/KR*/*/*.txt
	(res (split-string
	      (shell-command-to-string
	       (concat mandoku-grep-command " -n -H -e " "\""
		       (mapconcat 'char-to-string (string-to-list search-string) "\(<[^>]+>\|¶\)*")
		       "\" "
		       mandoku-text-dir "KR*/*/*.txt")) "\n") )
	l )
    (dolist (s res)
      (unless (string-match "tls:concept" s)
	(when (string-match mandoku-tls-grep-split-line-regexp s)
	  (let* ((l1 (cl-loop for n from 1 to 3 collect (match-string n s)))
		 (fname (car-safe l1))
		 (txt (split-string (file-name-base fname) "_"))
		 (textid (car txt))
		 (str (mandoku-remove-markup (nth 2 l1)))
		 )
	    (setcar (nthcdr 2 l1) search-string)
	    (push (cons
		   ;; TODO: insert Sigle for date, make title a text property
		   (format "%s,%s %-18.18s　%s" textid (cadr txt)
			   (concat "《" (mandoku-cut-string (mandoku-textid-to-title textid) 7) "》")
			   (mandoku-tls-hi-in-string str search-string))
		   l1) l)))))
    (mandoku-tls-grep-work-file search-string)
    (helm :sources (list (helm-build-sync-source (format "Grep %s" search-string)
			   :action '(("Open file" . (lambda (s)
                                                (find-file-other-window (car s))
                                                (goto-line (string-to-number (cadr s)))
						(hi-lock-mode t)
						(highlight-regexp
						 (mapconcat 'char-to-string (string-to-list (nth 2 s)) "\\(<[^>]*>\\|¶\\)*")
						 )))
			       ("Attribute directly" . (nil))
                               ("Grep CHANT files" . (lambda (s) (mandoku-tls-wg-helm)))
                               )
			   :candidates (nreverse l))
			 (helm-build-dummy-source (if l "CHANT files:" "Please wait for CHANT search to finish.")
			   :action '(("Search in CHANT files:" . (lambda (s) (mandoku-tls-wg-helm))))))
	  :buffer "*TLS Grep*")
    ))

(defun mandoku-tls-wg-helm (&optional candidate)
  "This is to call up the things in *tls-grep*."
  (let (l search-string)
    (unless mandoku-tls-chant-titles
      (when (file-exists-p (concat mandoku-tls-chant-path "titles.txt"))
	(setq mandoku-tls-chant-titles (make-hash-table :test 'equal))
	(with-current-buffer (find-file-noselect (concat mandoku-tls-chant-path "titles.txt"))
	  (let ((sb (split-string (buffer-string) "\n")) line title)
	    (dolist (s sb)
	      (when (string-match "\t" s)
		(setq line (split-string s "\t"))
		(setq title (if (string-match "（\\([^）]+\\)）" (cadr line))
				(match-string 1 (cadr line))
			      (cadr line)))
		(setq title (replace-regexp-in-string "\\(作者\\)?不詳-" ""
		    (replace-regexp-in-string "[一二三四五六七八九十]+卷" ""
		      (replace-regexp-in-string "卷附.*" "卷" title)))))
	      (puthash (car line) title mandoku-tls-chant-titles))))))
    (with-current-buffer "*tls-grep*"
      (setq search-string (car (split-string (thing-at-point 'line) "\n")))
      (dolist (s (split-string (buffer-string) "\n"))
	(when (string-match mandoku-tls-grep-split-line-regexp s)
	  (let* ((l1 (cl-loop for n from 1 to 3 collect (match-string n s)))
		 (fname (file-name-sans-extension (file-name-nondirectory (car-safe l1))))
		 (txt (split-string (file-name-base fname) "_"))
		 (textid (car txt))
		 (str (mandoku-remove-markup  (nth 2 l1)))
		 )
	    (setcar (nthcdr 2 l1) search-string)
	    (push (cons
		   (format "%s,%s %-18.18s　%s" textid (cadr txt)
			   (concat "《" (mandoku-cut-string (gethash textid mandoku-tls-chant-titles) 7) "》")
			   (mandoku-tls-hi-in-string str search-string))
		   l1) l)))))
    (helm :sources (helm-build-sync-source (format "Grep %s" search-string)
                     :action '(("Open file" . (lambda (s)
                                                (find-file-other-window (car s))
                                                (goto-line (string-to-number (cadr s)))
						(hi-lock-mode t)
						(highlight-regexp (nth 2 s))))
                               )
		     :candidates (nreverse l))
	  :buffer "*TLS CHANT Grep*")
    ))

(defun mandoku-tls-grep-work-file (search-string)
  "Updates the index for local files."
  (interactive)
  (let ((grep-buffer (get-buffer-create "*tls-grep*")))
    (with-current-buffer grep-buffer
      (erase-buffer)
      (insert  search-string "\n"))
  (set-process-sentinel
   (start-process-shell-command "*tls-grep-command*" grep-buffer
      (concat mandoku-grep-command " -n -H -e " "\""
	      (mapconcat 'char-to-string (string-to-list search-string) "\(<[^>]+>\|¶\)*")
	      "\" "
	      mandoku-tls-chant-path "*/*.txt"))
   'mandoku-tls-grep-sentinel) ))

(defun mandoku-tls-grep-sentinel (proc msg)
  (when (string-match "exited" msg)
    (with-current-buffer "*tls-grep*"
      (goto-char (point-min)))
    (message "TLS grep finished.")
    (sleep-for 1)
    (message nil)
  ))

(defun mandoku-tls-grep-local-texts (search-for)
  (interactive "sTLS text search -- Enter search term: ")
  (let ((buffer
	 (mandoku-tls-search-user-text search-for mandoku-text-dir)))
    (with-current-buffer grep-last-buffer
      
      )))

(defun mandoku-tls-search-user-text (search-for &optional search-dir)
  "This command searches through the texts located in `mandoku-work-dir'."
  (interactive "s")
  (let ((coding-system-for-read 'utf-8)
	(coding-system-for-write 'utf-8)
	(grep-find-ignored-files nil)
	(grep-find-ignored-directories nil)
	(sd (or search-dir mandoku-work-dir)))
    (if (fboundp 'ripgrep-regexp-x)
	(ripgrep-regexp search-for sd '("-ttxt"))
      (rgrep search-for "*.txt" sd nil))))

(bind-key* (kbd "<f7>") '(lambda () (interactive)
			   (find-file
			    (concat mandoku-tls-root-path "tls/work/taxonomy-2017-02-20.txt"))))

;; capture template
(defun mandoku-tls-capture-att ()
  (when (and (derived-mode-p 'mandoku-dict-mode)
	     (looking-at " \\+ "))
    (save-excursion
      (let* ((line (split-string (thing-at-point 'line) "："))
	    (def (org-get-heading (org-previous-visible-heading 2)))
	    (char (car (split-string (org-get-heading (outline-up-heading 2)) " ")))
            (att-cand (mandoku-tls-sw-candidates char))
            (uuid (mandoku-tls-id-new))
            (res  (replace-regexp-in-string mandoku-tls-punc-regex "\\1\n"
                              (replace-regexp-in-string "[『』〈〉]" "" (cadr line))))
             att new tit)
        (helm :sources (helm-build-sync-source (format "Syntactic words for %s" char)
                         :action '(("Select for attribution" . (lambda (candidate)
                                                                 (setq att candidate))))
		     :candidates  (mandoku-tls-sw-candidates char))
              :buffer "*TLS Syntactic words*")
        (dolist (r (split-string res "\n"))
          (when (string-match char r)
            (setq r (format "%s\n:zhu:\n%s\n:end:" r att)))
          (push r new))
        (when (string-match "《\\([^》]+\\)》" (car line))
          (setq uuid (concat uuid "\n:TITLE: " (match-string 1 (car line)))))
        (setq res (concat  "\n** "
                          (replace-regexp-in-string " \\+ " "" (car line))
                          "\n:PROPERTIES:\n:CUSTOM_ID: "
                          uuid
                          "\n:END:\n# src 漢語大詞典 : " char " : " def "\n%?"
                          (string-join (nreverse new) "\t\n")
                          ""))
        res
	))))

(defun mandoku-tls-dict-immediate-capture ()
  (interactive)
  (let ((org-capture-templates  '(("x" "TLS: Attribute 漢語大詞典 entries" entry
	 (file+headline (concat mandoku-tls-lexicon-path "misc/more-attributions.org") "漢語大詞典")
	 (function mandoku-tls-capture-att)))))
    (org-capture '(current-prefix-arg 4) "x")))

;; this will also be called  from a map-entry call over the whole file

(defun mandoku-tls-read-more-attributions ()
  "Get the attributions in the lexicon/misc files."
  (let ((p (point))
        (hw (org-get-heading ))
        (end (org-end-of-subtree))
        line)
    (goto-char p)
    (while (re-search-forward "@" end t)
      (setq line (thing-at-point 'line))
      
          
      )))

(defun mandoku-tls-hi-in-string (str hi)
  "Highlight 'hi' in str."
  (let ((s (split-string str hi)))
    (mapconcat 'identity s (propertize hi 'face 'hi-yellow))))

(provide 'mandoku-tls)
;;; mandoku-tls.el ends here

