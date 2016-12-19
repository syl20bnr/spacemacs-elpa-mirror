;;; xah-lookup.el --- look up word on internet. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2011-2016 by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Version: 2.1.5
;; Package-Version: 20161218.2303
;; Created: 14 Nov 2011
;; Package-Requires: ((emacs "24.1"))
;; Keywords: help, docs, convenience
;; URL: http://ergoemacs.org/emacs/xah-lookup.html

;; This file is not part of GNU Emacs.

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; This package provide commands for looking up the web of word under cursor.

;; xah-lookup-word-on-internet
;; xah-lookup-google           ; 【C-h 7】 or 【F1 7】
;; xah-lookup-wikipedia        ; 【C-h 8】 or 【F1 8】
;; xah-lookup-word-definition  ; 【C-h 9】 or 【F1 9】
;; xah-lookup-word-dict-org
;; xah-lookup-answers.com
;; xah-lookup-wiktionary

;; If there's a text selection (a phrase you want to lookup), these commands will act on the selection.

;; If you prefer to use emacs 24.4's builtin eww browser, put the following in your emacs init
;; (require 'eww)
;; (setq xah-lookup-browser-function 'eww) ; or 'browse-url

;; For commands that lookup English word definition, you can specify browser separately.
;; (setq xah-lookup-dictionary-browser-function 'eww) ; or 'browse-url

;; To change/add keys, put the following in your emacs init.
;; (define-key help-map (kbd "7") 'xah-lookup-google)
;; Change the command to the one you want, or `nil' to reset.

;; You can also create your own lookup command to lookup perl, ruby, php, clojure, etc.
;; See: http://ergoemacs.org/emacs/xah-lookup.html

;; Like it?
;; Buy Xah Emacs Tutorial
;; http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html

;;; INSTALL:

;; To install manually, place this file 〔xah-lookup.el〕 in the directory 〔~/.emacs.d/lisp/〕.

;; Then, place the following code in your emacs init file

;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (autoload 'xah-lookup-google "xah-lookup" "Lookup in browser" t)
;; (autoload 'xah-lookup-wikipedia "xah-lookup" "Lookup in browser" t)
;; (autoload 'xah-lookup-word-dict-org "xah-lookup" "Lookup in browser" t)
;; (autoload 'xah-lookup-word-definition "xah-lookup" "Lookup in browser" t)
;; (autoload 'xah-lookup-wiktionary "xah-lookup" "Lookup in browser" t)

;;; HISTORY:

;; 2014-10-20 changes are no longer logged here, unless major.
;; version 1.5, 2013-04-21 removed xah-lookup-php-ref. Doesn't belong here.
;; version 1.4, 2013-03-23 added 2 more dict to the xah-lookup-dictionary-list. Good for vocabulary researchers
;; version 1.3, 2012-05-11 added “xah-lookup-xah-lookup-dictionary-list”.
;; version 1.2, 2012-05-10 added “xah-lookup-answers.com”. Improved inline docs.
;; version 1.1, 2012-05-09 changed the input from 「'symbol」 to 「'word」. Changed the English dictionary used from 「http://www.answers.com/main/ntquery?s=�」 to 「http://www.thefreedictionary.com/�」.
;; version 1.0, 2011-11-14 First released to public.


;;; Code:

(require 'browse-url) ; in emacs

(defcustom
  xah-lookup-browser-function
  'browse-url
  "Function to call to launch browser. Default is 'browse-url. You can also use 'eww. For dictionary lookup, use `xah-lookup-dictionary-browser-function'"
  :group 'xah-lookup
  )

(defcustom
  xah-lookup-dictionary-browser-function
  'browse-url
  "Function to call for English definition lookup. Default is 'browse-url. You can also use 'eww. For dictionary lookup, use `xah-lookup-dictionary-browser-function'"
  :group 'xah-lookup)

(defcustom
  xah-lookup-dictionary-list
  [
   "http://www.dict.org/bin/Dict?Form=Dict2&Database=*&Query=�" ; 1913 Webster, WordNet
   "http://www.thefreedictionary.com/�"                         ; AHD
   "http://www.answers.com/main/ntquery?s=�"                    ; AHD
   "http://en.wiktionary.org/wiki/�"
   "http://www.google.com/search?q=define:+�"     ; google
   "http://www.etymonline.com/index.php?search=�" ; etymology
   ]
  "A vector of dictionaries. Used by `xah-lookup-all-dictionaries'. http://wordyenglish.com/words/dictionary_tools.html "
  :group 'xah-lookup)

(defun xah-lookup--asciify-region (&optional *from *to)
  "Change some Unicode characters into equivalent ASCII ones.
For example, “passé” becomes “passe”.

This function works on chars in European languages, and does not transcode arbitrary Unicode chars (such as Greek, math symbols).  Un-transformed unicode char remains in the string.

When called interactively, work on text selection or current line.
Version 2014-10-20"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((case-fold-search t))
    (save-restriction
      (narrow-to-region *from *to)
      (mapc
       (lambda (-pair)
         (goto-char (point-min))
         (while (search-forward-regexp (elt -pair 0) (point-max) t)
           (replace-match (elt -pair 1))))
       [
        ["á\\|à\\|â\\|ä\\|ã\\|å" "a"]
        ["é\\|è\\|ê\\|ë" "e"]
        ["í\\|ì\\|î\\|ï" "i"]
        ["ó\\|ò\\|ô\\|ö\\|õ\\|ø" "o"]
        ["ú\\|ù\\|û\\|ü"     "u"]
        ["Ý\\|ý\\|ÿ"     "y"]
        ["ñ" "n"]
        ["ç" "c"]
        ["ð" "d"]
        ["þ" "th"]
        ["ß" "ss"]
        ["æ" "ae"]
        ]))))

(defun xah-lookup--asciify-string (*string)
  "Change some Unicode characters into equivalent ASCII ones.
For example, “passé” becomes “passe”.
See `xah-lookup--asciify-region'
Version 2014-10-20"
  (with-temp-buffer
      (insert *string)
      (xah-lookup--asciify-region (point-min) (point-max))
      (buffer-string)))

(defun xah-lookup-word-on-internet (&optional *word *site-to-use *browser-function)
  "Look up current word or text selection in a online reference site.
This command launches/switches you to default browser.

*SITE-TO-USE a is URL string in this form: 「http://en.wiktionary.org/wiki/�」.
the 「�」 is a placeholder for the query string.

If *SITE-TO-USE is nil, Google Search is used.

For a list of online reference sites, see:
 URL `http://ergoemacs.org/emacs/xah-lookup.html'"
  (interactive)
  (let (-word -refUrl -myUrl)
    (setq -word
          (if *word
              *word
            (if (region-active-p)
                (buffer-substring-no-properties (region-beginning) (region-end))
              (current-word))))

    (setq -word (replace-regexp-in-string " " "%20" (xah-lookup--asciify-string -word)))

    (setq -refUrl
          (if *site-to-use
              *site-to-use
            "http://www.google.com/search?q=�" ))

    (setq -myUrl (replace-regexp-in-string "�" -word -refUrl t t))

    (if (null *browser-function)
        (funcall xah-lookup-browser-function -myUrl)
      (funcall *browser-function -myUrl))))

;;;###autoload
(defun xah-lookup-google (&optional *word)
  "Lookup current word or text selection in Google Search."
  (interactive)
  (xah-lookup-word-on-internet
   *word
   "http://www.google.com/search?q=�") )

;;;###autoload
(defun xah-lookup-wikipedia (&optional *word)
  "Lookup current word or text selection in Wikipedia."
  (interactive)
  (xah-lookup-word-on-internet
   *word
   "http://en.wikipedia.org/wiki/�") )

;;;###autoload
(defun xah-lookup-word-definition (&optional *word)
  "Lookup definition of current word or text selection in URL `http://thefreedictionary.com/'."
  (interactive)
  (xah-lookup-word-on-internet
   *word
   "http://www.thefreedictionary.com/�"
   xah-lookup-dictionary-browser-function) )

(defun xah-lookup-word-dict-org (&optional *word)
  "Lookup definition of current word or text selection in URL `http://dict.org/'."
  (interactive)
  (xah-lookup-word-on-internet
   *word
   "http://www.dict.org/bin/Dict?Form=Dict2&Database=*&Query=�"
   xah-lookup-dictionary-browser-function))

(defun xah-lookup-answers.com (&optional *word)
  "Lookup current word or text selection in URL `http://answers.com/'."
  (interactive)
  (xah-lookup-word-on-internet
   *word
   "http://www.answers.com/main/ntquery?s=�"
   xah-lookup-dictionary-browser-function))

(defun xah-lookup-wiktionary (&optional *word)
  "Lookup definition of current word or text selection in URL `http://en.wiktionary.org/'"
  (interactive)
  (xah-lookup-word-on-internet
   *word
   "http://en.wiktionary.org/wiki/�"
   xah-lookup-dictionary-browser-function))

(defun xah-lookup-all-dictionaries (&optional *word)
  "Lookup definition in many dictionaries.
Current word or text selection is used as input.
The dictionaries used are in `xah-lookup-dictionary-list'."
  (interactive)
  (mapc
   (lambda
     (-url)
     (xah-lookup-word-on-internet *word -url xah-lookup-dictionary-browser-function))
   xah-lookup-dictionary-list))

(define-key help-map (kbd "7") 'xah-lookup-google)
(define-key help-map (kbd "8") 'xah-lookup-wikipedia)
(define-key help-map (kbd "9") 'xah-lookup-word-definition)

(provide 'xah-lookup)

;;; xah-lookup.el ends here
