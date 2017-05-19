;;; xah-lookup.el --- look up word on internet. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2011-2017 by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Version: 3.2.5
;; Package-Version: 20170517.1459
;; Created: 14 Nov 2011
;; Package-Requires: ((emacs "24.1"))
;; Keywords: help, docs, convenience
;; URL: http://ergoemacs.org/emacs/xah-lookup.html

;; This file is not part of GNU Emacs.

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; This package provide commands for looking up the web of word under cursor.

;; • xah-lookup-word-on-internet
;; • xah-lookup-google
;; • xah-lookup-wikipedia
;; • xah-lookup-word-definition
;; • xah-lookup-word-dict-org
;; • xah-lookup-wiktionary
;; • xah-lookup-etymology

;; If there's a text selection (a phrase you want to lookup), these commands will lookup the selected text.

;;; INSTALL:

;; To install manually, place this file xah-lookup.el in the directory ~/.emacs.d/lisp/

;; Then, place the following code in your emacs init file

;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (require 'xah-lookup)

;;; CUSTOMIZATION

;; If you prefer to use emacs 24.4's builtin eww browser, put the following in your emacs init
;; (require 'xah-lookup)
;; (require 'eww)
;; (setq xah-lookup-browser-function 'eww)

;; for operating system's default browser, use 'browse-url instead of 'eww

;; Each command can use a different URL or browser. For example:
;; (require 'xah-lookup)
;; (require 'eww)
;; (put 'xah-lookup-word-definition 'xah-lookup-url "http://www.thefreedictionary.com/word02051")
;; (put 'xah-lookup-word-definition 'xah-lookup-browser-function 'eww)

;; To change/add keys, put the following in your emacs init. For example:
;; (define-key help-map (kbd "7") 'xah-lookup-google) ; C-h 7
;; or
;; (global-set-key (kbd "<f2>") 'xah-lookup-word-definition) ; F2

;; You can also create your own lookup command to lookup ruby, php, clojure, etc.
;; For example:

;; (defun my-lookup-php (&optional *word)
;;   "lookup php doc of word under cursor"
;;   (interactive)
;;   (require 'xah-lookup)
;;   (xah-lookup-word-on-internet
;;    *word
;;    (get 'my-lookup-php 'xah-lookup-url )
;;    (get 'my-lookup-php 'xah-lookup-browser-function )))
;; (put 'my-lookup-php 'xah-lookup-url "http://us.php.net/word02051")
;; (put 'my-lookup-php 'xah-lookup-browser-function 'browse-url)

;; See: http://ergoemacs.org/emacs/xah-lookup.html

;; Like it?
;; Buy Xah Emacs Tutorial
;; http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html

;;; HISTORY:

;; 2017-02-09 each command can be costomized for lookup URL and browser to use.
;; 2014-10-20 changes are no longer logged here, unless major.
;; version 1.5, 2013-04-21 removed xah-lookup-php-ref. Doesn't belong here.
;; version 1.4, 2013-03-23 added 2 more dict to the xah-lookup-dictionary-list. Good for vocabulary researchers
;; version 1.3, 2012-05-11 added “xah-lookup-xah-lookup-dictionary-list”.
;; version 1.2, 2012-05-10 added “xah-lookup-answers.com”. Improved inline docs.
;; version 1.1, 2012-05-09 changed the input from 「'symbol」 to 「'word」. Changed the English dictionary used from 「http://www.answers.com/main/ntquery?s=word02051」 to 「http://www.thefreedictionary.com/word02051」.
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
   "http://www.dict.org/bin/Dict?Form=Dict2&Database=*&Query=word02051" ; 1913 Webster, WordNet
   "http://www.thefreedictionary.com/word02051"                         ; AHD
   "http://en.wiktionary.org/wiki/word02051"
   "http://www.google.com/search?q=define:+word02051"     ; google
   "http://www.etymonline.com/index.php?search=word02051" ; etymology
   ]
  "A vector of website URLs for lookup words. Used by `xah-lookup-all-dictionaries'. http://wordyenglish.com/words/dictionary_tools.html "
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

(defun xah-lookup-word-on-internet (&optional *word *url *browser-function)
  "Look up current word or text selection in a online reference site.
This command launches/switches you to default browser.

*URL a is URL string in this form: 「http://en.wiktionary.org/wiki/word02051」.
the 「word02051」 is a placeholder for the query string.

If *URL is nil, Google Search is used.

For a list of online reference sites, see:
 URL `http://ergoemacs.org/emacs/xah-lookup.html'
Version 2017-02-09"
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
          (if *url
              *url
            "http://www.google.com/search?q=word02051" ))
    (setq -myUrl (replace-regexp-in-string "word02051" -word -refUrl t t))
    (if *browser-function
        (funcall *browser-function -myUrl)
      (funcall xah-lookup-browser-function -myUrl))))

;;;###autoload
(defun xah-lookup-google (&optional *word)
  "Lookup current word or text selection in Google Search.
Version 2017-02-09"
  (interactive)
  (xah-lookup-word-on-internet
   *word
   (get 'xah-lookup-google 'xah-lookup-url)
   (get 'xah-lookup-google 'xah-lookup-browser-function )))

(put 'xah-lookup-google 'xah-lookup-url "http://www.google.com/search?q=word02051")
(put 'xah-lookup-google 'xah-lookup-browser-function xah-lookup-browser-function)

;;;###autoload
(defun xah-lookup-wikipedia (&optional *word)
  "Lookup current word or text selection in Wikipedia.
Version 2017-02-09"
  (interactive)
  (xah-lookup-word-on-internet
   *word
   (get 'xah-lookup-wikipedia 'xah-lookup-url )
   (get 'xah-lookup-wikipedia 'xah-lookup-browser-function )))

(put 'xah-lookup-wikipedia 'xah-lookup-url "http://en.wikipedia.org/wiki/word02051")
(put 'xah-lookup-wikipedia 'xah-lookup-browser-function xah-lookup-browser-function)

;;;###autoload
(defun xah-lookup-word-definition (&optional *word)
  "Lookup definition of current word or text selection in URL `http://www.thefreedictionary.com/curlicue'.
Version 2017-02-09"
  (interactive)
  (xah-lookup-word-on-internet
   *word
   (get 'xah-lookup-word-definition 'xah-lookup-url )
   (get 'xah-lookup-word-definition 'xah-lookup-browser-function ))
  ;;
  )

(put 'xah-lookup-word-definition 'xah-lookup-url "https://www.ahdictionary.com/word/search.html?q=word02051")
(put 'xah-lookup-word-definition 'xah-lookup-url "http://www.thefreedictionary.com/word02051")

(put 'xah-lookup-word-definition 'xah-lookup-browser-function 'browse-url)

(defun xah-lookup-word-dict-org (&optional *word)
  "Lookup definition of current word or text selection in URL `http://dict.org/'.
Version 2017-02-09"
  (interactive)
  (xah-lookup-word-on-internet
   *word
   (get 'xah-lookup-word-dict-org 'xah-lookup-url )
   (get 'xah-lookup-word-dict-org 'xah-lookup-browser-function )))

(put 'xah-lookup-word-dict-org 'xah-lookup-url "http://www.dict.org/bin/Dict?Form=Dict2&Database=*&Query=word02051")
(put 'xah-lookup-word-dict-org 'xah-lookup-browser-function 'eww)

(defun xah-lookup-wiktionary (&optional *word)
  "Lookup definition of current word or text selection in URL `http://en.wiktionary.org/'
Version 2017-02-09"
  (interactive)
  (xah-lookup-word-on-internet
   *word
   (get 'xah-lookup-wiktionary 'xah-lookup-url )
   (get 'xah-lookup-wiktionary 'xah-lookup-browser-function )))

(put 'xah-lookup-wiktionary 'xah-lookup-url "http://en.wikipedia.org/wiki/word02051")
(put 'xah-lookup-wiktionary 'xah-lookup-browser-function xah-lookup-browser-function)

(defun xah-lookup-etymology (&optional *word)
  "Lookup etymology of current word or text selection in URL `http://www.etymonline.com/index.php?search=curlicue'.
Version 2017-02-27"
  (interactive)
  (xah-lookup-word-on-internet
   *word
   (get 'xah-lookup-etymology 'xah-lookup-url )
   (get 'xah-lookup-etymology 'xah-lookup-browser-function )))

(put 'xah-lookup-etymology 'xah-lookup-url "http://www.etymonline.com/index.php?search=word02051")
(put 'xah-lookup-etymology 'xah-lookup-browser-function 'eww)

(defun xah-lookup-all-dictionaries (&optional *word)
  "Lookup definition in many dictionaries.
Current word or text selection is used as input.
The dictionaries used are in `xah-lookup-dictionary-list'."
  (interactive)
  (mapc
   (lambda (-url)
     (xah-lookup-word-on-internet
      *word
      -url
      (get 'xah-lookup-all-dictionaries 'xah-lookup-browser-function )))
   xah-lookup-dictionary-list))

(put 'xah-lookup-all-dictionaries 'xah-lookup-browser-function 'browse-url)

(define-key help-map (kbd "1") 'xah-lookup-etymology)
(define-key help-map (kbd "2") 'xah-lookup-word-dict-org)
(define-key help-map (kbd "3") 'xah-lookup-google)
(define-key help-map (kbd "4") 'xah-lookup-word-definition)
(define-key help-map (kbd "5") 'xah-lookup-wikipedia)
(define-key help-map (kbd "6") 'xah-lookup-wiktionary)
(define-key help-map (kbd "7") 'xah-lookup-all-dictionaries)
(define-key help-map (kbd "9") 'xah-lookup-word-definition)

(when (boundp 'xah-fly-h-keymap)
  (define-key xah-fly-h-keymap (kbd "1") 'xah-lookup-etymology)
  (define-key xah-fly-h-keymap (kbd "2") 'xah-lookup-word-dict-org)
  (define-key xah-fly-h-keymap (kbd "3") 'xah-lookup-google)
  (define-key xah-fly-h-keymap (kbd "4") 'xah-lookup-word-definition)
  (define-key xah-fly-h-keymap (kbd "5") 'xah-lookup-wikipedia)
  (define-key xah-fly-h-keymap (kbd "6") 'xah-lookup-wiktionary)
  (define-key xah-fly-h-keymap (kbd "7") 'xah-lookup-all-dictionaries)
  (define-key xah-fly-h-keymap (kbd "9") 'xah-lookup-word-definition)
  ;;
  )

(provide 'xah-lookup)

;;; xah-lookup.el ends here
