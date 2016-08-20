;;; kolon-mode.el --- Syntax highlighting for Text::Xslate's Kolon syntax
;; 
;; Filename: kolon-mode.el
;; Description: Syntax highlighting for Text::Xslate's Kolon syntax
;; Author: Sam Tran
;; Maintainer: Sam Tran
;; Created: Mon Apr 16 09:26:25 2012 (-0500)
;; Version: 0.1
;; Package-Version: 20140122.334
;; Last-Updated: Thu Jul 12 12:37:29 2012 (-0500)
;;           By: Sam Tran
;;     Update #: 5
;; URL: https://github.com/samvtran/kolon-mode
;; Keywords: xslate, perl
;; Compatibility: GNU Emacs: 23.x, 24.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Highlights Text::Xslate files using the Kolon syntax
;; 
;; Some parts of this code originated from two other projects:
;;
;; https://github.com/yoshiki/tx-mode
;; https://bitbucket.org/lattenwald/.emacs.d/src/347b18c4f834/site-lisp/kolon-mode.el
;;
;; Commands (interactive functions):
;; `kolon-show-version'
;; `kolon-open-docs'
;; `kolon-comment-region'
;; `kolon-uncomment-region'
;;
;; Other functions:
;; `kolon-indent-line'
;; `indent-newline'
;;
;; TODO: It would be nice to figure out how comment-or-uncomment-region
;; works so we can get it to work properly. Right now it'll insert
;; HTML comments, so kolon-(un)comment-region will need to be bound
;; individually or used from their menu entries.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the Artistic License 2.0. For details, see
;; http://www.perlfoundation.org/artistic_license_2_0
;; 
;; This program is distributed in the hope that it will be useful,
;; but it is provided "as is" and without any express or implied
;; warranties. For details, see 
;; http://www.perlfoundation.org/artistic_license_2_0
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'font-lock)
(require 'easymenu)

(defvar kolon-mode-hook nil)

;; Customizable things
(defconst kolon-mode-version "0.1"
  "The version of `kolon-mode'.")

(defgroup kolon nil
  "Major mode for the Xslate Kolon syntax."
  :group 'languages)

(defcustom kolon-indent-offset tab-width
  "Indentation offset for `kolon-mode'."
  :type 'integer
  :group 'kolon)

(defcustom kolon-newline-clear-whitespace nil
  "Clear whitespace on adding a newline."
  :type 'boolean
  :group 'kolon)

(defvar kolon-mode-map (make-keymap)
  "Keymap for `kolon-mode'.")

(defvar kolon-keywords "block\\|override\\|cascade\\|include\\|super\\|before\\|after\\|around\\|with\\|macro")
(defconst kolon-font-lock-keywords 
  (list
   ;; <: foobar :> tags
   ;; was '("\\(<:\\)\\(\\(?:.\\|\\)?+?\\)\\(:>\\)"
   '("\\(<:\\)\\(.*?\\)\\(:>\\)"
	 (1 font-lock-string-face t)
	 (2 font-lock-variable-name-face t)
	 (3 font-lock-string-face t))
   ;; : foobar -> {} line code
   '("\\(^[ \t]*:\\)\\(\\(?:.\\)*?\\)\\({.*}?;?\\|}\\|;\\|$\\)"
	 (1 font-lock-string-face t)
	 (2 font-lock-variable-name-face t)
	 (3 font-lock-string-face t))
   ;; : # comments only line code
   '("^[ \t]*\\(:\s*#\\)\\(.*$\\)"
	 (1 font-lock-comment-delimiter-face t)
	 (2 font-lock-comment-face t))
   ;; : foobar -> {} # comments at end of line code
   '("\\(^[ \t]*:\\)\\(\\(?:.\\)*?\\)\\({.*}?;?\\|}\\|;\\|$\\)?\\([ \t]*#\\)\\(.*$\\)"
	 (4 font-lock-comment-delimiter-face t)
	 (5 font-lock-comment-face t))
   ;; <: $foobar #comments :>
   ;; was '("\\(<:\\)\\(\\(?:.\\|\\)?+?\\)\\(#\\)\\(.*?\\)\\(:>\\)"
   ;; New regex stops the second .*? from grabbing :>
   ;; which makes '<: :> # <: :>' comment '# <:' when it shouldn't
   '("\\(<:\\)\\(.*?\\)\\(#\\)\\([^:>]*?\\)\\(:>\\)"
	 (3 font-lock-comment-delimiter-face t)
	 (4 font-lock-comment-face t))
   (list
   	(concat "\\b\\(" kolon-keywords "\\)\\b") 
   	1 font-lock-keyword-face t)
   )
  "Expressions to font-lock in kolon-mode.")

(defun kolon-indent-line ()
  "Indent current line for Kolon mode."
  (let (previous-indentation)
	(save-excursion
	  (forward-line -1)
	  (setq previous-indentation (current-indentation)))
	(if (< (current-indentation) previous-indentation)
		(indent-line-to previous-indentation)
	  (indent-line-to (+ (current-indentation) kolon-indent-offset)))))

;; Prevents indentation at bob, bol, whitespace-only lines (if enabled),
;; or lines at column 0
(defun indent-newline ()
  "Newline and indents"
  (interactive)
  (cond
   ;; If we're on a whitespace-only line,
   ;; AND only if kolon-newline-clear-whitespace is enabled
   ((and (eolp)
		 kolon-newline-clear-whitespace
   		 (save-excursion (re-search-backward "^\\(\\s \\)*$"
   											 (line-beginning-position) t)))
   	(kill-line 0)
   	(newline))

   ;; Catches bob, bol, and everything at column 0 (i.e., not indented)
   ((= 0 (current-indentation)) (newline))
   
   ;; Else (not on whitespace-only) insert a newline,
   ;; then add the appropriate indent:
   (t (insert "\n")
	  (indent-according-to-mode)))
  )

;(make-local-variable 'comment-start)
;(setq comment-start ":#")
;(make-local-variable 'comment-end)
;(setq comment-end "")
(defun kolon-comment-region (beg end arg)
  "Comment each line in the region in kolon-mode.
See `comment-region'."
  (interactive "r\np")
  (let ((comment-start ":#") (comment-end ""))
	(comment-region beg end arg)))

(defun kolon-uncomment-region (beg end)
  "Uncomment each line in the region in kolon-mode.
See `comment-region'."
  (interactive "r")
  ;; Note: This is what ruby mode uses. Has issues with grabbing too much.
  ;; Will need to investigate more "official" ways of doing these
  (save-excursion
	(goto-char beg)
	(while (re-search-forward "^\\([ \t]*\\):\s*#\s?" end t)
	  (replace-match "\\1" nil nil))))

;; Menubar
(easy-menu-define kolon-mode-menu kolon-mode-map
  "Menu for `kolon-mode'."
  '("Kolon"
	["Comment Region" kolon-comment-region]
	["Uncomment Region" kolon-uncomment-region]
	["Kolon Docs" kolon-open-docs]
	["Version" kolon-show-version]
	))

;; Print version number
(defun kolon-show-version ()
  "Prints the version number of `kolon-mode'."
  (interactive)
  (message (concat "kolon-mode version " kolon-mode-version)))

;; Show Kolon docs
;;;###autoload
(defun kolon-open-docs ()
  "Shows Kolon syntax docs in the browser."
  (interactive)
  (browse-url "http://search.cpan.org/dist/Text-Xslate/lib/Text/Xslate/Syntax/Kolon.pm"))

;;;###autoload
(define-derived-mode kolon-mode html-mode
  (font-lock-add-keywords nil kolon-font-lock-keywords)
  (make-local-variable 'kolon-indent-offset)
  (set (make-local-variable 'indent-line-function) 'kolon-indent-line)

  ;; Keybindings
  (define-key kolon-mode-map (kbd "C-c C-k") 'kolon-open-docs)
  (setq mode-name "Kolon"))

(provide 'kolon-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; kolon-mode.el ends here
