;;; minibuf-isearch.el --- incremental search on minibuffer history

;; Copyright (C) 2002 Keiichiro Nagano <knagano@sodan.org>
;;
;; Filename: minibuf-isearch.el
;; Version: 1.7.1
;; Package-Version: 20151226.1143
;; Author: Keiichiro Nagano <knagano@sodan.org>,
;;         Hideyuki SHIRAI  <shirai@meadowy.org>
;; Maintenance: Keiichiro Nagano <knagano@sodan.org>
;; Keywords: minibuffer, history, incremental search

;;; This file is *NOT* (yet?) part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This package enables incremental-searching on minibuffer history.
;;
;; Put this code into your .emacs:
;;
;; (require 'minibuf-isearch)
;;
;; Then type C-r in minibuffer and you'll be happy.  (yes, I suppose!)
;; See 'M-x describe-function minibuf-isearch-mode' for details.

;;; History:
;; + Version 1.7.1, 04 Jun 2007
;;   by Keiichiro Nagano <knagano@sodan.org>:
;;   - Bugfix.  Thanks to Wataru Tachibana <MLB33828 AT nifty DOT com>.
;;
;; + Version 1.7, 28 May 2007
;;   by Keiichiro Nagano <knagano@sodan.org>:
;;   - New feature 'shrink-completion-window,' shrinks the completion
;;   window to exactly fit its contents. (enabled by default)
;;   Patch by Per Nordlow <per AT foi DOT se> and Shirai-san.
;;
;; + Version 1.6.1, 23 Jul 2005
;;   by Keiichiro Nagano <knagano@sodan.org>:
;;   - Bugfix.  Patch by Ramkumar R <andyetitmoves AT gmail DOT com>.
;;
;; + Version 1.6, 10 Jul 2005
;;   by Keiichiro Nagano <knagano@sodan.org>:
;;   - Add autoload support by Ramkumar R <andyetitmoves AT gmail DOT com>
;;
;; + Version 1.5, 04 Apr 2004
;;   by Hideyuki SHIRAI <shirai@meadowy.org>:
;;   - Support exit minibuf-isearch, and exit minibuffer too.
;;     Set `minibuf-isearch-exit-and-exit-keys' as you like.
;;   - Support `filecache'.
;;     Set your ~/.emacs like the following.
;;        (eval-after-load
;;            "filecache"
;;          '(progn
;;             (file-cache-add-directory-list load-path)))
;;
;;        (add-hook 'minibuf-isearch-mode-hook
;;      	    (lambda () (require 'filecache)))
;;   - Add keys; 'M-r', 'M-p', 'M-s', 'M-n', 'M-h', Let's try!!
;;   - Change the way of `minibuf-isearch-mode' executing; run mode-hook.
;;   - New option `minibuf-isearch-show-completion-steps'.
;;   - Remove option `minibuf-isearch-show-completion-always'
;;
;; + Version 1.4, 17 Feb 2004
;;   by Hideyuki SHIRAI <shirai@meadowy.org>:
;;   - Support completions and multiple keys bind to kick minibuf-isearch.
;;     See `minibuf-isearch-fire-keys', `minibuf-isearch-reverse-fire-keys'
;;     , `minibuf-isearch-always-with-complete' and etc...
;;   - Change default value in `minibuf-isearch-show-completion-always'.
;;   - Support '^' at head and '$' at tail.
;;   - New construction `minibuf-isearch-ignore-case'.
;;   - When searching is faile, retry another direction.
;;   - Support calculation on overlay strings in minibuffer for kogiku
;;     (ex. before-string, after-string).
;;
;; + Version 1.3, 09 Feb 2004
;;   by Hideyuki SHIRAI <shirai@meadowy.org>:
;;   - If `input-pending', don't execute `minibuf-isearch-do-search'.
;;
;; + Version 1.2, 31 Jul 2003
;;   by Hideyuki SHIRAI <shirai@meadowy.org>:
;;   - Support XEmacs.
;;   - Now you can type C-i (minibuf-isearch-show-completion) to show
;;     the completion window. If you set 'minibuf-isearch-show-completion-always'
;;     to non-nil, show the completion window anytime.
;;   - Use custom.
;;
;; + Version 1.1, 02 Jun 2002
;;   by Keiichiro Nagano <knagano@sodan.org>:
;;   - Hideyuki SHIRAI <shirai@meadowy.org> made a great patch
;;     again (my gratitude.)  New features presented by him are:
;;     - Now minibuf-isearch can show the word you've typed *all the
;;       time* at the right side of the minibuffer.  You can toggle
;;       this feature by the variable
;;       'minibuf-isearch-display-message-always.'
;;     - Now you can toggle the position where minibuf-isearch
;;       messages appear by the variable
;;       'minibuf-isearch-message-on-right.'
;;     - Now C-g finishes minibuf-isearch-mode itself.
;;
;; + Version 1.0, 19 Jan 2002
;;   by Keiichiro Nagano <knagano@sodan.org>:
;;   - Hideyuki SHIRAI <shirai@meadowy.org> made a great patch
;;     (thanks.)  New features added by the patch are:
;;     - Now you can type C-s (minibuf-isearch-next) to search
;;       *forward* the word you've typed.
;;     - Now minibuf-isearch can cooperate with Migemo
;;       (http://migemo.namazu.org/).  You can search Japanese by
;;       ASCII characters when Migemo is available on your Emacs.
;;     - Now 'no match' message is displayed on the right side of the
;;       minibuffer.  You are not annoyed by the error messages any
;;       longer.
;;   - Added many docstrings.  M-x checkdoc passed.
;;   - minibuf-isearch-regexp-quote -> minibuf-isearch-make-regexp.
;;
;; + Version 0.3, 11 Jan 2002
;;   by Keiichiro Nagano <knagano@sodan.org>:
;;   - First version published at my website.
;;
;; + Version 0.2, 11 Jan 2002
;;   by Keiichiro Nagano <knagano@sodan.org>:
;;   - Now minibuf-isearch can display indicator that tells
;;     minibuf-isearch-mode is active.
;;   - Now minibuf-isearch can cope nice with icomplete by saving and
;;     restoring {pre,post}-command-hook not to display mandatory "(No
;;     match)".
;;   - Now this supports abortion (minibuf-isearch-abort), restores
;;     initial content when C-g is pressed.
;;   - Now this supports match highlighting.
;;   - Fixed minibuf-isearch-history-position adjustment bug.
;;
;; + Original version 0.1, 08 Jan 2002
;;   by Keiichiro Nagano <knagano@sodan.org>:
;;   - proof-of-concept prototypical code
;;   - the original idea and suggestions by Satoru Takabayashi
;;     <satoru@namazu.org> (thanks)
;;   - based on gmhist.el modified by HIROSE yuuji <yuuji@gentei.org>
;;     (thanks)
;;   - released for ELF mailing list
;;     (http://www.gentei.org/~yuuji/ml/ELF/)
;;

(require 'easy-mmode)
;; This package assumes 'isearch' face exists

;;; Code:
(eval-when-compile
  (require 'cl))

;;; variables
(defgroup minibuf-isearch nil
  "Incremental search minor mode in minibuffer."
  :prefix "minibuf-isearch-"
  :group 'matching)

(defconst minibuf-isearch-version "1.7.1"
  "Version number of minibuf-isearch.")

(defcustom minibuf-isearch-ignore-case t
  "*Ignore case in minibuf-isearch."
  :type 'boolean
  :group 'minibuf-isearch)

(defcustom minibuf-isearch-use-migemo t
  "*If non-nil, use migeme when isearch."
  :type 'boolean
  :group 'minibuf-isearch)

;;;###autoload
(defcustom minibuf-isearch-fire-keys '("\C-r")
  "*Executing keys of minibuf-isearch."
  :type '(repeat (sexp :tag "Key bind"))
  :group 'minibuf-isearch)

;;;###autoload
(defcustom minibuf-isearch-reverse-fire-keys '("\M-r")
  "*Executing keys of minibuf-isearch with prefix argument."
  :type '(repeat (sexp :tag "Key bind"))
  :group 'minibuf-isearch)

(defcustom minibuf-isearch-exit-and-exit-keys '("\C-j")
  "*Key of exit minibuf-isearch, and exit originai minibuffer too."
  :type '(repeat (sexp :tag "Key bind"))
  :group 'minibuf-isearch)

(defcustom minibuf-isearch-always-with-complete nil
  "*If non-nil, minibuf-isearch treats `all-completions'."
  :type 'boolean
  :group 'minibuf-isearch)

(defcustom minibuf-isearch-indicator-string "(isearch) "
  "*Indicator string displayed while minibuf-isearch mode is active."
  :type 'string
  :group 'minibuf-isearch)

(defcustom minibuf-isearch-display-message-always t
  "*If non-nil, display isearch string in minibuffer all the time."
  :type 'boolean
  :group 'minibuf-isearch)

(defcustom minibuf-isearch-message-on-right nil
  "*If non-nil, display strings of message on right in minibuffer."
  :type 'boolean
  :group 'minibuf-isearch)

(defcustom minibuf-isearch-show-completion-steps
  '((extended-command-history . 3)	;; for Emacs
    (read-command-history . 3)		;; for XEmacs
    (file-name-history . 3)
    (w3m-input-url-history . 3)
    (buffer-name-history . nil)
    (t . 1))
  "*A pair of history-symbol and length of input string which display completion window timing."
  :type '(repeat (cons (choice (variable :tag "History symbol")
			       (const :tag "Others" t))
		       (choice (integer :tag "Input string length")
			       (const :tag "Do not show" nil))))
  :group 'minibuf-isearch)

(defcustom minibuf-isearch-match-format-string "[%s]"
  "*Format string of message displayed when some match are found."
  :type 'string
  :group 'minibuf-isearch)

(defcustom minibuf-isearch-no-match-format-string "[No further match with %s]"
  "*Format string of error message displayed when no match are found."
  :type 'string
  :group 'minibuf-isearch)

(defcustom minibuf-isearch-treat-filecache t
  "*If non-nil, treat `filecache' when filename complate."
  :type 'boolean
  :group 'minibuf-isearch)

(defcustom minibuf-isearch-filecache-completion-tables
  '(read-file-name-internal ffap-read-file-or-url-internal)
  "*Table to use filecache."
  :type '(repeat symbol)
  :group 'minibuf-isearch)

(defcustom minibuf-isearch-shrink-completion-window t
  "*If non-nil, shrink completion window to fit its contents."
  :type 'boolean
  :group 'minibuf-isearch)

(defface minibuf-isearch-face
  '((((class color) (background light))
     (:background "dark khaki" :bold t :underline t))
    (((class color) (background dark))
     (:background "blue" :bold t :underline t))
    (t (:bold t :underline t)))
  "*Face of minibuf-isearch."
  :group 'minibuf-isearch)

(defface minibuf-isearch-comp-face
  '((((class color) (background light))
     (:background "khaki" :underline t))
    (((class color) (background dark))
     (:background "navy" :underline t))
    (t (:underline t)))
  "*Face of completion window of minibuf-isearch."
  :group 'minibuf-isearch)

;; internals
(defvar minibuf-isearch-input-string "")
(defvar minibuf-isearch-original-icomplete-mode)
(defvar minibuf-isearch-minibuf-initial-content)
(defvar minibuf-isearch-original-prepost-command-hook)
(defvar minibuf-isearch-overlay (unless (featurep 'xemacs) (make-overlay 0 0)))
(defvar minibuf-isearch-message-use-redraw (< emacs-major-version 21))
(defvar minibuf-isearch-history-position 0)
(defvar minibuf-isearch-history-position-original 0)
(defvar minibuf-isearch-wincfg nil)
(defvar minibuf-isearch-completion-buff " *minibuf-isearch*")
(defvar minibuf-isearch-last-message "")
(defvar minibuf-isearch-last-regexp "")
(defvar minibuf-isearch-mode-map nil "Keymap for minibuf-isearch mode.")
(defvar minibuf-isearch-with-completion nil)
(defvar minibuf-isearch-history nil)
(defvar minibuf-isearch-do-search-regexp "")
(defvar minibuf-isearch-original-exit-func nil)
(defvar minibuf-isearch-filecache-list nil)
(defvar minibuf-isearch-regexp-alist nil)
(defvar minibuff-isearch-mode-line "")
(defvar minibuff-isearch-mode-name "")
(defvar minibuff-isearch-mode-info "")
(defvar minibuff-isearch-mode-matchenum 0)
(defvar minibuf-isearch-comp-winheight nil)

(defvar minibuf-isearch-debug nil)

(defmacro minibuf-isearch-ifdebug (&rest body)
  "Evaluate BODY iff.  `minibuf-isearch-debug' is non-nil.
\(for debugging purposes only)"
  `(if minibuf-isearch-debug
       (progn ,@body)))

;;; interactives

;; entry point

;;;###autoload
(defun minibuf-isearch-backward-reverse (&optional args)
  "Start backward incremental searching on minibuffer history with prefix."
  (interactive "P")
  (minibuf-isearch-backward (null args)))

;;;###autoload
(defun minibuf-isearch-backward (&optional args)
  "Start backward incremental searching on minibuffer history."
  (interactive "P")
  (if (or (and args (not minibuf-isearch-always-with-complete))
	  (and (not args) minibuf-isearch-always-with-complete))
      (setq minibuf-isearch-with-completion t)
    (setq minibuf-isearch-with-completion nil))
  (setq minibuf-isearch-history nil)
  (setq minibuf-isearch-original-exit-func
	(lookup-key (current-local-map) "\C-m"))
  (minibuf-isearch-disable-icomplete-mode)
  (minibuf-isearch-save-initial-content)
  (setq minibuf-isearch-input-string "")
  (minibuf-isearch-mode 1)
  (minibuf-isearch-erase-minibuffer)
  (minibuf-isearch-display-indicator)
  (minibuf-isearch-goto-minibuf-point-max)
  (minibuf-isearch-do-search))

(defun minibuf-isearch-self-insert-command ()
  "Non-control character inputs are handled by this function."
  (interactive)
  (setq minibuf-isearch-input-string
	(concat minibuf-isearch-input-string
		(if (featurep 'xemacs)
		    (char-to-string last-input-char)
		  (this-command-keys))))
  (unless (input-pending-p)
    (minibuf-isearch-do-search)))

(defun minibuf-isearch-count-multi ()
  (let ((win (get-buffer-window minibuf-isearch-completion-buff))
	(count 1))
    (when win
      (setq count (max (/ (window-height win) 2) 2)))
    count))

(defun minibuf-isearch-prev-multi ()
  "Search backward multi count the word you've typed."
  (interactive)
  (minibuf-isearch-do-search (minibuf-isearch-count-multi)))

(defun minibuf-isearch-next-multi ()
  "Search forward multi count the word you've typed."
  (interactive)
  (minibuf-isearch-do-search (minibuf-isearch-count-multi) 'next))

(defun minibuf-isearch-prev ()
  "Search backward the word you've typed."
  (interactive)
  (minibuf-isearch-do-search 1))

(defun minibuf-isearch-next ()
  "Search forward the word you've typed."
  (interactive)
  (minibuf-isearch-do-search 1 'next))

(defun minibuf-isearch-fullback ()
  (interactive)
  (setq minibuf-isearch-input-string "")
  (setq minibuf-isearch-history-position
	minibuf-isearch-history-position-original)
  (unless (input-pending-p)
    (minibuf-isearch-do-search nil nil 'back)))

(defun minibuf-isearch-backspace ()
  "Delete the last type and search again."
  (interactive)
  (if (>= 0 (length minibuf-isearch-input-string))
      (when minibuf-isearch-display-message-always
	(minibuf-isearch-message
	 (format minibuf-isearch-match-format-string "")))
    (setq minibuf-isearch-input-string
	  (substring minibuf-isearch-input-string
		     0 (1- (length minibuf-isearch-input-string))))
    (setq minibuf-isearch-history-position
	  minibuf-isearch-history-position-original)
    ;; rewind
    (unless (input-pending-p)
      (minibuf-isearch-do-search nil nil 'back))))

(defun minibuf-isearch-exit-and-exit ()
  "Exit minibuf-isearch mode, and exit minibuffer too."
  (interactive)
  (let ((orgfunc minibuf-isearch-original-exit-func))
    (minibuf-isearch-exit)
    (when (and orgfunc (commandp orgfunc))
      (call-interactively orgfunc))))

(defun minibuf-isearch-exit ()
  "Exit minibuf-isearch mode."
  (interactive)
  (minibuf-isearch-mode -1)
  (setq minibuf-isearch-comp-winheight nil)
  (setq minibuf-isearch-do-search-regexp "")
  (setq minibuf-isearch-with-completion nil)
  (setq minibuf-isearch-history nil)
  (setq minibuf-isearch-original-exit-func nil)
  (setq minibuf-isearch-regexp-alist nil)
  (minibuf-isearch-dehighlight)
  (minibuf-isearch-erase-indicator)
  (minibuf-isearch-restore-winconf)
  (minibuf-isearch-restore-icomplete-mode)
  (minibuf-isearch-goto-minibuf-point-max))

(defun minibuf-isearch-abort ()
  "Abort minibuf-isearch mode.
The initial content of the minibuffer is restored."
  (interactive)
  (minibuf-isearch-exit)
  (setq minibuf-isearch-history-position 0)
  (minibuf-isearch-restore-initial-content))

;;; functions
(defun minibuf-isearch-restore-winconf (&optional bury-buffer)
  "Restore window configuration."
  (when (window-configuration-p minibuf-isearch-wincfg)
    (set-window-configuration minibuf-isearch-wincfg))
  (setq minibuf-isearch-wincfg nil)
  (when (get-buffer minibuf-isearch-completion-buff)
    (if bury-buffer
	(bury-buffer minibuf-isearch-completion-buff)
      (kill-buffer minibuf-isearch-completion-buff))))

(defun minibuf-isearch-stringify (obj)
  "Stringify given OBJ."
  (cond ((null obj) "")
	((stringp obj)
	 (set-text-properties 0 (length obj) nil obj)
	 obj)
	(t (prin1-to-string obj))))

(defun minibuf-isearch-get-minibuf-history ()
  "Return minibuffer history as a list."
  (cond
   (minibuf-isearch-history minibuf-isearch-history)
   ((not minibuf-isearch-with-completion)
    (setq minibuf-isearch-history-position 0)
    (setq minibuf-isearch-history-position-original 0)
    (setq minibuf-isearch-history (symbol-value minibuffer-history-variable)))
   (t
    (let ((hist (symbol-value minibuffer-history-variable))
	  (comp (sort (condition-case nil
			  (all-completions ""
					   minibuffer-completion-table
					   minibuffer-completion-predicate
					   'nospace)
			(error
			 (all-completions ""
					  minibuffer-completion-table
					  minibuffer-completion-predicate)))
		      'string<)))
      (when (and minibuf-isearch-treat-filecache
		 (memq minibuffer-completion-table
		       minibuf-isearch-filecache-completion-tables))
	(unless minibuf-isearch-filecache-list
	  (setq minibuf-isearch-filecache-list
		(when (and minibuf-isearch-treat-filecache
			   (boundp 'file-cache-alist) file-cache-alist)
		  (let ((alist file-cache-alist)
			(home (concat "^" (regexp-quote (expand-file-name "~/"))))
			tmp file dirs ret)
		    (while alist
		      (setq tmp (car (car alist)))
		      (setq dirs (cdr (car alist)))
		      (while dirs
			(setq file (concat (file-name-as-directory (car dirs))
					   tmp))
			(when (string-match home file)
			  (setq file (replace-match "~/" nil nil file)))
			(setq ret (cons file ret))
			(setq dirs (cdr dirs)))
		      (setq alist (cdr alist)))
		    (sort ret 'string<)))))
	(if comp
	    (nconc comp minibuf-isearch-filecache-list)
	  (setq comp minibuf-isearch-filecache-list)))
      (cond
       ((and comp hist)
	(setq minibuf-isearch-history-position (1+ (length comp))))
       (comp
	(setq minibuf-isearch-history-position (length comp)))
       (t
	(setq minibuf-isearch-history-position 0)))
      ;; 			  	    ; don't use nreverse
      (setq minibuf-isearch-history-position-original
	    minibuf-isearch-history-position)
      (setq minibuf-isearch-history (append (reverse comp) hist))))))

(defun minibuf-isearch-goto-minibuf-point-min ()
  "Move point to the top of the minibuffer (after the prompt)."
  (goto-char (minibuf-isearch-minibuf-point-min)))

(defun minibuf-isearch-minibuf-point-min ()
  "Return point of the top of the minibuffer (except the prompt)."
  (if (fboundp 'field-beginning) (field-beginning) (point-min)))

(defun minibuf-isearch-goto-minibuf-point-max ()
  "Move point to the end of the minibuffer."
  (goto-char (minibuf-isearch-minibuf-point-max)))

(defun minibuf-isearch-minibuf-point-max ()
  "Return point of the end of the minibuffer."
  (if (fboundp 'field-end) (field-end) (point-max)))

(defun minibuf-isearch-indicator-enabled-p ()
  "Return if minibuf-isearch indicator is enabled."
  (and (stringp minibuf-isearch-indicator-string)
       (< 0 (length minibuf-isearch-indicator-string))))

(defun minibuf-isearch-display-indicator ()
  "Display the minibuf-isearch indicator."
  (if (minibuf-isearch-indicator-enabled-p)
      (save-excursion
	(minibuf-isearch-goto-minibuf-point-min)
	(insert minibuf-isearch-indicator-string))))

(defun minibuf-isearch-erase-indicator ()
  "Erase the minibuf-isearch indicator."
  (if (minibuf-isearch-indicator-enabled-p)
      (save-excursion
	(minibuf-isearch-goto-minibuf-point-min)
	(if (search-forward minibuf-isearch-indicator-string
			    (+ 1 (point)
			       (length minibuf-isearch-indicator-string))
			    t)
	    (replace-match "")))))

(defun minibuf-isearch-disable-icomplete-mode ()
  "Save and clear {pre,post}-command-hook to cope with icomplete."
  (setq minibuf-isearch-original-prepost-command-hook
	(cons pre-command-hook post-command-hook))
  (setq pre-command-hook nil)
  (if (featurep 'xemacs)
      (setq post-command-hook 'minibuf-isearch-command-check-xmas)
    (setq post-command-hook nil)))

(defun minibuf-isearch-command-check-xmas ()
  "Emulate [t] in keymap on Xemacs."
  (unless (string-match "^minibuf-isearch-" (symbol-name this-command))
    (minibuf-isearch-exit)))

(defun minibuf-isearch-restore-icomplete-mode ()
  "Restore the initial {pre,post}-command-hook to cope with icomplete."
  (setq pre-command-hook (car minibuf-isearch-original-prepost-command-hook))
  (setq post-command-hook (cdr minibuf-isearch-original-prepost-command-hook)))

(defun minibuf-isearch-save-initial-content ()
  "Save initial content of the minibuffer."
  (setq minibuf-isearch-minibuf-initial-content
	(buffer-substring (minibuf-isearch-minibuf-point-min)
			  (minibuf-isearch-minibuf-point-max))))

(defun minibuf-isearch-restore-initial-content ()
  "Restore the initial content of the minibuffer."
  (minibuf-isearch-erase-minibuffer)
  (insert minibuf-isearch-minibuf-initial-content))

(defun minibuf-isearch-erase-minibuffer ()
  "Clear the minibuffer (editable field only)."
  (message nil)
  (if (fboundp 'field-beginning)
      (delete-field)
    (erase-buffer)))

(defun minibuf-isearch-highlight (beg end)
  "Add highlight from BEG to END."
  (when search-highlight
    (or minibuf-isearch-overlay
	(setq minibuf-isearch-overlay (make-overlay beg end)))
    (move-overlay minibuf-isearch-overlay beg end (current-buffer))
    (overlay-put minibuf-isearch-overlay 'face 'minibuf-isearch-face)
    (overlay-put minibuf-isearch-overlay 'evaporate t)))

(defun minibuf-isearch-dehighlight ()
  "Delete highlight of minibuf-isearch."
  (when minibuf-isearch-overlay
    (delete-overlay minibuf-isearch-overlay)))

(defun minibuf-isearch-make-regexp (str)
  "Make a regular expression from STR."
  (if (assoc str minibuf-isearch-regexp-alist)
      (cdr (assoc str minibuf-isearch-regexp-alist))
    (let ((original str)
	  limit pattern)
      (cond
       ((string-match "^^" str)
	(setq limit 'pre)
	(setq str (substring str 1)))
       ((string-match "\\$$" str)
	(setq limit 'post)
	(setq str (substring str 0 -1))))
      (setq pattern (if (and minibuf-isearch-use-migemo
			     (fboundp 'migemo-get-pattern))
			(migemo-get-pattern str)
		      (regexp-quote str)))
      (prog1
	  (setq pattern
		(cond
		 ((eq limit 'pre) (concat "^\\(" pattern "\\)"))
		 ((eq limit 'post) (concat "\\(" pattern "\\)$"))
		 (t pattern)))
	(setq minibuf-isearch-regexp-alist
	      (cons (cons original pattern) minibuf-isearch-regexp-alist))))))

(defun minibuf-isearch-show-completion (&optional nomsg back)
  "Make and show the completion window.
If NOMSG is non-nil, not show the message."
  (interactive)
  (unless (get-buffer-window minibuf-isearch-completion-buff)
    (setq minibuf-isearch-wincfg (current-window-configuration)))
  (let* ((regexp minibuf-isearch-do-search-regexp)
	 (history (minibuf-isearch-get-minibuf-history))
	 (len (length history))
	 (name (symbol-name minibuffer-history-variable))
	 (pos (max (- minibuf-isearch-history-position 1) 0))
	 (buff (get-buffer-create minibuf-isearch-completion-buff))
	 (case-fold-search minibuf-isearch-ignore-case)
	 (pop-up-windows t)
	 (pop-up-frames nil)
	 (i 0)
	 down match matches height)
    (if (string= regexp "")
	(setq minibuf-isearch-last-regexp nil)
      (setq minibuf-isearch-last-regexp regexp))
    (while (and history
		(setq match (minibuf-isearch-stringify (car history))))
      (when (or (null minibuf-isearch-last-regexp)
		(string-match regexp match))
	(when (string= match "") (setq match " "))
	(while (string-match "[\n\r]" match)
	  (setq match (replace-match " || " nil nil match)))
	(cond
	 ((= i pos) (setq down 0))
	 (down (setq down (1+ down)))
	 (t nil))
	(setq matches (cons match matches)))
      (setq i (1+ i))
      (setq history (cdr history)))
    (setq minibuff-isearch-mode-name (format "minibuff-isearch: %s (" name))
    (setq minibuff-isearch-mode-info (format "/%d/%d)" (length matches) len))
    (setq minibuff-isearch-mode-matchenum (length matches))
    (add-text-properties 0 (length minibuff-isearch-mode-name)
			 '(face bold) minibuff-isearch-mode-name)
    (add-text-properties 0 (length minibuff-isearch-mode-info)
			 '(face bold) minibuff-isearch-mode-info)
    (save-selected-window
      (pop-to-buffer buff)
      (setq buffer-read-only nil)
      (erase-buffer)
      (unless minibuf-isearch-comp-winheight
       (setq minibuf-isearch-comp-winheight (window-height)))
      ;; (insert (format "%s\n\n" name))
      (setq i 0)
      (while matches
	(insert (car matches))
	(setq i (1+ i))
	(when (zerop down)
	  (setq minibuff-isearch-mode-line (number-to-string i))
	  (beginning-of-line)
	  (add-text-properties (point) (line-end-position)
			       '(face
				 minibuf-isearch-comp-face
				 minibuf-isearch
				 t))
	  (when (and minibuf-isearch-last-regexp
		     (re-search-forward regexp (line-end-position) t))
	    (add-text-properties (match-beginning 0) (match-end 0)
				 '(face minibuf-isearch-face)))
	  (end-of-line))
	(insert "\n")
	(setq down (1- down))
	(setq matches (cdr matches)))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (when minibuf-isearch-shrink-completion-window
       (when (and back
                  minibuf-isearch-comp-winheight
                  (> (setq height (- minibuf-isearch-comp-winheight
                                     (window-height))) 0))
         (enlarge-window height))
       (shrink-window-if-larger-than-buffer)))
    (minibuf-isearch-update-completion))
  (unless (or nomsg (not minibuf-isearch-display-message-always))
    (minibuf-isearch-message minibuf-isearch-last-message)))

(defun minibuf-isearch-check-completion (&optional back)
  "Check and redraw the completion window."
  (if (or (and (assq minibuffer-history-variable
		     minibuf-isearch-show-completion-steps)
	       (not (cdr (assq minibuffer-history-variable
			       minibuf-isearch-show-completion-steps))))
	  (< (length minibuf-isearch-input-string)
	     (or (cdr (assq minibuffer-history-variable
			    minibuf-isearch-show-completion-steps))
		 (cdr (assq t minibuf-isearch-show-completion-steps))
		 0)))
      (when (get-buffer-window minibuf-isearch-completion-buff)
	(minibuf-isearch-restore-winconf 'bury-buffer))
    (minibuf-isearch-show-completion 'nomsg back)))

(defun minibuf-isearch-update-completion (&optional move next)
  "Update the completion window.
MOVE and NEXT mean to move the selcted point only."
  (let ((buf (get-buffer minibuf-isearch-completion-buff))
	(win (get-buffer-window minibuf-isearch-completion-buff))
	(line (string-to-number minibuff-isearch-mode-line))
	pos)
    (when win
      (save-selected-window
	(set-buffer buf)
	(if (setq pos (or (and (get-text-property (point-min) 'minibuf-isearch)
			       (point-min))
			  (next-single-property-change (point-min) 'minibuf-isearch)))
	    (let ((case-fold-search minibuf-isearch-ignore-case)
		  buffer-read-only)
	      (goto-char pos)
	      (when move
		(set-text-properties (point) (line-end-position) nil)
		(forward-line (- next))
		(setq line (min (max (- line next) 1) minibuff-isearch-mode-matchenum))
		(when (eobp) (forward-line -1))
		(add-text-properties (point) (line-end-position)
				     '(face
				       minibuf-isearch-comp-face
				       minibuf-isearch
				       t))
		(when (and minibuf-isearch-last-regexp
			   (re-search-forward minibuf-isearch-last-regexp
					      (line-end-position) t))
		  (add-text-properties (match-beginning 0) (match-end 0)
				       '(face minibuf-isearch-face)))
		(set-buffer-modified-p nil)))
	  (goto-char (point-max)))
	(setq minibuff-isearch-mode-line (number-to-string line))
	(unless (pos-visible-in-window-p (point) win)
	  (set-window-start win
			    (progn (forward-line
				    (- (* (if (>= (window-start win) (point))
					      1 2)
					  (/ (window-height win) 3))))
				   (point)))))
      (add-text-properties 0 (length minibuff-isearch-mode-line)
			   '(face bold) minibuff-isearch-mode-line)
      (setq mode-line-buffer-identification
	    `(,minibuff-isearch-mode-name
	      ,minibuff-isearch-mode-line
	      ,minibuff-isearch-mode-info))
      (set-buffer (window-buffer (minibuffer-window))))))

(defun minibuf-isearch-do-search (&optional skip-count next back)
  "Do search.
Skips entry if SKIP-COUNT is non-nil.
Searches forward when NEXT is non-nil."
  (let* ((enable-recursive-minibuffers t) ; FIXME: ???
	 (regexp (minibuf-isearch-make-regexp minibuf-isearch-input-string))
	 (history (minibuf-isearch-get-minibuf-history))
	 (pos (max (1- minibuf-isearch-history-position) 0))
	 (case-fold-search minibuf-isearch-ignore-case)
	 (count skip-count)
	 found pre-pos)
    (setq minibuf-isearch-do-search-regexp regexp)
    (save-match-data
      (cond
       (next
	(if skip-count
	    (setq pos (1- pos)))
	(if (< pos 0)
	    (minibuf-isearch-message
	     (format minibuf-isearch-no-match-format-string
		     minibuf-isearch-input-string))
	  ;; search
	  (setq found (catch 'loop
			(while (>= pos 0)
			  (when (and (string-match regexp
						   (minibuf-isearch-stringify
						    (nth pos history)))
				     (setq pre-pos pos)
				     (or (not count)
					 (and count
					      (= (setq count (1- count)) 0))))
			    (throw 'loop t))
			  (setq pos (1- pos)))))
	  (when (and (not found) pre-pos)
	    (setq found t)
	    (setq pos pre-pos))
	  (unless (or found skip-count)
	    (setq pos (max (1- minibuf-isearch-history-position) 0))
	    (while (and (< pos (length history))
			(not (setq found
				   (string-match regexp
						 (minibuf-isearch-stringify
						  (nth pos history))))))
	      (setq pos (1+ pos))))))
       (t
	(if skip-count
	    (setq pos (1+ pos)))
	;; search
	(setq found (catch 'loop
		      (while (< pos (length history))
			(when (and (string-match regexp
						 (minibuf-isearch-stringify
						  (nth pos history)))
				   (setq pre-pos pos)
				   (or (not count)
				       (and count
					    (= (setq count (1- count)) 0))))
			  (throw 'loop t))
			(setq pos (1+ pos)))))
	(when (and (not found) pre-pos)
	  (setq found t)
	  (setq pos pre-pos))
	(unless (or found skip-count)
	  (setq pos (max (1- minibuf-isearch-history-position) 0))
	  (while (and (>= pos 0)
		      (not (setq found
				 (string-match regexp
					       (minibuf-isearch-stringify
						(nth pos history))))))
	    (setq pos (1- pos))))))
      (minibuf-isearch-ifdebug
       (if found
	   (message (concat (format "pos:%d " pos)
			    (minibuf-isearch-stringify (nth pos history))
			    " " (minibuf-isearch-stringify history)))))
      (if (not found)
	  (minibuf-isearch-message
	   (format minibuf-isearch-no-match-format-string
		   minibuf-isearch-input-string))
	(put minibuffer-history-variable 'cursor-pos regexp)
	(unwind-protect
	    (progn
	      (minibuf-isearch-goto-history (1+ pos))
	      (if skip-count
		  (minibuf-isearch-update-completion
		   'move (if next (- skip-count) skip-count))
		(minibuf-isearch-check-completion back))
	      (if minibuf-isearch-display-message-always
		  (minibuf-isearch-message
		   (format minibuf-isearch-match-format-string
			   minibuf-isearch-input-string))))
	  (put minibuffer-history-variable 'cursor-pos nil))))))

(defun minibuf-isearch-message (msg)
  "Display MSG on the right side of the minibuffer."
  (setq minibuf-isearch-last-message msg)
  (and minibuf-isearch-message-use-redraw
       minibuf-isearch-display-message-always
       (sit-for 0))
  (let ((max (point-max))
	(ovls (overlays-in (point-min) (point-max)))
	(bastr "")
	ovl spc)
    (save-excursion
      (if (or (null minibuf-isearch-message-on-right)
	      (featurep 'xemacs))
	  (goto-char (point-max))
	(beginning-of-line)
	(while (setq ovl (car ovls))
	  (when (overlay-get ovl 'before-string)
	    (setq bastr (concat (overlay-get (car ovls) 'before-string)
				bastr)))
	  (when (overlay-get ovl 'after-string)
	    (setq bastr (concat (overlay-get (car ovls) 'after-string)
				bastr)))
	  (setq ovls (cdr ovls)))
	(setq spc (- (window-width)
		     (minibuffer-prompt-width)
		     (string-width msg)
		     (string-width bastr)
		     (string-width (buffer-substring-no-properties (point) max))
		     1))
	(goto-char (point-max)))
      (if (and spc (> spc 0))
	  (insert (make-string spc ?\ ) msg)
	(insert " " msg)))
    (let ((inhibit-quit t))
      (if minibuf-isearch-display-message-always
	  (sit-for 10)
	(sit-for 1.5))
      (delete-region max (point-max))
      (when quit-flag
	(if (featurep 'xemacs)
	    (let ((event (character-to-event 7)))
	      (setq quit-flag nil)
	      (message "")
	      (push unread-command-events event))
	  (setq quit-flag nil)
	  (minibuf-isearch-abort))))))

(defun minibuf-isearch-goto-history (n)
  "Update `minibuf-isearch-history-position' by N and display history entry."
  (if (< n 0)
      nil
    (setq minibuf-isearch-history-position n)
    ;; clear minibuf and insert history element
    (minibuf-isearch-erase-indicator)
    (minibuf-isearch-erase-minibuffer)
    (minibuf-isearch-display-indicator)
    (minibuf-isearch-goto-minibuf-point-max)
    (save-excursion
      (insert (minibuf-isearch-stringify
	       (nth (1- n) (minibuf-isearch-get-minibuf-history)))))
    (let ((pos (get minibuffer-history-variable 'cursor-pos))
	  (case-fold-search minibuf-isearch-ignore-case))
      (if (and (stringp pos)
	       ;; move point
	       (re-search-forward pos nil t))
	  ;; add highlight overlay
	  (minibuf-isearch-highlight (match-beginning 0)
				     (match-end 0))))))

(defun minibuf-isearch-setup ()
  "Setup minibuf-isearch."
  (interactive)
  (setq minibuf-isearch-filecache-list nil)
  (let ((map (make-sparse-keymap)))
    (let ((key 32))
      (while (<= key 126)
	(define-key map (char-to-string key)
	  'minibuf-isearch-self-insert-command)
	(setq key (1+ key))))
    (define-key map "\C-j" 'minibuf-isearch-exit)
    (define-key map "\C-m" 'minibuf-isearch-exit)
    (define-key map "\C-g" 'minibuf-isearch-abort)
    (define-key map "\C-d" 'minibuf-isearch-exit)
    (define-key map "\C-r" 'minibuf-isearch-prev)
    (define-key map "\C-p" 'minibuf-isearch-prev)
    (define-key map "\C-s" 'minibuf-isearch-next)
    (define-key map "\C-n" 'minibuf-isearch-next)
    (define-key map "\M-r" 'minibuf-isearch-prev-multi)
    (define-key map "\M-p" 'minibuf-isearch-prev-multi)
    (define-key map "\M-s" 'minibuf-isearch-next-multi)
    (define-key map "\M-n" 'minibuf-isearch-next-multi)
    (define-key map "\C-h" 'minibuf-isearch-backspace)
    (define-key map "\C-?" 'minibuf-isearch-backspace)
    (define-key map "\M-h" 'minibuf-isearch-fullback)
    (define-key map "\C-i" 'minibuf-isearch-show-completion)
    (define-key map [tab]  'minibuf-isearch-show-completion)
    (mapcar (lambda (key)
	      (define-key map key 'minibuf-isearch-exit-and-exit))
	    minibuf-isearch-exit-and-exit-keys)
    (unless (featurep 'xemacs)
      (define-key map [t]  'minibuf-isearch-exit))
    (setq minibuf-isearch-mode-map map))
  (unless (interactive-p)
    (easy-mmode-define-minor-mode
     minibuf-isearch-mode			; mode var
     "Incremental search on minibuffer history.
In this mode, you can type:

Non-control characters to incremental-search. The matched part is
highlighted.
C-r to search backward the word you typed.
C-s to search forward.
C-h, DEL, BS to delete the last type and search again.
C-g to abort isearching.  The initial content of the minibuffer
is restored.
C-m, C-j, RET, etc. to exit this minor mode."	; docstr
     nil					; initial value
     " MinibufIsearch"				; mode line indicator
     minibuf-isearch-mode-map)))		; keybindings

;;;###autoload
(mapcar (lambda (keymap)
	  (mapcar (lambda (key)
		    (define-key keymap key 'minibuf-isearch-backward))
		  minibuf-isearch-fire-keys)
	  (mapcar (lambda (key)
		    (define-key keymap key 'minibuf-isearch-backward-reverse))
		  minibuf-isearch-reverse-fire-keys))
	(delq nil (list (and (boundp 'minibuffer-local-map)
			     minibuffer-local-map)
			(and (boundp 'minibuffer-local-ns-map)
			     minibuffer-local-ns-map)
			(and (boundp 'minibuffer-local-completion-map)
			     minibuffer-local-completion-map)
			(and (boundp 'minibuffer-local-must-match-map)
			     minibuffer-local-must-match-map))))

(minibuf-isearch-setup)

(provide 'minibuf-isearch)

;;; minibuf-isearch.el ends here
