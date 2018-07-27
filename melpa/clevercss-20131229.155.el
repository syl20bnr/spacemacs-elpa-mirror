;;; clevercss.el --- A major mode for editing CleverCSS files

;; Copyright (C) 2010 Joe Schafer
;; Author: Joe Schafer (joesmoe10@gmail.com)
;; Created: Apr 2010
;; Keywords: languages css
;; Package-Version: 20131229.155

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(defconst clevercss-version "0.1"
  "`clevercss-mode' version number.")

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is a major mode for editing CleverCSS files.  Much of the
;; functionality imitates that of `clevercss-mode'.  `clevercss-mode'
;; provides support for imenu and hideshow.

;;; Installation:

;; - Put `clevercss.el' somewhere in your emacs load path
;; - Add these lines to your .emacs file:
;;   (autoload 'clevercss-mode "clevercss nil t)
;;   (add-to-list auto-mode-alist '("\\.pcss\\'" . clevercss-mode))
;;  
;; This mode assumes that CleverCSS files have the suffix ".pcss".
;; You may use additional suffixes by adding them to
;; `auto-mode-alist'.  For example, to add the suffix ".ccss" you
;; would write the following in your .emacs file:
;; 
;; (add-to-list auto-mode-alist '("\\.ccss\\'" . clevercss-mode))
;;
;; To customize how it works:
;;  M-x customize-group RET clevercss-mode RET
;;

;;; Bug Reporting:

;; Patches welcome.

;;; History:

;; Apr 2010 - Created

;;; Todo:
;; Fix font-lock for opening block css selectors.  It's a hack.
;; Add imenu support (wishlist)
;; Add compilation support

;;; Code:

(defgroup clevercss nil
  "Major mode for editing CleverCSS files in Emacs."
  :group 'languages
  :prefix "clevercss-")


;;;; Font Lock

(defvar clevercss-font-lock-keywords
  `(
    ;; Keywords
    (,(rx symbol-start (group "@define") symbol-end)
     (1 font-lock-keyword-face))

    ;; Builtin functions
    (,(rx symbol-start (group (or
                               "string" "bare" "length" "upper" "lower"
                               "strip" "split" "eval" "round" "abs" "brighten"
                               "darken" "length" "join" "list" "seq")) ?\()
     (1 font-lock-builtin-face))

    ;; Comments
    (,(rx (group "//" (0+ not-newline)))
     (1 font-lock-comment-face))

    ;; Macro calls
    (,(rx symbol-start (group (: "%" (1+ (or ?_ alnum)))))
     (1 font-lock-function-name-face))

    ;; Macro definition
    (,(rx symbol-start "@define" (1+ space) (group (1+ (or ?_ alnum))))
     (1 font-lock-function-name-face))

    ;; Variable assignments (only allowed at top level)
    (,(rx line-start (group (1+ (or ?_ alnum))) (1+ space) (? "?") "=")
     (1 font-lock-variable-name-face))

    ;; Variable references
    (,(rx symbol-start (group ?$ (1+ (or ?_ alnum))))
     (1 font-lock-variable-name-face))

    ;; Opening blocks (e.g. body, div: ...)
    (,(rx (group (+? (or alnum ?_ space (any "&#.>,*+\"~=|[]\""))))
          ":" (0+ space) (? "//" (0+ not-newline)) line-end)
     (1 font-lock-type-face))

    ;; Unnamed numerical constants
    (,(rx not-wordchar (group (? "-") (1+ (or ?. num))
                              (? (or "px" "em" ?% "pt" "ex" "in"
                                     "cm" "pc" "mm"))
                              word-end))
     (1 font-lock-constant-face))

    ;; include and end directives
    (,(rx line-start (group  (or "__END__" "@include")))
     (1 font-lock-preprocessor-face))

    ))

(defvar clevercss-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\177" 'clevercss-backspace)
    (define-key map "\C-c<" 'clevercss-shift-left)
    (define-key map "\C-c>" 'clevercss-shift-right)
    (define-key map "\C-c\C-k" 'clevercss-mark-block)
    (define-key map "\C-c\C-d" 'clevercss-pdbtrack-toggle-stack-tracking)
    (define-key map "\C-c\C-n" 'clevercss-next-statement)
    (define-key map "\C-c\C-p" 'clevercss-previous-statement)
    (define-key map "\C-c\C-u" 'clevercss-beginning-of-block)
    
    (easy-menu-define clevercss-menu map "CleverCSS Mode menu"
      `("CleverCSS"
	:help "CleverCSS-specific Features"
	["Shift region left" clevercss-shift-left :active mark-active
	 :help "Shift by a single indentation step"]
	["Shift region right" clevercss-shift-right :active mark-active
	 :help "Shift by a single indentation step"]
	"-"
	["Mark block" clevercss-mark-block
	 :help "Mark innermost block around point"]
	["Mark def/class" mark-defun
	 :help "Mark innermost definition around point"]
	"-"
	["Start of block" clevercss-beginning-of-block
	 :help "Go to start of innermost definition around point"]
	["End of block" clevercss-end-of-block
	 :help "Go to end of innermost definition around point"]
	["Start of def/class" beginning-of-defun
	 :help "Go to start of innermost definition around point"]
	["End of def/class" end-of-defun
	 :help "Go to end of innermost definition around point"]))
    map))

(defvar clevercss-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Give punctuation syntax to ASCII that normally has symbol
    ;; syntax or has word syntax and isn't a letter.
    (let ((symbol (string-to-syntax "_"))
	  (sst (standard-syntax-table)))
      (dotimes (i 128)
	(unless (= i ?_)
	  (if (equal symbol (aref sst i))
	      (modify-syntax-entry i "." table)))))
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?% "_" table)
    (modify-syntax-entry ?@ "_" table)
    (modify-syntax-entry ?_ "_" table)

    ;; exceptions
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?\n "> b" table)
    table))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.pcss\\'" . clevercss-mode))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.ccss\\'" . clevercss-mode))



;;;; Utility stuff

(defsubst clevercss-in-string/comment ()
  "Return non-nil if point is in a CleverCSS literal (a comment or string)."
  ;; We don't need to save the match data.
  (nth 8 (syntax-ppss)))

(defun clevercss-skip-comments/blanks (&optional backward)
  "Skip comments and blank lines.
BACKWARD non-nil means go backwards, otherwise go forwards.
Backslash is treated as whitespace so that continued blank lines
are skipped.  Doesn't move out of comments -- should be outside
or at end of line."
  (let ((arg (if backward
		 ;; If we're in a comment (including on the trailing
		 ;; newline), forward-comment doesn't move backwards out
		 ;; of it.  Don't set the syntax table round this bit!
		 (let ((syntax (syntax-ppss)))
		   (if (nth 4 syntax)
		       (goto-char (nth 8 syntax)))
		   (- (point-max)))
	       (point-max))))
    (forward-comment arg)))

(defun clevercss-comment-line-p ()
  "Return non-nil if and only if current line has only a comment."
  (save-excursion
    (end-of-line)
    (when (eq 'comment (syntax-ppss-context (syntax-ppss)))
      (back-to-indentation)
      (looking-at (rx (or (syntax comment-start) line-end))))))

(defun clevercss-blank-line-p ()
  "Return non-nil if and only if current line is blank."
  (save-excursion
    (beginning-of-line)
    (looking-at "\\s-*$")))

(defun clevercss-open-block-statement-p (&optional bos)
  "Return non-nil if statement at point opens a block.
BOS non-nil means point is known to be at beginning of statement."
  (save-excursion
    (unless bos (clevercss-beginning-of-statement))
    (end-of-line)
    (clevercss-skip-comments/blanks t)
    (looking-back (rx (or "->" ":")))))


;;;; Indentation

(defcustom clevercss-indent 4
  "*Amout of offset per level of indentation."
  :type 'integer
  :group 'clevercss)
(put 'clevercss-indent 'safe-local-variable 'integerp)

(defun clevercss-guess-indent ()
  "Guess step for indentation of current buffer.
Set `clevercss-indent' locally to the value guessed."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (done indent)
	(while (and (not done) (not (eobp)))
	  (when (and (re-search-forward (rx ?: (0+ space)
					    (or (syntax comment-start)
						line-end))
					nil 'move)
		     (clevercss-open-block-statement-p))
	    (save-excursion
	      (clevercss-beginning-of-statement)
	      (let ((initial (current-indentation)))
		(if (zerop (clevercss-next-statement))
		    (setq indent (- (current-indentation) initial)))
		(if (and indent (>= indent 2) (<= indent 8)) ; sanity check
		    (setq done t))))))
	(when done
	  (when (/= indent (default-value 'clevercss-indent))
	    (set (make-local-variable 'clevercss-indent) indent)
	    (unless (= tab-width clevercss-indent)
	      (setq indent-tabs-mode nil)))
	  indent)))))

(defcustom clevercss-honour-comment-indentation nil
  "Non-nil means indent relative to preceding comment line.
Only do this for comments where the leading comment character is
followed by space.  This doesn't apply to comment lines, which
are always indented in lines with preceding comments."
  :type 'boolean
  :group 'clevercss)

(defcustom clevercss-guess-indent t
  "Non-nil means CleverCSS mode guesses `clevercss-indent' for the buffer."
  :type 'boolean
  :group 'clevercss)

;; Alist of possible indentations and start of statement they would
;; close.  Used in indentation cycling (below).
(defvar clevercss-indent-list nil
  "Internal use.")
;; Length of the above
(defvar clevercss-indent-list-length nil
  "Internal use.")
;; Current index into the alist.
(defvar clevercss-indent-index nil
  "Internal use.")


;;;; Movement

(defun clevercss-beginning-of-statement ()
  "Go to start of current statement.
Accounts for continuation lines, multi-line strings, and
multi-line bracketed expressions."
  (back-to-indentation))

(defun clevercss-skip-out (&optional forward syntax)
  "Skip out of any nested brackets.
Skip forward if FORWARD is non-nil, else backward.
If SYNTAX is non-nil it is the state returned by `syntax-ppss' at point.
Return non-nil if and only if skipping was done."
  (let ((depth (syntax-ppss-depth (or syntax (syntax-ppss))))
	(forward (if forward -1 1)))
    (unless (zerop depth)
      (if (> depth 0)
	  ;; Skip forward out of nested brackets.
	  (condition-case ()		; beware invalid syntax
	      (progn (backward-up-list (* forward depth)) t)
	    (error nil))
	;; Invalid syntax (too many closed brackets).
	;; Skip out of as many as possible.
	(let (done)
	  (while (condition-case ()
		     (progn (backward-up-list forward)
			    (setq done t))
		   (error nil)))
	  done)))))

(defun clevercss-end-of-statement ()
  "Go to the end of the current statement and return point.
Usually this is the start of the next line, but if this is a
multi-line statement we need to skip over the continuation lines.
On a comment line, go to end of line."
  (end-of-line)
  (while (let (comment)
	   ;; Move past any enclosing strings and sexps, or stop if
	   ;; we're in a comment.
	   (while (let ((s (syntax-ppss)))
		    (cond ((eq 'comment (syntax-ppss-context s))
			   (setq comment t)
			   nil)
			  ((eq 'string (syntax-ppss-context s))
			   ;; Go to start of string and skip it.
                           (let ((pos (point)))
                             (goto-char (nth 8 s))
                             (condition-case () ; beware invalid syntax
                                 (progn (forward-sexp) t)
                               ;; If there's a mismatched string, make sure
                               ;; we still overall move *forward*.
                               (error (goto-char pos) (end-of-line)))))
			  ((clevercss-skip-out t s))))
	     (end-of-line))
	   (unless comment
	     (eq ?\\ (char-before))))	; Line continued?
    (end-of-line 2))			; Try next line.
  (point))

(defun clevercss-previous-statement (&optional count)
  "Go to start of previous statement.
With argument COUNT, do it COUNT times.  Stop at beginning of buffer.
nReturn count of statements left to move."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0)
      (clevercss-next-statement (- count))
    (clevercss-beginning-of-statement)
    (while (and (> count 0) (not (bobp)))
      (clevercss-skip-comments/blanks t)
      (clevercss-beginning-of-statement)
      (unless (bobp) (setq count (1- count))))
    count))

(defun clevercss-next-statement (&optional count)
  "Go to start of next statement.
With argument COUNT, do it COUNT times.  Stop at end of buffer.
Return count of statements left to move."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0)
      (clevercss-previous-statement (- count))
    (beginning-of-line)
    (let (bogus)
      (while (and (> count 0) (not (eobp)) (not bogus))
	(clevercss-end-of-statement)
	(clevercss-skip-comments/blanks)
	(if (eq 'string (syntax-ppss-context (syntax-ppss)))
	    (setq bogus t)
	  (unless (eobp)
	    (setq count (1- count))))))
    count))

(defun clevercss-beginning-of-block (&optional arg)
  "Go to start of current block.
With numeric arg, do it that many times.  If ARG is negative, call
`clevercss-end-of-block' instead.
If point is on the first line of a block, use its outer block.
If current statement is in column zero, don't move and return nil.
Otherwise return non-nil."
  (interactive "p")
  (unless arg (setq arg 1))
  (cond
   ((zerop arg))
   ((< arg 0) (clevercss-end-of-block (- arg)))
   (t
    (let ((point (point)))
      (if (or (clevercss-comment-line-p)
	      (clevercss-blank-line-p))
	  (clevercss-skip-comments/blanks t))
      (clevercss-beginning-of-statement)
      (let ((ci (current-indentation)))
	(if (zerop ci)
	    (not (goto-char point))	; return nil
	  ;; Look upwards for less indented statement.
	  (if (catch 'done
		(while (and (zerop (forward-line -1)))
		  (when (and (< (current-indentation) ci)
			     (not (clevercss-comment-line-p))
			     ;; Move to beginning to save effort in case
			     ;; this is in string.
			     (progn (clevercss-beginning-of-statement) t)
			     (clevercss-open-block-statement-p t))
		    (beginning-of-line)
		    (throw 'done t)))
		(not (goto-char point))) ; Failed -- return nil
	      (clevercss-beginning-of-block (1- arg)))))))))

(defun clevercss-end-of-block (&optional arg)
  "Go to end of current block.
With numeric arg, do it that many times.  If ARG is negative,
call `clevercss-beginning-of-block' instead.
If current statement is in column zero and doesn't open a block,
don't move and return nil.  Otherwise return t."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (clevercss-beginning-of-block (- arg))
    (while (and (> arg 0)
		(let* ((point (point))
		       (_ (if (clevercss-comment-line-p)
			      (clevercss-skip-comments/blanks t)))
		       (ci (current-indentation))
		       (open (clevercss-open-block-statement-p)))
		  (if (and (zerop ci) (not open))
		      (not (goto-char point))
		    (catch 'done
		      (while (zerop (clevercss-next-statement))
			(when (or (and open (<= (current-indentation) ci))
				  (< (current-indentation) ci))
			  (clevercss-skip-comments/blanks t)
			  (beginning-of-line 2)
			  (throw 'done t)))))))
      (setq arg (1- arg)))
    (zerop arg)))

(defun clevercss-first-word ()
  "Return first word (actually symbol) on the line."
  (save-excursion
    (back-to-indentation)
    (current-word t)))

(defun clevercss-initial-text ()
  "Text of line following indentation and ignoring any trailing comment."
  (save-excursion
    (buffer-substring (progn
			(back-to-indentation)
			(point))
		      (progn
			(end-of-line)
			(forward-comment -1)
			(point)))))

(defun clevercss-block-end-p ()
  "Non-nil if this statement or indented blank line closes a block."
  (and (not (clevercss-comment-line-p))
       (< (current-indentation)
          (save-excursion
            (clevercss-previous-statement)
            (current-indentation)))))

(defun clevercss-mark-block ()
  "Mark the block around point.
Uses `clevercss-beginning-of-block', `clevercss-end-of-block'."
  (interactive)
  (push-mark)
  (clevercss-beginning-of-block)
  (push-mark (point) nil t)
  (clevercss-end-of-block)
  (exchange-point-and-mark))

(defun clevercss-shift-left (start end &optional count)
  "Shift lines in region COUNT (the prefix arg) columns to the left.
If region isn't active, just shift current line.  The region
shifted includes the lines in which START and END lie.  It is an
error if any lines in the region are indented less than COUNT
columns."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count clevercss-indent))
  (when (> count 0)
    (save-excursion
      (goto-char start)
      (while (< (point) end)
	(if (and (< (current-indentation) count)
		 (not (looking-at "[ \t]*$")))
	    (error "Can't shift all lines enough"))
	(forward-line))
      (indent-rigidly start end (- count)))))

(add-to-list 'debug-ignored-errors "^Can't shift all lines enough")

(defun clevercss-shift-right (start end &optional count)
  "Shift lines in region COUNT (the prefix arg) columns to the right.
COUNT defaults to `clevercss-indent'.  If region isn't active, just shift
current line.  The region shifted includes the lines in which START and
END lie."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count clevercss-indent))
  (indent-rigidly start end count))

(defun clevercss-indentation-levels ()
  "Return a list of possible indentations for this line.
It is assumed not to be a continuation line or in a multi-line string.
Includes the default indentation and those which would close all
enclosing blocks.  Elements of the list are actually pairs:
\(INDENTATION . TEXT), where TEXT is the initial text of the
corresponding block opening (or nil)."
  (save-excursion
    (let ((initial "")
	  levels indent)
      ;; Only one possibility immediately following a block open
      ;; statement.
      (cond
       ((save-excursion (and (clevercss-previous-statement)
			     (clevercss-open-block-statement-p t)
                             (setq indent (+ clevercss-indent
                                             (current-indentation)))))
	(push (cons indent initial) levels))
       ;; Only one possibility for comment line immediately following
       ;; another.
       ((save-excursion
	  (when (clevercss-comment-line-p)
	    (forward-line -1)
	    (if (clevercss-comment-line-p)
		(push (cons (current-indentation) initial) levels)))))
       ;; Fixme: Maybe have a case here which indents (only) first
       ;; line after a lambda.
       (t
	(progn
	  (clevercss-previous-statement)
          (push (cons (current-indentation) (clevercss-initial-text))
                levels)
	  (while (clevercss-beginning-of-block)
            (push (cons (current-indentation) (clevercss-initial-text))
                  levels)))))
      (prog1 (or levels (setq levels '((0 . ""))))
	(setq clevercss-indent-list levels
	      clevercss-indent-list-length (length clevercss-indent-list))))))

;; This is basically what `clevercss-indent-line' would be if we didn't
;; do the cycling.
(defun clevercss-indent-line-1 (&optional leave)
  "Subroutine of `clevercss-indent-line'.
Does non-repeated indentation.  LEAVE non-nil means leave
indentation if it is valid, i.e. one of the positions returned by
`clevercss-calculate-indentation'."
  (let ((target (clevercss-calculate-indentation))
	(pos (- (point-max) (point))))
    (if (or (= target (current-indentation))
	    ;; Maybe keep a valid indentation.
	    (and leave clevercss-indent-list
		 (assq (current-indentation) clevercss-indent-list)))
	(if (< (current-column) (current-indentation))
	    (back-to-indentation))
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to target)
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))))

(defun clevercss-indent-line ()
  "Indent current line as CleverCSS code.
When invoked via `indent-for-tab-command', cycle through possible
indentations for current line.  The cycle is broken by a command
different from `indent-for-tab-command', i.e. successive TABs do
the cycling."
  (interactive)
  (if (and (eq this-command 'indent-for-tab-command)
	   (eq last-command this-command))
      (if (= 1 clevercss-indent-list-length)
	  (message "Sole indentation")
	(progn (setq clevercss-indent-index
		     (% (1+ clevercss-indent-index) clevercss-indent-list-length))
	       (beginning-of-line)
	       (delete-horizontal-space)
	       (indent-to (car (nth clevercss-indent-index clevercss-indent-list)))
	       (if (clevercss-block-end-p)
		   (let ((text (cdr (nth clevercss-indent-index
					 clevercss-indent-list))))
		     (if text
			 (message "Closes: %s" text))))))
    (clevercss-indent-line-1)
    (setq clevercss-indent-index (1- clevercss-indent-list-length))))

(defun clevercss-calculate-indentation ()
  "Calculate CleverCSS indentation for line at point."
  (setq clevercss-indent-list nil
	clevercss-indent-list-length 1)
  (save-excursion
    (beginning-of-line)
    (let ((syntax (syntax-ppss))
	  start)
      (cond
       ((bobp) 0)
       ;; Fixme: Like clevercss-mode.el; not convinced by this.
       ((looking-at (rx (0+ space) (syntax comment-start)
			(not (any " \t\n")))) ; non-indentable comment
	(current-indentation))
       ((and clevercss-honour-comment-indentation
	     ;; Back over whitespace, newlines, non-indentable comments.
	     (catch 'done
	       (while (cond ((bobp) nil)
			    ((not (forward-comment -1))
			     nil)	; not at comment start
			    ;; Now at start of comment -- trailing one?
			    ((/= (current-column) (current-indentation))
			     nil)
			    ;; Indentable comment, like clevercss-mode.el?
			    ((and (looking-at (rx (syntax comment-start)
						  (or space line-end)))
				  (/= 0 (current-column)))
			     (throw 'done (current-column)))
			    ;; Else skip it (loop).
			    (t))))))
       (t
	(clevercss-indentation-levels)
	;; Prefer to indent comments with an immediately-following
	;; statement, e.g.
	;;       ...
	;;   # ...
	;;   body h1 ...
	(when (and (> clevercss-indent-list-length 1)
		   (clevercss-comment-line-p))
	  (forward-line)
	  (unless (clevercss-comment-line-p)
	    (let ((elt (assq (current-indentation) clevercss-indent-list)))
	      (setq clevercss-indent-list
		    (nconc (delete elt clevercss-indent-list)
			   (list elt))))))
	(caar (last clevercss-indent-list)))))))

;;;; `Electric' commands.

(defun clevercss-backspace (arg)
  "Maybe delete a level of indentation on the current line.
Do so if point is at the end of the line's indentation outside
strings and comments.
Otherwise just call `backward-delete-char-untabify'.
Repeat ARG times."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column))
	  (bolp)
	  (clevercss-in-string/comment))
      (backward-delete-char-untabify arg)
    ;; Look for the largest valid indentation which is smaller than
    ;; the current indentation.
    (let ((indent 0)
	  (ci (current-indentation))
	  (indents (clevercss-indentation-levels))
	  initial)
      (dolist (x indents)
	(if (< (car x) ci)
	    (setq indent (max indent (car x)))))
      (setq initial (cdr (assq indent indents)))
      (if (> (length initial) 0)
	  (message "Closes %s" initial))
      (delete-horizontal-space)
      (indent-to indent))))


;;;; Modes

;;;###autoload
(define-derived-mode clevercss-mode fundamental-mode "CleverCSS"
  "Major mode for editing CleverCSS programs.
Blank lines separate paragraphs, comments start with `// '.

The indentation width is controlled by `clevercss-indent', which
defaults to 4.  If `clevercss-guess-indent' is non-nil, then try to
match the indentation of the file.

Modules can hook in via `clevercss-mode-hook'.

Use `clevercss-version' to display the current version of this
file.

\\{clevercss-mode-map} "
  :group 'clevercss
  (set (make-local-variable 'font-lock-defaults)
       '(clevercss-font-lock-keywords))
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  
  (set (make-local-variable 'indent-line-function) #'clevercss-indent-line)
  (set (make-local-variable 'indent-region-function) #'clevercss-indent-region)
  (set (make-local-variable 'paragraph-start) "\\s-*$")
  (set (make-local-variable 'fill-paragraph-function) 'clevercss-fill-paragraph)
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)
  ;; FIXME
  (set (make-local-variable 'which-func-functions) '(clevercss-which-function))
  (set (make-local-variable 'add-log-current-defun-function)
       'clevercss-current-defun)

  (set (make-local-variable 'beginning-of-defun-function)
       'clevercss-beginning-of-block)
  (set (make-local-variable 'end-of-defun-function) 'clevercss-end-of-block)
  (add-hook 'which-func-functions 'clevercss-which-func nil t)
  (set (make-local-variable 'ispell-check-comments) 'exclusive)

  (unless font-lock-mode (font-lock-mode 1))
  (when clevercss-guess-indent (clevercss-guess-indent))
  )

(provide 'clevercss)

;;; clevercss.el ends here
