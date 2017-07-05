;;; jemdoc-mode.el --- Major mode for editing jemdoc files

;; Copyright (C) 2017 Dimitar Dimitrov

;; Author: Dimitar Dimitrov <mail.mitko@gmail.com>
;; URL: https://github.com/drdv/jemdoc-mode
;; Package-Version: 20170704.1327
;; Package-X-Original-Version: 20170609.1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: convenience, usability

;; This file is not part of GNU Emacs.

;;; License:

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; Package tested on:
;; GNU Emacs 25.1.1 (x86_64-apple-darwin16.1.0)
;;
;; jemdoc is a light text-based markup language designed
;; for creating websites.  For more information see
;; http://jemdoc.jaboc.net

;; Quick start:
;;
;; add (require 'jemdoc-mode) to your .emacs

;;; Code:



;; Terminology I use in the code:
;;
;; a "tilde block" is a block of the form
;; ~~~
;; ARGUMENTS
;; this is a tilde block
;; ~~~
;; for all possible ARGUMENTS, see http://jemdoc.jaboc.net
;;
;; I call a "code block" a tilde block with ARGUMENTS
;; {optional block name}{optional programming language name}



;; used to not fontify the content of a code-block
(require 'font-lock+ nil t)

(defgroup jemdoc-mode nil
  "Major mode for editing jemdoc files."
  :group 'languages)

(defgroup jemdoc-mode-faces nil
  "Jemdoc-mode related phases."
  :group 'jemdoc-mode
  :group 'faces)

(defface jemdoc-mode-face-monospace
  '((t . (:inherit font-lock-type-face)))
  "Face for +monospace+."
  :group 'jemdoc-mode-faces)

(defface jemdoc-mode-face-bold
  '((t . (:inherit 'bold)))
  "Face for *bold*."
  :group 'jemdoc-mode-faces)

(defface jemdoc-mode-face-italics
  '((t . (:inherit 'italic :foreground "dark slate blue")))
  "Face for /italics/.
When using emacs in macos Terminal, :slant doesn't have an effect,
that's why I set :foreground as well."
  :group 'jemdoc-mode-faces)

(defface jemdoc-mode-face-tilde-block-delimiters
  '((t . (:inherit 'success)))
  "Face for tilde block delimiters."
  :group 'jemdoc-mode-faces)

(defface jemdoc-mode-face-equation
  '((t . (:inherit font-lock-keyword-face)))
  "Face for $inline equation$ or \(equation\)."
  :group 'jemdoc-mode-faces)

(defface jemdoc-mode-face-special-keywords
  '((t . (:inherit font-lock-variable-name-face)))
  "Face for special keywords.
For example, keywords in comments or the definition construct : {}."
  :group 'jemdoc-mode-faces)

(defface jemdoc-mode-face-special-keywords-name
  '((t . (:inherit font-lock-function-name-face)))
  "Face for names in special keywords.
For example the definition construct : {A name}
or #include{name of file}."
  :group 'jemdoc-mode-faces)

(defface jemdoc-mode-face-monospace-html
  '((t . (:inherit font-lock-function-name-face :weight bold)))
  "Face for +{{monospace html}}+ which is equivalent to %monospace html%."
  :group 'jemdoc-mode-faces)

(defface jemdoc-mode-face-html-text
  '((t . (:inherit font-lock-function-name-face)))
  "Face for {{html text}}."
  :group 'jemdoc-mode-faces)

(defface jemdoc-mode-face-other
  '((t . (:inherit font-lock-constant-face)))
  "Face for \\n, \\A, \\C, \\R, \\M, `singly quoted text', etc."
  :group 'jemdoc-mode-faces)

(defface jemdoc-mode-face-http-mail
  '((t . (:inherit font-lock-constant-face :weight bold)))
  "Face for [http/mail block]."
  :group 'jemdoc-mode-faces)

(defface jemdoc-mode-face-dashes-2
  '((t . (:inherit font-lock-variable-name-face :weight bold)))
  "Face for --."
  :group 'jemdoc-mode-faces)

(defface jemdoc-mode-face-dashes-3
  '((t . (:inherit font-lock-function-name-face :weight bold)))
  "Face for ---."
  :group 'jemdoc-mode-faces)

(defface jemdoc-mode-face-ellipsis
  '((t . (:inherit font-lock-function-name-face :weight bold)))
  "Face for ellipsis (...)."
  :group 'jemdoc-mode-faces)

(defface jemdoc-mode-face-bullet
  '((t .(:inherit bold :foreground "red")))
  "Face for bullets."
  :group 'jemdoc-mode-faces)

(defface jemdoc-mode-face-bullet-warning
  '((t . (:inherit font-lock-warning-face :background "dark slate blue")))
  "Warning face for bullets."
  :group 'jemdoc-mode-faces)

(defface jemdoc-mode-face-title-1
  '((t . (:inherit bold :foreground "color-18")))
  "Face for title with one \"=\"."
  :group 'jemdoc-mode-faces)

(defface jemdoc-mode-face-title-2
  '((t . (:inherit bold :foreground "color-21")))
  "Face for title with two \"==\"."
  :group 'jemdoc-mode-faces)

(defface jemdoc-mode-face-title-3
  '((t . (:inherit bold :foreground "color-27")))
  "Face for title with three \"===\"."
  :group 'jemdoc-mode-faces)

(defface jemdoc-mode-face-title-4
  '((t . (:inherit bold :foreground "color-33")))
  "Face for title with four \"====\"."
  :group 'jemdoc-mode-faces)



(defvar-local jemdoc-mode-tilde-block-delimiter-last-value nil
  "Holds the last assigned value of the text property `tilde-block-delimiter'.")

(defvar-local jemdoc-mode-debug-messages nil
  "Set to non-nil to output debug messages.")

(defvar-local jemdoc-mode-warning-messages t
  "Set to non-nil to output warning messages.")

(defvar jemdoc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x n r") 'jemdoc-mode-edit-code-block)
    map)
  "Keymap for jemdoc major mode.")

(defvar-local jemdoc-mode-region-extended-already nil
  "`jemdoc-mode-extend-region' should change the region only once per iteration.

After each font-lock iteration, it is set back to nil in
`jemdoc-mode-extend-region-initialize', which is registerd in
`font-lock-extend-after-change-region-function'.")

(defvar jemdoc-mode-font-lock-syntax-table
  (let ((st (make-syntax-table)))
    ;; I use the "b" below because otherwise the single-line comments
    ;; interfere with other comments that I might want to define.
    (modify-syntax-entry ?#  "< b" st)
    (modify-syntax-entry ?\n ">#b" st)
    st)
  "Syntax table for `jemdoc-mode'.")

(defvar-local jemdoc-mode-font-lock-support-mode nil
  "Specify the support mode for jemdoc.")

;; To suppress warnings during byte-compilation.
(defvar font-lock-beg)
(defvar font-lock-end)



(defun jemdoc-mode-syntax-propertize-function (start end)
  "Assign text properties from START to END.

Text properties:
  jemdoc-keywords-in-comments-property: delimits keywords in comments
  font-lock-ignore: used by font-lock+ to ignore region (and not fontify it)."
  (let ((case-fold-search nil))
    (goto-char start)

    ;;(message "syntax: %d - %d" start end)

    (remove-text-properties start end '(jemdoc-keywords-in-comments-property))
    (remove-text-properties start end '(font-lock-ignore t))

    (funcall
     (syntax-propertize-rules
      ;; keywords in comments
      ((regexp-opt '("jemdoc" "menu" "nofooter" "nodate" "notime"
		     "fwtitle" "showsource" "nodefaultcss" "addcss"
		     "addjs" "addpackage" "addtex" "analytics" "title"
		     "noeqs" "noeqcache" "eqsize" "eqdir")
		   'words)
       (0 (ignore (jemdoc-mode-keywords-in-comments-property-assign))))
      ;; handle tilde blocks
      ("^~~~ *$"
       (0 (ignore (jemdoc-mode-tilde-block-text-properties)))))
     start end))
  (when (and jemdoc-mode-warning-messages
	     (eq jemdoc-mode-tilde-block-delimiter-last-value 'start))
    (message "Warning: wrong delimiters of tilde blocks.")))

(defun jemdoc-mode-tilde-block-text-properties ()
  "Assign text properties to tilde-blocks.

1. `tilde-block-delimiter' -> `start'
   first character of ~~~ at the beginning of tilde-blocks
2. `tilde-block-delimiter' -> `end'
   first character of ~~~ at the end of tilde-blocks
3. `font-lock-ignore' -> t
   content of code-blocks."
  (let (previous-label
	single-property-change)
    ;; first, record the 'tilde-block-delimiter text property of
    ;; the previous "^~~~ *$" (don't move point)
    (save-excursion
      (beginning-of-line)
      (when (re-search-backward "^~~~ *$" nil t)
	(setq previous-label
	      (get-text-property (point) 'tilde-block-delimiter))))
    ;; second, assign a 'tilde-block-delimiter text property to
    ;; the current "^~~~ *$"
    (save-excursion
      (beginning-of-line)
      (cond
       ;; when previous-label is 'start
       ((eq previous-label 'start)
	(put-text-property (point) (1+ (point)) 'tilde-block-delimiter 'end)
	(setq jemdoc-mode-tilde-block-delimiter-last-value 'end))
       ;; when previous-label is either 'end or nil (the first delimiter)
       (t
	(put-text-property (point) (1+ (point)) 'tilde-block-delimiter 'start)
	(setq jemdoc-mode-tilde-block-delimiter-last-value 'start)))

      ;; here I have to go back to the beginning of the containing tilde-block
      (setq single-property-change
	    (previous-single-property-change (point) 'tilde-block-delimiter))
      (when single-property-change
	(goto-char single-property-change)
	(goto-char (line-beginning-position 2))
	;; code-block
	(when (looking-at "^ *\\({[^}{]*} *\\)\\{2,2\\}$")
	  (let ((start (line-beginning-position 2))
		(end (progn
		       (re-search-forward "^~~~ *$" nil t)
		       (line-end-position 0))))
	    (put-text-property start end  'font-lock-ignore t)
	    (remove-text-properties start end '(face nil))
	    ))))))

(defun jemdoc-mode-keywords-in-comments-property-assign ()
  "Assign text properties in keywords in comments."
  (let* ((beg (match-beginning 0))
	 (str-line (thing-at-point 'line t))
	 (jemdoc-mode-line-start-p
	  (if (> (length str-line) 9)
	      (equal (substring str-line 0 9) "# jemdoc:")
	    nil))
	 (context (save-excursion
		    (save-match-data (syntax-ppss beg)))))
    (when jemdoc-mode-line-start-p
      (put-text-property beg (1+ beg)
			 'jemdoc-keywords-in-comments-property
			 (cons (nth 4 context) (match-data))))))

(defun jemdoc-mode-keywords-in-comments-property-retrieve (limit)
  "Highlight text with jemdoc-keywords-in-comments-property (until LIMIT)."
  (let ((pos
	 (next-single-char-property-change (point)
					   'jemdoc-keywords-in-comments-property
					   nil limit)))
    (when (and pos (> pos (point)))
      (goto-char pos)
      (let ((value
	     (get-text-property pos 'jemdoc-keywords-in-comments-property)))
        (if (eq (car value) t)
            (progn
              (set-match-data (cdr value))
              t)
          (jemdoc-mode-keywords-in-comments-property-retrieve limit))))))



(defun jemdoc-mode-highlight-curly-brackets-tilde-block (limit)
  "Highlight curly brackets in the preamble of a tilde block (until LIMIT)."
  ;; search for {}x1 or {}x2 or {}x7
  (when (re-search-forward
	 "^ *\\(\\({[^}{]*} *\\)\\{1,2\\}\\|\\({[^}{]*} *\\)\\{7,7\\}\\)$"
	 limit t)
    (save-excursion
      (save-match-data
	(goto-char (line-beginning-position 0))
	;; If the match is not preceded by a "^~~~ *$" I don't want to return
	;; nil because the search for this keyword would terminate (and there
	;; might be more matches below). Instead, I return true but with empty
	;; match data.
	(unless (looking-at "^~~~ *$")
	  (set-match-data nil))))
    t))

(defun jemdoc-mode-highlight-monospace-html-text (limit)
  "Highlight monospace html text (until LIMIT).

There are two formats: +{{format 1}}+ and %format 2%."
  (re-search-forward "\\(\\+{{.*?}}\\+\\|%.*?%\\)" limit t))

(defun jemdoc-mode-highlight-curly-brackets-html-text (limit)
  "Highlight curly brackets with html text (until LIMIT)."
  (re-search-forward "{{.*?}}" limit t))



(defun jemdoc-mode-extend-tilde-region ()
  "Extend region to contain encolsing tilde block.

This function is called in `jemdoc-mode-extend-region' which is registered
in `font-lock-extend-region-functions' and is called by font-lock during
fontification.  The variables `font-lock-beg' and `font-lock-end' in the code
refer to dynamically bound variables used by font-lock."
  (let (region-beg
	region-end)
    (save-excursion
      (goto-char font-lock-beg)
      (setq region-beg (jemdoc-mode-in-tilde-block-internal 'general-block)))
    (save-excursion
      (goto-char font-lock-end)
      (setq region-end (jemdoc-mode-in-tilde-block-internal 'general-block)))
    (when (and region-beg
	       (< (car region-beg) font-lock-beg)) ;; don't shorten the region
      (setq font-lock-beg (car region-beg)))
    (when (and region-end
	       (> (cdr region-end) font-lock-end)) ;; don't shorten the region
      (setq font-lock-end (cdr region-end)))))

(defun jemdoc-mode-extend-bullet-region ()
  "Extend region to contain encolsing bullet block.

This function is called in `jemdoc-mode-extend-region' which is registered
in `font-lock-extend-region-functions' and is called by font-lock during
fontification.  The variables `font-lock-beg' and `font-lock-end' in the code
refer to dynamically bound variables used by font-lock."
  (save-excursion
    (when jemdoc-mode-debug-messages
      (message "----------------------------------------------------")
      (message "[initial]: font-lock-beg = %d, font-lock-end = %d, point = %d"
	       font-lock-beg font-lock-end (point)))
    (save-excursion
      (goto-char (line-beginning-position 2))
      (when jemdoc-mode-debug-messages
	(message "[move down]: point = %d" (point)))
      ;; search for
      ;; ^ *- +   (bullet with one dash)
      ;; ^ *\\. + (bullet with one dot)
      ;; ^ *$     (empty line)
      ;; \\'      (end of buffer)
      ;; ^~~~ *$  (beginning or end of a tilde block)
      (if (re-search-forward
	   "\\(^ *- +\\|^ *\\. +\\|^ *$\\|\\'\\|^~~~ *$\\)"
	   nil t)
	  (progn
	    (when jemdoc-mode-debug-messages
	      (message "[after first search]: point = %d" (point)))
	    ;; Here I don't include the last line, because if it is the
	    ;; beginning of a tilde block, later on it would be included
	    ;; as well because the end of the region would be inside it
	    ;; (which is not what I want). On the other hand if the last
	    ;; line is the end of a tilde region it is included in the region
	    ;; in `jemdoc-mode-extend-tilde-region'.
	    (goto-char (line-end-position 0))
	    (when (> (point) font-lock-end) ;; don't shorten the region
	      (setq font-lock-end (point))))
	(setq font-lock-end (point-max))))
    ;; Here I go up because when point is at the beginning of an empty line
    ;; (re-search-backward "^ *$") matches it and doesn't move point, which
    ;; I think is quite a bit counter-intuitive.
    (when (looking-at "^ *$")
      (goto-char (line-beginning-position 0)))
    ;; the only difference with the regex above is that here I have
    ;; \\` (i.e., beginning of buffer) instead of \\' (i.e., end of buffer)
    (if (re-search-backward
	 "\\(^ *- +\\|^ *\\. +\\|^ *$\\|\\`\\|^~~~ *$\\)"
	 nil t)
	(progn
	  (when jemdoc-mode-debug-messages
	    (message "[after second search]: point = %d" (point)))
	  ;; don't shorten the region
	  (when (< (match-beginning 0) font-lock-beg)
	    (setq font-lock-beg (match-beginning 0))))
      (setq font-lock-beg (point-min))))
  (when jemdoc-mode-debug-messages
    (message "[extend]: font-lock-beg = %d, font-lock-end = %d, point = %d"
	     font-lock-beg font-lock-end (point)))
  nil)

(defun jemdoc-mode-extend-region-initialize (beg end &optional len)
  "Reset `jemdoc-mode-region-extended-already'.

BEG, END and LEN are the standard input provided to `after-change-functions'."
  (setq jemdoc-mode-region-extended-already nil)
  nil)

(defun jemdoc-mode-extend-region()
  "Extend the font-lock region.

registered in `font-lock-extend-region-functions'."
  (unless jemdoc-mode-region-extended-already
    (setq jemdoc-mode-region-extended-already t)
    (jemdoc-mode-extend-bullet-region)
    (jemdoc-mode-extend-tilde-region))
  (when jemdoc-mode-debug-messages
    (message "[while]: font-lock-beg = %d, font-lock-end = %d"
	     font-lock-beg font-lock-end))
  ;; this function is executed first among the functions in
  ;; `font-lock-extend-region-functions' and there is no problem
  ;; to always return true even if we haven't changed the region
  ;; because this wouldn't cause the start of other iterations
  ;; in the while loop of `font-lock-default-fontify-region'
  t)



(defun jemdoc-mode-concat-string (str n)
  "Concatenate a string STR, N times."
  (let (out-str)
    (dotimes (k n)
      (setq out-str (concat out-str str)))
    out-str))

(defun jemdoc-mode-end-of-block-delimiter (str n)
  "Return regexp describing an end of block delimiter.
Delimiters can be: empty line, end of buffer, or line starting with STR
appearing N or less times in a row."
  ;; an empty line ("^$"),
  ;; end of buffer ("\\'") or
  ;; a line starting with 1, ..., n str
  (let ((S "\\(\\'\\|^ *$"))
    (dotimes (k n)
      (setq S (concat
	       S
	       (format "\\|^ *%s +" (jemdoc-mode-concat-string str (- n k))))))
    (setq S (concat S "\\)"))))

(defun jemdoc-mode-end-of-block (str n)
  "Return position of next end of block delimiter.
Delimiters can be: empty line, end of buffer, or line starting with STR
appearing N or less times in a row.  See `jemdoc-mode-end-of-block-delimiter'."
  (save-excursion
    (re-search-forward (jemdoc-mode-end-of-block-delimiter str n) nil t)))



(defun jemdoc-mode-in-tilde-block-internal (&optional tilde-block-type)
  "Check whether point is inside a tilde block.

If point is inside a tilde block with type TILDE-BLOCK-TYPE,
return a cell array with its beginning and end.  If not, return nil.

TILDE-BLOCK-TYPE can be 'code-block, 'general-block."
  (or tilde-block-type (setq tilde-block-type 'general-block))

  (save-excursion
    (let ((code-block-regexp "^ *\\({[^}{]*} *\\)\\{2,2\\}$")
	  beg
	  end)

      (beginning-of-line)
      (cond
       ;; On a closing "^~~~ *$"
       ((eq (get-text-property (point) 'tilde-block-delimiter) 'end)
	(setq end (line-end-position 1))
	(re-search-backward "^~~~ *$" nil t)
	(setq beg (point)))
       ;; On an opening "^~~~ *$"
       ((eq (get-text-property (point) 'tilde-block-delimiter) 'start)
	(setq beg (point))
	(end-of-line)
	(re-search-forward "^~~~ *$" nil t)
	(setq end (point)))
       ;; Not on a "^~~~ *$" line
       (t
	(when (and (re-search-backward "^~~~ *$" nil t)
		   (eq (get-text-property (point) 'tilde-block-delimiter)
		       'start))
	  (setq beg (point))
	  (end-of-line)
	  ;; to to closing "^~~~ *$"
	  (re-search-forward "^~~~ *$" nil t)
	  (setq end (point)))))

      ;; when we are in a gelera tilde-block
      (when beg
	(cond
	 ;; when checking for a general-block
	 ((eq tilde-block-type 'general-block) `(,beg . ,end))
	 ;; when checking for a code-block
	 ((eq tilde-block-type 'code-block)
	  (goto-char beg)
	  (goto-char (line-beginning-position 2))
	  (when (looking-at code-block-regexp)
	    `(,beg . ,end))))))))



;; Below I tried to narrow to a block and then change the major mode but
;; this doesn't work because the parts not within the narrowed region
;; interfere with the fontification. For example, if we narrow only to
;; lines 2,3,4,5 the code in the narrowed region would be fontified as
;; if it is in a comment
;;
;; 1. /*
;; 2. int f(int a)
;; 3. {
;; 4.     cout << "test" << "\n";
;; 5. }
;; 6. */
;;
;; I don't know why this happens. If I check (syntax-ppss POINT) in the
;; narrowed region, the parser state shows that we are not in a comment!
;; I tried setting `font-lock-dont-widen' to non-nil - no effect.

;; Maybe I should do something as in
;; https://github.com/aaronbieber/fence-edit.el where the block is simply
;; copied in a new buffer, the appropriate major mode is set and after
;; editing, the code can be inserted back in the original buffer.
;;
;; todo: maybe I can use directly mmm-mode ...

;; I leave this code for reference (who knows, maybe someone would find a
;; cool trick to make it work)
(defun jemdoc-mode-narrow-to-code-block ()
  "Narrow to containing code block and change the major mode.
The major mode is determined from the content of the second {}
in the code-block arguments."
  (interactive)
  (let ((region (jemdoc-mode-in-tilde-block-internal 'code-block)))
    (if region
	(let ((start (save-excursion
		       (goto-char (car region))
		       ;; skip the opening ~~~\n{}{}
		       (line-beginning-position 3)))
	      (end (save-excursion
		     (goto-char (cdr region))
		     ;; skip the closing ~~~
		     (line-end-position 0)))
	      ;; detect the programming language (specified in the second {})
	      (lang (save-excursion
		      ;; first, go to the end of the code-block
		      ;; useful when point is in the arguments of a code-block
		      (re-search-forward "^ *~~~ *$")
		      ;; here we are sure that we are in a code block
		      (re-search-backward "^ *~~~ *\n *{.*?} *{\\(.*?\\)}")
		      (substring-no-properties (match-string 1)))))
	  (narrow-to-region start end)
	  (message "lang = %s" lang)
	  (cond
	   ((equal lang "lisp")
	    (emacs-lisp-mode)
	    (remove-text-properties start end '(font-lock-ignore t)))
	   ((or (equal lang "python")
		(equal lang "py"))
	    (python-mode)
	    (remove-text-properties start end '(font-lock-ignore t)))
	   ((or (equal lang "c++")
		(equal lang "cpp"))
	    (c++-mode)
	    (remove-text-properties start end '(font-lock-ignore t)))))
      (message "warning: not in code block"))))



;; Here I am adopting/stealing the approach from
;; github.com/aaronbieber/fence-edit.el
;; see as well the discussion in
;; www.reddit.com/r/emacs/comments/42yi77/any_solution_for_editing_a_region_in_a_different

(defvar-local jemdoc-mode-lang-mode-alist
  '(("lisp"   . "emacs-lisp-mode")
    ("elisp"  . "emacs-lisp-mode")
    ("c++"    . "c++-mode")
    ("cpp"    . "c++-mode")
    ("c"      . "c-mode")
    ("python" . "python-mode")
    ("py"     . "python-mode")
    ("rb"     . "ruby-mode")
    ("ruby"   . "ruby-mode")
    ("sh"     . "sh-mode")
    ("octave" . "octave-mode")
    ("matlab" . "octave-mode"))
  "Association between programming language specifier and major mode.")

(defvar jemdoc-mode-code-block-beg nil
  "Mark (in jemdoc buffer) for beginning of region to edit.
This variable is set in the jemdoc-buffer but is accessed from
the edit-buffer (so I don't use `defvar-local').")

(defvar jemdoc-mode-code-block-end nil
  "Mark (in jemdoc buffer) for end of region to edit.
This variable is set in the jemdoc-buffer but is accessed from
the edit-buffer (so I don't use `defvar-local').")

(defconst jemdoc-mode-edit-code-block-buffer-name "*edit-code-block*"
  "Name of the buffer where we edit code-blocks.")

(defconst jemdoc-mode-edit-insert-button-name "← [insert]\n"
  "Name of the abort button in the preamble when editing code-blocks.")

(defconst jemdoc-mode-edit-abort-button-name "× [abort]\n"
  "Name of the abort button in the preamble when editing code-blocks.")

(defconst jemdoc-mode-edit-preamble-termination-indicator "\^L\n"
  "Length of preamble when editing code-blocks.
This is the number of characters used for the menu before the code.")

(define-button-type 'jemdoc-mode-insert-button
  'action 'jemdoc-mode-edit-code-block-insert-back
  'follow-link t)

(define-button-type 'jemdoc-mode-abort-button
  'action 'jemdoc-mode-edit-code-block-abort
  'follow-link t)

(defun jemdoc-mode-edit-code-block ()
  "Edit a code-block in new buffer (appropriate major mode is activated).
The major mode is determined by the content of the second {} in the
arguments of the code-block."
  (interactive)
  (let ((region (jemdoc-mode-in-tilde-block-internal 'code-block)))
    (if region
	(let* ((m-beg (set-marker (make-marker)
				  (save-excursion
				    (goto-char (car region))
				    ;; skip the opening ~~~\n{...}{...}
				    (line-beginning-position 3))))
	       (m-end (set-marker (make-marker)
				  (save-excursion
				    (goto-char (cdr region))
				    ;; skip the closing ~~~
				    (line-end-position 0))))
	       ;; the abort button is placed right after the insert button
	       (abort-button-location
		(1+ (length jemdoc-mode-edit-insert-button-name)))
	       (preamble-length
		(+ (length jemdoc-mode-edit-insert-button-name)
		   (length jemdoc-mode-edit-abort-button-name)
		   (length jemdoc-mode-edit-preamble-termination-indicator)))
	       (lang (save-excursion
		       ;; first, go to the end of the code-block
		       ;; useful when point is in the arguments of a code-block
		       (re-search-forward "^ *~~~ *$")
		       ;; here we are sure that we are in a code block
		       (re-search-backward "^ *~~~ *\n *{.*?} *{\\(.*?\\)}")
		       (substring-no-properties (match-string 1))))
	       (code (buffer-substring-no-properties m-beg m-end))
	       (mode (cdr (assoc lang jemdoc-mode-lang-mode-alist)))
	       ;;(ovl (make-overlay m-beg m-end))
	       (edit-buffer
		(progn
		  ;; make sure that there is only one code-block editor buffer
		  (when (get-buffer jemdoc-mode-edit-code-block-buffer-name)
		    (kill-buffer jemdoc-mode-edit-code-block-buffer-name))
		  (generate-new-buffer
		   jemdoc-mode-edit-code-block-buffer-name))))
	  (setq jemdoc-mode-code-block-beg m-beg
		jemdoc-mode-code-block-end m-end)
	  ;; (switch-to-buffer-other-window edit-buffer t)
	  (switch-to-buffer edit-buffer t)
	  ;;(overlay-put ovl 'face 'secondary-selection)
	  (insert jemdoc-mode-edit-insert-button-name)
	  (insert jemdoc-mode-edit-abort-button-name)
	  (insert jemdoc-mode-edit-preamble-termination-indicator)
	  (put-text-property (point-min)
			     preamble-length 'read-only t)
	  (make-button 1
		       (1- abort-button-location)
		       :type 'jemdoc-mode-insert-button)
	  (make-button abort-button-location
		       (- preamble-length 2)
		       :type 'jemdoc-mode-abort-button)
	  (insert code)
	  (condition-case e
	      (funcall (intern mode))
	    (error
	     (message "warning: major mode `%s' fails with: %s" mode e)
	     (fundamental-mode)))
	  ;; `header-line-format' automatically becomes buffer-local when set
	  (setq header-line-format
		(concat "jemdoc-code-block-editor [" mode "]: "
			(format "region (%d,%d) in %s"
				(marker-position jemdoc-mode-code-block-beg)
				(marker-position jemdoc-mode-code-block-end)
				(marker-buffer jemdoc-mode-code-block-beg))))
	  (goto-char abort-button-location))
      (message "warning: not in code-block"))))

(defun jemdoc-mode-edit-code-block-insert-back (button)
  "Insert edited code-block back in the jemdoc buffer.
BUTTON is the standard input given to functions registerd in the
`action' property of `define-button-type'."
  (let* ((buffer (current-buffer))
	 (preamble-length
	  (+ (length jemdoc-mode-edit-insert-button-name)
	     (length jemdoc-mode-edit-abort-button-name)
	     (length jemdoc-mode-edit-preamble-termination-indicator)))
	 (code (buffer-substring-no-properties
	       (1+ preamble-length)
	       (point-max))))

    (switch-to-buffer (marker-buffer jemdoc-mode-code-block-beg))
    (kill-buffer buffer)
    (delete-region jemdoc-mode-code-block-beg
		   jemdoc-mode-code-block-end)
    (insert code)
    (goto-char jemdoc-mode-code-block-beg)
    (set-marker jemdoc-mode-code-block-beg nil)
    (set-marker jemdoc-mode-code-block-end nil)))

(defun jemdoc-mode-edit-code-block-abort (button)
  "Abort code-block editing.
BUTTON is the standard input given to functions registerd in the
`action' property of `define-button-type'."
  (let ((buffer (current-buffer)))
    (switch-to-buffer (marker-buffer jemdoc-mode-code-block-beg))
    (kill-buffer buffer)
    (set-marker jemdoc-mode-code-block-beg nil)
    (set-marker jemdoc-mode-code-block-end nil)))



(defvar jemdoc-mode-font-lock-keywords
  (list
   ;; ---------------------------------------------------------

   '("^ *\\(-\\) +" (1 'jemdoc-mode-face-bullet)
     ("^ *\\(--\\) +"
      (jemdoc-mode-end-of-block "-" 1)
      nil
      (1 'jemdoc-mode-face-bullet t)))

   '("^ *\\(--\\) +" (1 'jemdoc-mode-face-bullet-warning)
     ("^ *\\(---\\) +"
      (jemdoc-mode-end-of-block "-" 2)
      nil
      (1 'jemdoc-mode-face-bullet t)))

   '("^ *\\(---\\) +" (1 'jemdoc-mode-face-bullet-warning)
     ("^ *\\(----\\) +"
      (jemdoc-mode-end-of-block "-" 3)
      nil
      (1 'jemdoc-mode-face-bullet t)))

   '("^ *\\(----\\) +" (1 'jemdoc-mode-face-bullet-warning)
     ("^ *\\(-----\\) +"
      (jemdoc-mode-end-of-block "-" 4)
      nil
      (1 'jemdoc-mode-face-bullet t)))

   ;; ---------------------------------------------------------

   '("^ *\\(\\.\\) +" (1 'jemdoc-mode-face-bullet)
     ("^ *\\(\\.\\.\\) +"
      (jemdoc-mode-end-of-block "\\." 1)
      nil
      (1 'jemdoc-mode-face-bullet t)))

   '("^ *\\(\\.\\.\\) +" (1 'jemdoc-mode-face-bullet-warning)
     ("^ *\\(\\.\\.\\.\\) +"
      (jemdoc-mode-end-of-block "\\." 2)
      nil
      (1 'jemdoc-mode-face-bullet t)))

   '("^ *\\(\\.\\.\\.\\) +" (1 'jemdoc-mode-face-bullet-warning)
     ("^ *\\(\\.\\.\\.\\.\\) +"
      (jemdoc-mode-end-of-block "\\." 3)
      nil
      (1 'jemdoc-mode-face-bullet t)))

   '("^ *\\(\\.\\.\\.\\.\\) +" (1 'jemdoc-mode-face-bullet-warning)
     ("^ *\\(\\.\\.\\.\\.\\.\\) +"
      (jemdoc-mode-end-of-block "\\." 4)
      nil
      (1 'jemdoc-mode-face-bullet t)))

   ;; ---------------------------------------------------------

   ;; handle `singly quoted text'
   '("\`.*?\'"  . 'jemdoc-mode-face-other)

   ;; titles
   '("^ *= +.*"    0 'jemdoc-mode-face-title-1 t)
   '("^ *== +.*"   0 'jemdoc-mode-face-title-2 t)
   '("^ *=== +.*"  0 'jemdoc-mode-face-title-3 t)
   '("^ *==== +.*" 0 'jemdoc-mode-face-title-4 t)

   ;; ---------------------------------------------------------

   ;; definition
   '("^ *\\(: *{\\)\\(.*?\\)\\(}\\)"
     (1 'jemdoc-mode-face-special-keywords)  ;; : {
     (2 'jemdoc-mode-face-special-keywords-name)  ;; definition name
     (3 'jemdoc-mode-face-special-keywords)) ;; }

   ;; ---------------------------------------------------------

   ;; monospace
   ;; (unfortunately I cannot use negative look-behind assertions in emacs)
   ;; the following (rx ...) generates "\\(?:^\\|[^\\]\\)\\(\\+.*?[^\\]\\+\\)"
   `(,(rx (or line-start
	      (not (any "\\")))
	  (group
	   "+"
	   (minimal-match (zero-or-more not-newline))
	   (not (any "\\"))
	   "+"))
     1 'jemdoc-mode-face-monospace prepend)

   ;; bold
   '("\\(?:^\\|[^\\]\\)\\(\\*.*?[^\\]\\*\\)"  1 'jemdoc-mode-face-bold prepend)

   ;; italics
   '("\\(?:^\\|[^\\]\\)\\(/.*?[^\\]/\\)"  1 'jemdoc-mode-face-italics prepend)

   ;; tilde blocks
   '("^~~~" . 'jemdoc-mode-face-tilde-block-delimiters)
   '(jemdoc-mode-highlight-curly-brackets-tilde-block
     . 'jemdoc-mode-face-tilde-block-delimiters)

   ;; {{html text}}
   '(jemdoc-mode-highlight-curly-brackets-html-text
     0 'jemdoc-mode-face-html-text t)

   ;; +{{monospace html text}}+ or %monospace html text%
   '(jemdoc-mode-highlight-monospace-html-text
     0 'jemdoc-mode-face-monospace-html t)

   ;; inline $equations$
   '("\\$.*?\\$" . 'jemdoc-mode-face-equation)

   ;; \(equations\)
   '("^ *\\\\(.*\\\\)" 0 'jemdoc-mode-face-equation t)

   ;; [http/mail/files ...]
   '("\\[\\(http\\|mail\\|\\./\\|/\\).*\\]"
     0 'jemdoc-mode-face-http-mail prepend)

   ;; syntax-table stuff
   '(jemdoc-mode-keywords-in-comments-property-retrieve
     0 'jemdoc-mode-face-special-keywords t)

   ;; #include{...} and #includeraw{...}
   ;; since I use "t" as a third argument I can directly nest \\(.*?\\)
   ;; in the first group
   '("\\(?:^ *# *\\)\\(include\\(?:raw\\)?{\\(.*?\\)}\\)"
     (1 'jemdoc-mode-face-special-keywords t)
     (2 'jemdoc-mode-face-special-keywords-name t))

   ;; 2--3 different-sized dashes
   ;; I have to put this after the ^-, ^--, ^---, ...
   '("[^-\\]\\(-\\{3,3\\}\\)[^-]" 1 'jemdoc-mode-face-dashes-3)
   '("[^-\\]\\(-\\{2,2\\}\\)[^-]" 1 'jemdoc-mode-face-dashes-2)

   ;; ... ellipsis
   ;; I have to put this after the ^\\., ^\\.\\., ^\\.\\.\\., ...
   '("[^-\\]\\(\\.\\{3,3\\}\\)[^-]" 1 'jemdoc-mode-face-ellipsis)

   ;; ---------------------------------------------------------

   ;; other
   `(,(regexp-opt '("\\n" "\\A" "\\C" "\\R" "\\M" "\\\#" "\\`" "\\\'" "\\\"")) 0
     'jemdoc-mode-face-other))
  "Keywords to highlight in jemdoc mode.")



;;;###autoload
(define-derived-mode jemdoc-mode prog-mode "jemdoc"
  "Major mode for editing jemdoc files."
  (set (make-local-variable 'comment-start) "#") ;; required to use M-;
  (setq-local font-lock-defaults
	      '(jemdoc-mode-font-lock-keywords ;; KEYWORDS
		nil                            ;; KEYWORDS-ONLY
		nil                            ;; CASE-FOLD
		nil                            ;; SYNTAX-ALIST
		(syntax-propertize-function
		 . jemdoc-mode-syntax-propertize-function)
		(font-lock-extend-after-change-region-function
		 . jemdoc-mode-extend-region-initialize)
		;; sometimes the region used in jit-lock
		;; doesn't contain the whole block so I
		;; prefer to not use it by default
		(font-lock-support-mode . jemdoc-mode-font-lock-support-mode)))
  (add-hook 'font-lock-extend-region-functions 'jemdoc-mode-extend-region)
  ;; I don't need (setq-local font-lock-multiline t)
  (set-syntax-table jemdoc-mode-font-lock-syntax-table))



;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jemdoc\\'" . jemdoc-mode))

(provide 'jemdoc-mode)

;;; jemdoc-mode.el ends here
