This package provides motions and text objects for delimited arguments in
Evil, the extensible vi layer. To activate it, add the following to your
.emacs:

    (add-to-list 'load-path "path/to/evil-args")
    (require 'evil-args)

    ;; bind evil-args text objects
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

    ;; bind evil-forward/backward-args
    (define-key evil-normal-state-map "L" 'evil-forward-arg)
    (define-key evil-normal-state-map "H" 'evil-backward-arg)
    (define-key evil-motion-state-map "L" 'evil-forward-arg)
    (define-key evil-motion-state-map "H" 'evil-backward-arg)

    ;; bind evil-jump-out-args
    (define-key evil-normal-state-map "K" 'evil-jump-out-args)

See README.md for more details.


(require 'evil)

(defgroup evil-args nil
  "Motions and text objects for delimited arguments in Evil."
  :group 'evil)

(defcustom evil-args-openers '("(" "{" "[")
  "Argument openers"
  :type '(sexp))

(defcustom evil-args-closers '(")" "}" "]")
  "Argument closers."
  :type '(sexp))

(defcustom evil-args-delimiters '("," ";")
  "Argument delimiters."
  :type '(sexp))

(defun evil-args--backward-delimiter (&optional count)
  (let ((openers-regexp (regexp-opt evil-args-openers))
	(closers-regexp (regexp-opt evil-args-closers))
	(delimiters-regexp (regexp-opt evil-args-delimiters))
	(all-regexp (regexp-opt (append evil-args-openers
					evil-args-closers
					evil-args-delimiters)))
	(begin -1)
	(count (or count 1)))
    (save-excursion
      (while (and (< begin 0)
		  (> count 0))
	;; search backwards for delimiter, opener, or closer
	(if (not (re-search-backward all-regexp nil t))
	    ;; not found
	    (setq begin (- (point-at-bol) 1))
	  ;; found:
	  ;; skip over any matching pairs if necessary
	  (while (looking-at-p closers-regexp)
	    (evil-jump-item)
	    (backward-char))
	  ;; if looking at an opener, stop
	  (if (looking-at-p openers-regexp)
	      (setq begin (point))
            ;; looking at a delimiter: decrement count and check if done
	    (when (and (looking-at-p delimiters-regexp)
		       (<= (setq count (- count 1)) 0))
	      (setq begin (point)))))))
    (if begin (goto-char begin))))

(defun evil-args--forward-delimiter (&optional count)
  (let ((openers-regexp (regexp-opt evil-args-openers))
	(closers-regexp (regexp-opt evil-args-closers))
	(delimiters-regexp (regexp-opt evil-args-delimiters))
	(all-regexp (regexp-opt (append evil-args-openers
					evil-args-closers
					evil-args-delimiters)))
	(end -1)
	(count (or count 1)))
    (save-excursion
      (while (and (< end 0)
		  (> count 0))
	;; search forward for a delimiter, opener, or closer
	(if (not (re-search-forward all-regexp nil t))
	    ;; not found
	    (setq end (point-at-eol))
	  ;; found:
	  ;; skip over any matching pairs if necessary
	  (backward-char)
	  (while (looking-at-p openers-regexp)
	    (evil-jump-item)
	    (forward-char))
	  ;; if looking at a closer, stop
	  (if (looking-at-p closers-regexp)
	      (setq end (point))
            ;; looking at a delimiter: decrement count and check if done
	    (when (looking-at-p delimiters-regexp)
	      (if (<= (setq count (- count 1)) 0)
		  (setq end (point))
		(forward-char)))))))
    (if end (goto-char end))))


(defun evil-args--backward-arg-no-skip (count)
  (evil-args--backward-delimiter (or count 1))
  (forward-char)
  (when (looking-at " ")
    (forward-char))
  (when (looking-at "\n")
    (evil-next-line)
    (evil-first-non-blank)))

###autoload
(defun evil-backward-arg (count)
  "Move the cursor backward COUNT arguments."
  (interactive "p")
  (let ((delimiters-regexp (regexp-opt evil-args-delimiters)))
    (evil-args--backward-arg-no-skip
     (+ (if (looking-back (concat delimiters-regexp
				  "[\t\n ]*")) 1 0) (or count 1)))))

###autoload
(defun evil-forward-arg (count)
  "Move the cursor forward COUNT arguments."
  (interactive "p")
  (let ((closers-regexp (regexp-opt evil-args-closers)))
  (evil-args--forward-delimiter (or count 1))
  (when (not (looking-at-p closers-regexp))
    (forward-char)
    (when (looking-at " ")
      (forward-char))
    (when (looking-at "\n")
      (evil-next-line)
      (evil-first-non-blank)))))

###autoload (autoload 'evil-inner-arg "evil-args")
(evil-define-text-object evil-inner-arg (count &optional beg end type)
  "Select inner delimited argument."
  (let ((begin (save-excursion (evil-args--backward-arg-no-skip 1) (point)))
        (end (save-excursion (evil-args--forward-delimiter) (point))))
    (evil-range begin end)))

###autoload (autoload 'evil-outer-arg "evil-args")
(evil-define-text-object evil-outer-arg (count &optional beg end type)
  "Select a delimited argument."
  (let ((openers-regexp (regexp-opt evil-args-openers))
	(begin nil)
        (end-on-closer nil)
        (end nil))
    (save-excursion
      (evil-args--forward-delimiter)
      (if (member (string (char-after)) evil-args-delimiters)
          (evil-forward-arg 1)
        (setq end-on-closer t))
      (setq end (point)))
    (save-excursion
      (evil-args--backward-arg-no-skip 1)
      (if (and end-on-closer
	       (not (looking-back (concat openers-regexp
					  "[\t\n ]*"))))
          (progn (evil-args--backward-delimiter)
                 (setq begin (point)))
        (setq begin (point))))
     (evil-range begin end)))

###autoload
(defun evil-jump-out-args (count)
  "Move the cursor out of the nearest enclosing matching pairs."
  (interactive "p")
  (setq count (or count 1))
  (let ((openers-regexp (regexp-opt evil-args-openers))
	(closers-regexp (regexp-opt evil-args-closers))
	(delimiters-regexp (regexp-opt evil-args-delimiters))
	(all-regexp (regexp-opt (append evil-args-openers
					evil-args-closers
					evil-args-delimiters))))
    (while (> count 0)
      (let ((begin -1)
	    (success nil))
	(save-excursion
	  (while (< begin 0)
	    (if (not (re-search-backward all-regexp nil t))
		(setq begin (- (point-at-bol) 1))
	      (while (looking-at-p closers-regexp)
		(evil-jump-item)
		(backward-char))
	      (when (looking-at-p openers-regexp)
		(setq begin (point))
		(setq success t))))
	  (when success
	    (evil-backward-word-end)
	    (if (not (looking-at closers-regexp))
		(evil-backward-word-begin)
	      (evil-jump-item)
	      (forward-char))
	    (setq begin (point))))
	(if begin (goto-char begin)))
      (setq count (- count 1)))))

declare evil motions
(evil-declare-motion 'evil-forward-arg)
(evil-declare-motion 'evil-backward-arg)
(evil-declare-motion 'evil-jump-out-args)

(provide 'evil-args)
evil-args.el ends here
