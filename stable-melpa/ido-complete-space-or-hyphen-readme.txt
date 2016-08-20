The default behavior of ido SPACE key will try to insert SPACE if it makes
sence (a.k.a, the comman part of all matches contains SPACE). Howerver,
when ido is used to complete lisp functions or variables, like what smex
does, HYPHEN is used as separator. This extension for ido inserts SPACE or
HYPHEN whenever which one makes sence, just like what built-in M-x does.

Example:

Choises: "ido-foo-bar", "ido-space" "idotest"

After you type "i", then SPACE key. The input text is completed to "ido-" and
HYPHEN is inserted for you.

However if the choises are "ido-foo-bar", "ido-space" and "ido test", the input
text is completed to "ido", type SPACE again will insert SPACE.

Usage

    (require 'ido-complete-space-or-hyphen)
    (ido-mode t)

Changes

-   1.1 (2013-02-27)

    -  Add `ido-complete-space-or-hyphen--insert-space' to allow user type
       SPACE twice to insert SPCE.

(eval-when-compile
  (require 'ido))

(defvar ido-complete-space-or-hyphen t
  "Set to nil to disable ido-complete-space-or-hyphen.

Useful to temporary disable withing a function:

    (let ((ido-complete-space-or-hyphen nil))
      (ido-completing-read ...))")

(defvar ido-complete-space-or-hyphen--insert-space nil
  "Internal variable to indicate whether SPACE should be inserted
when both SPACE and HYPHEN make sence.

It allows user press SPACE twice to insert real SPACE.
")

(defun ido-complete-space-or-hyphen ()
  "Try completion unless inserting the SPACE or HYPHEN makes sense."
  (interactive)
  (let ((space-or-hyphen
         (and (stringp ido-common-match-string)
              (stringp ido-text)
              (cond
               ;; test whether next char is SPACE or HYPHEN
               ((> (length ido-common-match-string) (length ido-text))
                (car-safe (member (aref ido-common-match-string (length ido-text)) '(?  ?-))))
               (ido-matches
                (let ((re (concat (regexp-quote ido-text) "\\([- ]\\)"))
                      (comp ido-matches)
                      space-or-hyphen name)
                  (while comp
                    (setq name (ido-name (car comp)))
                    (if (string-match re name)
                        ;; If both SPACE and HYPHEN matches
                        (if (and space-or-hyphen (not (= space-or-hyphen (aref (match-string 1 name) 0))))
                            (if ido-complete-space-or-hyphen--insert-space
                                ;; insert SPACE if user has typed SPACE twice
                                (setq ido-complete-space-or-hyphen--insert-space nil
                                      space-or-hyphen 32
                                      comp nil)
                              ;; do not insert any the first time, but mark the flag
                              (setq ido-complete-space-or-hyphen--insert-space t
                                    space-or-hyphen nil
                                    comp nil))
                          (setq comp nil space-or-hyphen nil)
                          (setq space-or-hyphen (aref (match-string 1 name) 0))))
                    (setq comp (cdr comp)))
                  space-or-hyphen))
               (t nil)))))
    (if space-or-hyphen
        (insert (char-to-string space-or-hyphen))
      (ido-complete))))

replace ido-complete-space with ido-complete-space-or-hyphen
(defadvice ido-complete-space (around ido-complete-space-or-hyphen () activate compile)
  (if ido-complete-space-or-hyphen
      (call-interactively 'ido-complete-space-or-hyphen)
    ad-do-it))

###autoload
(defun ido-complete-space-or-hyphen-enable ()
  "Enable ido-complete-space-or-hyphen"
  (interactive)
  (ad-enable-advice 'ido-complete-space 'around 'ido-complete-space-or-hyphen)
  (ad-activate 'ido-complete-space)
  (setq ido-complete-space-or-hyphen t))

###autoload
(defun ido-complete-space-or-hyphen-disable ()
  "Disable ido-complete-space-or-hyphen"
  (interactive)
  (setq ido-complete-space-or-hyphen nil))

(provide 'ido-complete-space-or-hyphen)
