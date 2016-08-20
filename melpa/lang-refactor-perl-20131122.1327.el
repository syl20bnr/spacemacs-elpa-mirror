;;; lang-refactor-perl.el --- Simple refactorings, primarily for Perl

;; Copyright © 2013 Johan Linsdtrom
;;
;; Author: Johan Lindstrom <buzzwordninja not_this_bit@googlemail.com>
;; URL: https://github.com/jplindstrom/emacs-lang-refactor-perl
;; Package-Version: 20131122.1327
;; Version: 0.1.5
;; Keywords: languages, refactoring, perl

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;
;;; Commentary:
;;
;; Provides commands for simple refactorings for Perl, currently:
;; * extract variable.
;;

;;
;;; Installation:
;;
;; Put in load-path and initialize with:
;;    (require 'lang-refactor-perl)
;;
;;    ;; Suggested key bindings
;;    (global-set-key (kbd "\C-c r e v") 'lr-extract-variable)
;;    (global-set-key (kbd "\C-c r h r") 'lr-remove-highlights)
;;
;; Note: This code is also part of Devel::PerlySense (install from
;; CPAN), so if you're already using that, you won't need to install
;; this package. In that case the key bindings will be slightly
;; different.
;;
;;
;;; Usage:
;;
;; Let's say you have a block of code with annoying small-scale duplication:
;;
;;     # Perl
;;     sub customer_example {
;;         my $self = shift;
;;         my @customers = $self->schema->resultset("Customer")->all;
;;         my $customer_row = $self->schema->resultset("Customer")->find($cust_id);
;;     }
;;
;; Mark a region of code you want to extract, in this case
;;
;;     $self->schema
;;
;; and then type
;;     C-c r e v (bound to M-x lr-extract-variable)
;;
;;
;; You'll be asked for a variable name, with a suitable default (in
;; this case "$schema").
;;
;; Hit return and all occurrences of $self->schema is replaced with
;; $schema. Like this:
;;
;;     # Perl
;;     sub customer_example {
;;         my $self = shift;
;;         my $schema = $self->schema;
;;         my @customers = $schema->resultset("Customer")->all;
;;         my $customer_row = $schema->resultset("Customer")->find($cust_id);
;;     }
;;
;; The new variable "$schema" is declared and initialized before the
;; first use, but you may well need to move it around to a more
;; suitable space.
;;
;; All edits are highlighted. Once you've eye-balled the refactoring,
;; run
;;     M-x lr-remove-highlights
;; to remove them. If you're not happy, just undo the edit.
;;
;; The mark was set, so you can jump back with "C-u C-SPC".
;;
;; In this example, there's still duplication, so let's extract the
;; resultset as well. Mark
;;
;;     $schema->resultset("Customer")
;;
;; and hit "C-c r e v" again. Edit the suggestion to "$customer_rs",
;; and hit return. You'll end up with:
;;
;;     # Perl
;;     sub customer_example {
;;         my $self = shift;
;;         my $schema = $self->schema;
;;         my $customer_rs = $schema->resultset("Customer");
;;         my @customers = $customer_rs->all;
;;         my $customer_row = $customer_rs->find($cust_id);
;;     }
;;
;; It's up to you to mark syntactically relevant portions of the
;; code.
;;
;;
;;
;; For more details, see the function documentation:
;;     C-c r e v (bound to M-h f lr-extract-variable)
;;
;;
;; Suggested key bindings, forwards compatible with future
;; refactorings and other features (like "Toggle Highlight"):
;;    (global-set-key (kbd "\C-c r e v") 'lr-extract-variable)
;;    (global-set-key (kbd "\C-c r h r") 'lr-remove-highlights)
;;
;;
;;; Changes
;;
;; 2013-11-22 - 0.1.5
;;
;; * No really, fixed word boundaries properly
;;
;;
;; 2013-11-09 - 0.1.4
;;
;; * Fixed matching to look at word boundaries
;;



;;; Code:

(defun lr/debug (value &optional mess)
  (prin1 value)
  (message "^^ JPL %s" (or mess ""))
  )

;; TODO: defcustom
(defvar lr-extract-variable-face
  '(:background "bisque"))

;; (defvar lr/extract-variable-restore-face
;;   '(:background 'inherit))

;; to reset
;; (setq lr-extract-variable-face
;;   '(:background "bisque"))
;; (setq lr/extract-variable-restore-face
;;   '(:background "red"))


(defun lr/regex-end-word-boundary (str end-word-boundary)
  "Append a \\b to STR to make it match a word boundary if
END-WORD-BOUNDARY is true"
  (if end-word-boundary
      (concat str "\\b")
    str
    )
  )

(defun lr/open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun lr/get-variable-name (expression)
    (if (string-match "\\([[:alnum:]_]+?\\)\\([^[:alnum:]_]+?\\)?$" expression)
        (format  "$%s" (match-string-no-properties 1 expression))
      (error "Could not find a variable name in (%s)" expression)
      )
  )

(defun lr/replace-all-buffer (search-for replace-with)
  (goto-char (point-min))
  (let* ((quoted-search-for (regexp-quote search-for))
         (ends-at-word-boundary (string-match "[a-zA-Z0-9_]$" search-for 1))
         (search-for-rex (lr/regex-end-word-boundary quoted-search-for ends-at-word-boundary))
         )
    ;; (lr/debug quoted-search-for)
    ;; (lr/debug ends-at-word-boundary "ends-at-word-boundary")
    ;; (lr/debug search-for-rex "search-for-rex")
    (while (search-forward-regexp
            search-for-rex nil t)
      (replace-match replace-with nil nil))
    )
  )

(defun lr/goto-earliest-usage (variable-name)
  (goto-char (point-min))
  (search-forward-regexp
   (lr/regex-end-word-boundary variable-name t) nil t)

  ;; if possible, find previous statement terminator ; or closing
  ;; block }
  (when (search-backward-regexp "[;}]" nil t)
      (forward-line)
      ;; If possible, go down past blank lines
      (while (and
              (looking-at "\n")
              (not (eobp)))
        (forward-line))
      )
  )

(defun lr/insert-declaration (variable-declaration)
  (lr/open-line-above)
  (insert variable-declaration)
  (beginning-of-line)
  (search-forward "= " nil t)
  )

;;;###autoload
(defun lr-extract-variable (beg end &optional arg-variable-name)
  "Do refactoring 'extract Perl variable' of active region.

Ask the user for a variable name to extract the active region
into.

Replace all occurences in the current defun with the variable and
insert a variable declarion (initialized with the region text).

Push the mark and then leave point at the new variable
declaration (you'll need to ensure this is a reasonable location
before jumping back).

By default, only the current defun is changed. Invoke with the
prefix arg to change the entire buffer.

Both replacements and the declaration are highlighted."
  (interactive "r")
  (unless (and transient-mark-mode mark-active)
    (error "Select a self-contained piece of code to extract"))
  (set-mark-command nil)
  (let*
      ((should-narrow-to-defun (not current-prefix-arg))
       (expression (buffer-substring-no-properties beg end))
       (variable-name-suggestion (lr/get-variable-name expression))
       (variable-name (or arg-variable-name
                          (read-string
                           (format "Extract (%s) to variable: " expression)
                           variable-name-suggestion nil)))
       (formatted-variable-name
        (propertize variable-name
                    ;; 'font-lock-face lr-extract-variable-face
                    'category 'lr-edit
                    ))
       (variable-declaration
        (format "my %s = %s;" formatted-variable-name expression))
       )
    (save-restriction
      (when should-narrow-to-defun
        (narrow-to-defun))
      (lr/replace-all-buffer expression formatted-variable-name)
      (lr/goto-earliest-usage variable-name)
      (lr/insert-declaration variable-declaration)
      (lr/highlight 'lr-edit)
      )
    )
  )


;;;###autoload
(defun lr-remove-highlights ()
  (interactive)
  (lr/remove-highlights 'lr-edit)
  )

(defun lr/remove-highlights (category)
  "Remove all lr highlights from sections with CATEGORY"
  (lr/do-fn-for-catgory
   category
   (lambda (begin end)
     ;; Restore face
     (lr/set-face-property-at begin end font-lock-variable-name-face)
     ;; Remove category tag, so it doesn't get highlighted again
     (lr/remove-category-at begin end 'lr-edit)
     )
   )
  )

(defun lr/set-face-property-at (begin end face)
  "Set the font-lock-face property to FACE between BEGIN and END"
  (add-text-properties begin end (list 'font-lock-face face))
  )

(defun lr/remove-category-at (begin end category)
  "Remove the category property CATEGORY between BEGIN and END"
  (remove-text-properties begin end (list 'category category))
  )

(defun lr/highlight (category)
  "Highlight all sections with category CATEGORY"
  (lr/do-fn-for-catgory
   category
   (lambda (begin end)
     (lr/set-face-property-at begin end lr-extract-variable-face)
     )))

(defun lr/do-fn-for-catgory (category do-fn)
  "Call 'do-fn begin end' for sections tagged with CATEGORY"
  (save-excursion
    (goto-char (point-min))
    (while
        (let*
            (
             (begin (text-property-any (point) (point-max) 'category category))
             (safe-begin (or begin (point-max)))
             (end (or ;; End of section, or end of buffer
                   (text-property-not-all safe-begin (point-max) 'category category)
                   (point-max)))
             )
          (if (and begin (not (eq begin (point-max))))
              (progn
                (funcall do-fn begin end)
                (goto-char (+ 1 end))
                )
            nil
            ))
      )
    ))



(provide 'lang-refactor-perl)

;;; lang-refactor-perl.el ends here
