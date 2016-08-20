;;; propfont-mixed.el --- Use proportional fonts with space-based indentation. -*- lexical-binding: t -*-
;;
;; Copyright (C) 2014 Kirill Ignatiev <github.com/ikirill>
;;
;; Author: Kirill Ignatiev <github.com/ikirill>
;; Version: 0.1
;; Package-Version: 20150113.1411
;; Keywords: faces
;; URL: https://github.com/ikirill/propfont-mixed
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Enable use of variable-width fonts for displaying symbols,
;; in a way that does not conflict with fixed-width-space-based
;; indentation.
;;
;; Notes:
;;
;; - Customize `propfont-mixed-inhibit-regexes' to forbid some symbols
;;   from being shown with proportional fonts. See also
;;   `propfont-mixed-inhibit-function', and `propfont-mixed-inhibit-faces'.
;;
;; - It is probably necessary to adjust the face `variable-pitch', so
;;   that the proportional font looks good and is the correct size to
;;   match the default font.
;;
;;; Code:

(require 'cl-lib)

;; {{{ Customizations

(defgroup propfont-mixed nil
  "Customization options for `propfont-mixed-minor-mode'.

This minor mode enables the use of variable-width fonts for
displaying symbols, in a way that does not conflict with
fixed-width-space-based indentation."
  :group 'font-lock)

(defcustom propfont-mixed-inhibit-faces
  '(font-lock-warning-face font-lock-preprocessor-face)
  "Ignore symbols based on their font-lock faces.

This might be useful for constants, built-ins, types, keywords,
functions, etc."
  :group 'propfont-mixed
  :type
  '(repeat
    (choice
     (const :tag "Built-ins" font-lock-builtin-face)
     (const :tag "Constants" font-lock-constant-face)
     (const :tag "Function declarations/defitions" font-lock-function-face)
     (const :tag "Keywords" font-lock-keyword-face)
     (const :tag "Preprocessor statements" font-lock-preprocessor-face)
     (const :tag "Types" font-lock-type-face)
     (const
      :tag "Variable declarations/defitions" font-lock-variable-name-face)
     (const :tag "Warnings" font-lock-warning-face)
     (face :tag "Other"))))

(defcustom propfont-mixed-inhibit-regexes
  '("\\`\\(?:t\\|nil\\)\\'"
    "\\`..?\\'")
  "List of regular expressions that a symbol must not match,
if it is to be displayed with a proportional font. Typically this
is for symbols that look too unusual in a proportional font.

The regexes will not be anchored to string beginning or end, so
surround the regex with \"\\`\" and \"\\'\" if necessary. See
also `rx' and `regexp-builder' which simplify building regexes."
  :group 'propfont-mixed
  :type '(repeat string)
  :safe (lambda (it) (not (cl-remove-if #'stringp it))))

(defcustom propfont-mixed-inhibit-function nil
  "A user-specified function that causes symbols to be ignored.

It will be called with no arguments, with the current match data
being the symbol that was found. If it returns non-nil, the
symbol will be ignored by propfont-mixed.

For example, this could be
  (lambda () (eq 4 (car (syntax-after (1- (match-beginning 0))))))
to ignore symbols appearing after an open parenthesis-type character."
  :group 'propfont-mixed
  :type '(choice (const nil) function))

;; }}}
;; {{{ Fontification

(defun propfont-mixed--match-valid ()
  (let (col-here col-next syntax)
    (save-excursion
      (and
       (setq syntax (syntax-ppss (match-beginning 0)))
       (not (nth 3 syntax))
       (not (nth 4 syntax))
       (goto-char (match-end 0))
       (setq col-here (current-column))
       (setq col-next (save-excursion (when (= 0 (forward-line))
                                        (back-to-indentation)
                                        (current-column))))
       (>= col-here col-next)
       (goto-char (match-beginning 0))
       ;; Various user customizations
       (not (memq (face-at-point) propfont-mixed-inhibit-faces))
       (not (cl-find-if (lambda (r) (string-match-p r (match-string 0)))
                      propfont-mixed-inhibit-regexes))
       (not (and propfont-mixed-inhibit-function
               (save-excursion (save-match-data (funcall propfont-mixed-inhibit-function)))))))))

(defun propfont-mixed--fontify ()
  ;; See `propfont-mixed--keywords'
  `(face variable-pitch))

(defun propfont-mixed--regexes ()
  ;; special-seps are "internal" symbol separators that don't look
  ;; good in a variable-width font.
  (let* ((special-seps (rx (or "--" "__")))
         (r (rx
             (or symbol-start "--" "__")
             (group (char alpha ?_) (*? (or (syntax word) (syntax symbol))))
             (or symbol-end "--" "__"))))
    (cons r special-seps)))
;; (insert ?\n (prin1-to-string (propfont-mixed--regexes)))

(defun propfont-mixed--match (limit)
  ;; See `propfont-mixed--keywords'
  (let (match valid)
    (while (and
            (setq
             valid nil
             match (search-forward-regexp
                    (rx
                     (or symbol-start "--" "__")
                     (group (char alpha ?_) (*? (or (syntax word) (syntax symbol))))
                     (or symbol-end "--" "__"))
                    limit t))
            (not (setq valid (propfont-mixed--match-valid)))))
    (when (and match valid)
      (save-match-data
        (when (looking-back "--") (goto-char (match-beginning 0))))
      t)))

;; }}}
;; {{{ Minor mode

(defvar propfont-mixed--keywords
  ;; See `font-lock-keywords'
  '((propfont-mixed--match 1 (propfont-mixed--fontify) prepend)))

;; ;; Debugging:
;; (defun propfont-mixed--debug ()
;;   (propfont-mixed-minor-mode -1)
;;   (propfont-mixed-minor-mode +1)
;;   ;; Reset font-lock
;;   (font-lock-flush)
;;   (font-lock-ensure))

;;;###autoload
(define-minor-mode propfont-mixed-minor-mode
  "Enable use of variable-width fonts for displaying symbols,
in a way that does not conflict with fixed-width-space-based
indentation.

Notes:

- Customize `propfont-mixed-inhibit-regexes' to forbid some
  symbols from being shown with proportional fonts. See also
  `propfont-mixed-inhibit-function', and `propfont-mixed-inhibit-faces'.

- It is probably necessary to adjust the face `variable-pitch',
  so that the proportional font looks good and is the correct size."
  :group 'propfont-mixed
  :lighter nil
  :require 'font-lock
  (cond
   (propfont-mixed-minor-mode
    ;; Add at the end of the list so that all other faces are already
    ;; assigned.
    (font-lock-add-keywords nil propfont-mixed--keywords 'append)
    ;; Reset font-lock; the function `font-lock-flush' is present from
    ;; 25.0, so maybe fall back; see font-lock.el.gz
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      ;; font-lock-fontify-buffer is really insistent on warning us
      ;; not to use it, which we don't
      (with-no-warnings (font-lock-fontify-buffer))))
   (t
    (font-lock-remove-keywords nil propfont-mixed--keywords)
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (with-no-warnings (font-lock-fontify-buffer))))))

;; }}}

(provide 'propfont-mixed)
;;; propfont-mixed.el ends here
