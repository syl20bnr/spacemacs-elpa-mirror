;;; preproc-font-lock.el --- Highlight C-style preprocessor directives.

;; Copyright (C) 2003,2007,2014 Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: c, languages, faces
;; Package-Version: 20151107.2018
;; Created: 2003-??-??
;; Version: 0.0.5
;; URL: https://github.com/Lindydancer/preproc-font-lock

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

;;; Commentary:

;;{{{ Documentation

;; *Preproc Font Lock* is an Emacs package that highlight C-style
;; preprocessor directives. The main feature is support for macros
;; that span multiple lines.
;;
;; Preproc Font Lock is implemented as two minor modes:
;; `preproc-font-lock-mode' and `preproc-font-lock-global-mode'. The
;; former can be applied to individual buffers and the latter to all
;; buffers.

;; Installation:
;;
;; Place this package in a directory in the load-path. To activate it,
;; use *customize* or place the following lines in a suitable init
;; file:
;;
;;    (require 'preproc-font-lock)
;;    (preproc-font-lock-global-mode 1)

;; Customization:
;;
;; You can customize this package using the following:
;;
;; * `preproc-font-lock-preprocessor-background' -- The *face* used to
;;   highlight the preprocessor directive
;;
;; * `preproc-font-lock-modes' -- A list of major modes. A buffer is
;;   highlighted if it's major mode is, or is derived from, a member
;;   of this list.

;; Example:
;;
;; Below is a screenshot of a sample C file, demonstrating the effect
;; of this package:
;;
;; ![See doc/demo.png for screenshot of Preproc Font Lock](doc/demo.png)

;;}}}

;;; Code:

;;{{{ Variables

(defgroup preproc-font-lock nil
  "Highlight preprocessor directives of C-like languages."
  :group 'faces)


(defface preproc-font-lock-preprocessor-background
  '((t :inherit highlight))
  "Default face for highlighting preprocessor statements."
  :group 'preproc-font-lock)

(defcustom preproc-font-lock-preprocessor-background-face
  'preproc-font-lock-preprocessor-background
  "Face for highlighting preprocessor statements."
  :type 'face
  :group 'preproc-font-lock)

;;;###autoload
(defcustom preproc-font-lock-modes '(c-mode c++-mode objc-mode)
  "List of major modes where Preproc Font Lock Global mode should be enabled."
  :group 'preproc-font-lock
  :type '(repeat symbol))

;;}}}
;;{{{ The modes

;;;###autoload
(define-minor-mode preproc-font-lock-mode
  "Minor mode that highlights preprocessor directives."
  :group 'preproc-font-lock
  (if preproc-font-lock-mode
      (preproc-font-lock-add-keywords)
    (preproc-font-lock-remove-keywords))
  ;; As of Emacs 24.4, `font-lock-fontify-buffer' is not legal to
  ;; call, instead `font-lock-flush' should be used.
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))


;;;###autoload
(define-global-minor-mode preproc-font-lock-global-mode preproc-font-lock-mode
  (lambda ()
    (when (apply 'derived-mode-p preproc-font-lock-modes)
      (preproc-font-lock-mode 1)))
  :group 'preproc-font-lock)


;;}}}
;;{{{ Match functions

;; The idea here is to use the font-lock "achored" model. We match the
;; "#" sign and then do a submatch line-by-line for the preprocessor
;; statement.
;;
;; The main match function `preproc-font-lock-match-statement-line' finds the
;; end of the preprocessor statement. The line-by-line matching
;; function simple match each line until it reaches the limit.
;;
;; Note: Should we match the entire statement as one single match,
;; Emacs would extend the highlighted area to the right side of the
;; display. With the current solution, the highligt stop at the last
;; character on the line.

(defvar preproc-font-lock-match-debug nil
  "When non-nil, messages are beging echoed.")

(defun preproc-font-lock-match-pre ()
  "Prepare for matching multi-line preprocessor directive."
  ;; Set up the line-by-line search.
  (if preproc-font-lock-match-debug
      (message "preproc-font-lock-match-pre called. Point is %s" (point)))
  ;; ----------
  ;; Tell font-lock not to stop after one or a few lines.
  (setq font-lock-multiline t)
  ;; Move the point to include the "#" part.
  (beginning-of-line)
  ;; ----------
  ;; Find the end of the preprocessor statement.
  ;;
  ;; (Note: Do not return "point-max"; it works but it really slows
  ;; down font-lock.)
  (save-excursion
    (while (progn
             (end-of-line)
             (and
              (eq (char-before) ?\\)
              (not (eobp))))
      (forward-line))
    (point)))                           ; Return new search limit.


(defun preproc-font-lock-match-statement-line (limit)
  "Match function for highlighting preprocessor statements."
  (if preproc-font-lock-match-debug
      (message
       "preproc-font-lock-match-statement-line called at %s with limit %s"
       (point) limit))
  ;; Match one line at a time until we hit the limit.
  (if (>= (point) limit)
      nil
    (looking-at "^.*$")                 ; Always true.
    (forward-line)
    t))


(defvar preproc-font-lock-keywords
  '(("^\\s *#"
     (preproc-font-lock-match-statement-line
      (preproc-font-lock-match-pre)
      nil
      (0 preproc-font-lock-preprocessor-background-face append t))))
  "Highlighting rules used by Preproc Font Lock.")

(defun preproc-font-lock-add-keywords (&optional mode)
  "Install keywords into major MODE, or into current buffer if nil."
  (font-lock-add-keywords mode preproc-font-lock-keywords t))

(defun preproc-font-lock-remove-keywords (&optional mode)
  "Remove keywords from major MODE, or from current buffer if nil."
  (font-lock-remove-keywords mode preproc-font-lock-keywords))

;;}}}
;;{{{ Profile support

;; The following (non-evaluated) section can be used to
;; profile this package using `elp'.
;;
;; Invalid indentation on purpose!

(cond (nil
(setq elp-function-list
      '(preproc-font-lock-match-statement-line))))

;;}}}

;;{{{ The end

(provide 'preproc-font-lock)

;;}}}

;;; preproc-font-lock.el ends here.
