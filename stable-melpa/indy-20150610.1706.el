;;; indy.el --- A minor mode and EDSL to manage your mode's indentation rules.

;; Copyright (C) 2015  Kevin W. van Rooijen

;; Author: Kevin W. van Rooijen <kevin.van.rooijen@attichacker.com>
;; Keywords: convenience, matching, tools
;; Package-Version: 20150610.1706

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; A library which let's you set your custom indentation rules using a small EDSL.
;;
;; Functions:
;;
;; * indy
;;
;; EDSL Functions:
;; * indy--next-tab
;; * indy--current-tab
;; * indy--prev-char
;; * indy--next-char
;; * indy--current-char
;; * indy--prev
;; * indy--next
;; * indy--current
;; * indy--ends-on
;; * indy--starts-with
;; * indy--contains
;;
;;; Code:

(defgroup indy ()
  "Customize group for indy.el"
  :prefix "indy-"
  :group 'indent)

(defcustom indy-use-tab-cycle t
  "Use tab to cycle through 3 indentations depending on previous line."
  :type 'boolean
  :group 'indy)

(defcustom indy-skip-empty-lines nil
  "If you're comparing to 'previous line' it must contain characters."
  :type 'boolean
  :group 'indy)

(defcustom indy-cycle-zero nil
  "When cycling through the 3 indentation you also cycle to the beginning of the line."
  :type 'boolean
  :group 'indy)

(defcustom indy-indent-key "TAB"
  "The key to run indy."
  :type '(string)
  :group 'indy)

(defvar indy-rules '())

;;;###autoload
(defun indy ()
  "Indent current line using Indy.
This will indent the current line according to your indy rules."
  (interactive)
  (let ((rule (indy--get-rule)))
    (if (and (not (region-active-p)) rule)
        (eval rule)
      (indy--fallback))))

(defun indy--fallback ()
  "If no rules are applicable then use the fallback function.
If 'indy-use-tab-cycle' is non nil use the 3 indentation cycling.
If 'indy-use-tab-cycle' is nil then use 'indent-for-tab-command.'"
  (if indy-use-tab-cycle
      (indy--cycle)
    (indent-for-tab-command)))

(defun indy--indent (num)
  "Indent the current line by the amount of provided in NUM."
  (unless (equal (indy--current-indent) num)
    (let* ((num (indy--fix-num num))
           (ccn (+ (current-column) (- num (indy--current-indent)))))
      (indent-line-to num)
      (move-to-column (indy--fix-num ccn)))))

(defun indy--cycle ()
  "Cycle through 3 indentations depending on the previous line."
  (let* ((c (indy--current-indent))
         (p (indy--prev-indent))
         (w (cond
             ((< c (- p tab-width)) (- p tab-width))
             ((< c p) p)
             ((equal c p) (+ p tab-width))
             ((equal c p) (+ p tab-width))
             (t  (if indy-cycle-zero 0 (- p tab-width))))))
    (indy--indent w)))

(defun indy--prev-indent ()
  "Get the amount of indentation spaces if the previous line."
  (save-excursion
    (previous-line 1)
    (while (and (indy--line-empty?) indy-skip-empty-lines)
      (previous-line 1))
    (back-to-indentation)
    (current-column)))

(defun indy--next-indent ()
  "Get the amount of indentation spaces if the next line."
  (save-excursion
    (next-line 1)
    (while (and (indy--line-empty?) indy-skip-empty-lines)
      (next-line 1))
    (back-to-indentation)
    (current-column)))

(defun indy--current-indent ()
  "Get the amount of indentation spaces if the current line."
  (save-excursion
    (back-to-indentation)
    (current-column)))

(defun indy--get-next-line ()
  "Get the next line as a string."
  (save-excursion
    (next-line 1)
    (while (and (indy--line-empty?) indy-skip-empty-lines)
      (next-line 1))
    (indy--get-current-line)))

(defun indy--get-prev-line ()
  "Get the previous line as a string."
  (save-excursion
    (previous-line 1)
    (while (and (indy--line-empty?) indy-skip-empty-lines)
      (previous-line 1))
    (indy--get-current-line)))

(defun indy--get-current-line ()
  "Get the current line as a string."
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun indy--line-empty? ()
  "Check if the current line is empty."
  (string-match "^\s*$" (indy--get-current-line)))

(defun indy--rules ()
  "Check if the current line is empty."
  "Get the indent rules of the current major mode as well as the default 'all' rules"
  (let ((mode-rules (cdr (assoc major-mode indy-rules)))
        (all (cdr (assoc 'all indy-rules))))
    (append (if mode-rules mode-rules '()) all)))

(defun indy--get-rule ()
  "Get the defined rules of the current major mode and the 'all' rules."
  (let* ((filter-list (remove-if-not (lambda(x) (eval (car x))) (indy--rules))))
    (car (last (car filter-list)))))

(defun indy--escape-regexp (reg)
  "Escape regexp.
Argument REG regular expression to escape."
  (replace-regexp-in-string "\\[" "\\\\[" reg))

(defun indy--fix-num (num)
  "Make sure NUM is a valid number for calculating indentation."
  (cond
   ((not num) 0)
   ((< num 0) 0)
   (t num)))

;; EDSL

(defun indy--prev-tab (&optional num)
  "Indent the current line by previous line indentation + tab-with * NUM."
  (indy--indent (+ (indy--prev-indent) (* (indy--fix-num num) tab-width))))

(defun indy--next-tab (&optional num)
  "Indent the current line by next line indentation + tab-with * NUM."
  (indy--indent (+ (indy--next-indent) (* (indy--fix-num num) tab-width))))

(defun indy--current-tab (&optional num)
  "Indent the current by NUM."
  (indy--indent (indy--fix-num num)))

(defun indy--prev-char (&optional num)
  "Indent the current line by previous line indentation + NUM."
  (indy--indent (+ (indy--prev-indent) (indy--fix-num num))))

(defun indy--next-char (&optional num)
  "Indent the current line by next line indentation + NUM."
  (indy--indent (+ (indy--next-indent) (indy--fix-num num))))

(defun indy--current-char (&optional num)
  "Indent the current line by NUM."
  (indy--indent (indy--fix-num num)))

(defun indy--prev (function &rest values)
  "Apply a line check on the previous line using the EDSL FUNCTIONs.
Optional argument VALUES Values to compare with."
  "Previous line as a target"
  (funcall function (indy--get-prev-line) values))

(defun indy--next (function &rest values)
  "Apply a line check on the next line using the EDSL FUNCTIONs.
Optional argument VALUES Values to compare with."
  (funcall function (indy--get-next-line) values))

(defun indy--current (function &rest values)
  "Apply a line check on the current line using the EDSL FUNCTIONs.
Optional argument VALUES Values to compare with."
  (funcall function (indy--get-current-line) values))

(defun indy--ends-on (line values)
  "Check if LINE ends on one of the following strings.
Argument VALUES Values to compare with."
  (remove-if-not (lambda (x)
                   (string-match (concat (indy--escape-regexp x) "\s*$") line)) values))

(defun indy--starts-with (line values)
  "Check if LINE start with one of the following strings.
Argument VALUES Values to compare with."
  (remove-if-not (lambda (x)
                   (string-match (concat "^\s*" (indy--escape-regexp x) ) line)) values))

(defun indy--contains (line values)
  "Check if LINE has any of the following strings (regexp).
Argument VALUES Values to compare with."
  (remove-if-not (lambda (x) (string-match x line)) values))

(defvar indy-mode-map (make-keymap) "Indy-mode keymap.")

(define-minor-mode indy-mode
  "One indentation mode to rule them all."
  nil " Indy" 'indy-mode-map)

(define-key indy-mode-map (kbd indy-indent-key) 'indy)

(provide 'indy)
;;; indy.el ends here
