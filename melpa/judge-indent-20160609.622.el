;;; judge-indent.el --- detect indent style (indent and tab widths) and change behavior of Emacs

;;; Copyright (C) 2011-2016 yascentur

;; Author:   yascentur <screenname at gmail dot com>
;; Keywords: indent tab
;; Package-Version: 20160609.622
;; Version:  1.1.3b

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

;; Soon after finding a file, the `judge-indent-mode' detects indent style
;; of the file and then it changes the behavior of your Emacs to follow
;; the detected indent style.  It makes it possible to write your own code
;; into another person's or your team's program without breaking the existing
;; indent style.
;;
;; The detection method is described as follows.  First, one-tab, two-space,
;; four-space and eight-space indents at the beginning of all the lines are
;; counted.  Second, the numbers of counted indents are compared.
;; Finally, the indent style is chosen among the following pairs of indent
;; and tab widths.
;;
;;       \  Indent
;;        \  2 4 8
;;     Tab \------
;;       2 | U
;;       4 | X U
;;       8 | X X U <- It cannot distinguish three `U's.
;;       - | X X X

;;; Usage:

;; Add following 3 lines into your init file.
;;
;;     (require 'judge-indent)
;;     (global-judge-indent-mode 1)
;;     (setq judge-indent-major-modes '(c-mode python-mode sh-mode))

;;; Customization:

;; Set default indent width (2, 4 or 8).
;; Default: default value of `c-basic-offset' or 4.
;;
;;     (setq judge-indent-default-indent-width 2)
;;
;; Set default tab width (2, 4 or 8).
;; Default: default value of `tab-width' or 8.
;;
;;     (setq judge-indent-default-tab-width 4)
;;
;; Set flag of preferring tabs or not when tab width cannot be detected
;; Default: default value of `indent-tabs-mode' or `nil'.
;;
;;     (setq judge-indent-prefer-tabs-mode t)
;;
;; Set relative tolerance [%] for judging indent and tab widths.
;; Default: 0 %.
;;
;;     (setq judge-indent-relative-tolerance 3)
;;
;; Set search limit for large size files.
;; Default: 30000 characters (equal to ca. 1000 lines).
;;
;;     (setq judge-indent-search-limit 60000)
;;
;; Set additional variable holding indent width
;;
;;     (add-to-list 'judge-indent-variables-indent-width 'c-basic-offset)

;;; Functions:

;; * judge-indent-mode
;; * judge-indent-buffer
;; * judge-indent-region
;; * judge-indent-{set,set-apply}-indent-tab-widths
;; * judge-indent-{set,set-apply}-default-indent-tab-widths
;; * judge-indent-{set,set-apply}-indent-width{2,4,8}-tab-{disabled,width{2,4,8}}
;; * judge-indent-{set,set-apply}-indent-width
;; * judge-indent-{set,set-apply}-default-indent-width
;; * judge-indent-{set,set-apply}-indent-width{2,4,8}
;; * judge-indent-{set,set-apply}-tab-width
;; * judge-indent-{set,set-apply}-default-tab-width
;; * judge-indent-{set,set-apply}-tab-{disabled,width{2,4,8}}
;; * judge-indent-message-indent-counts-buffer
;; * judge-indent-message-indent-counts-region

;;; Code:

(eval-when-compile (require 'cl))

(defgroup judge-indent nil
  "Judge indent"
  :group  'convenience
  :prefix "judge-indent-")

(defcustom judge-indent-major-modes '()
  "Major modes of applying judge-indent-mode"
  :type  '(repeat symbol)
  :group 'judge-indent)

(defcustom judge-indent-default-indent-width
  (if (and (default-boundp 'c-basic-offset)
           (numberp (default-value 'c-basic-offset)))
      (default-value 'c-basic-offset) 4)
  "Default indent width (2, 4 or 8)"
  :type  'number
  :group 'judge-indent)

(defcustom judge-indent-default-tab-width
  (if (default-boundp 'tab-width) (default-value 'tab-width) 8)
  "Default tab width (4 or 8)"
  :type  'number
  :group 'judge-indent)

(defcustom judge-indent-prefer-tabs-mode
  (if (default-boundp 'indent-tabs-mode) (default-value 'indent-tabs-mode) nil)
  "Prefer tabs or not when tab width cannot be detected"
  :type  'boolean
  :group 'judge-indent)

(defcustom judge-indent-relative-tolerance 0
  "Relative tolerance [%] for judging indent and tab widths"
  :type  'number
  :group 'judge-indent)

(defcustom judge-indent-search-limit 30000
  "Search limit for large size files"
  :type  'number
  :group 'judge-indent)

(defcustom judge-indent-variables-indent-width
  '(c-basic-offset
    indent-level
    standard-indent
    c-indent-level
    perl-indent-level
    cperl-indent-level
    python-indent
    ruby-indent-level
    sh-basic-offset
    awk-indent-level
    lua-indent-level
    pascal-indent-level
    delphi-indent-level
    erlang-indent-level
    smalltalk-indent-amount
    html-basic-offset
    sgml-basic-offset
    html-helper-basic-offset
    yahtml-environment-indent
    nxml-child-indent
    css-indent-level
    cssm-indent-level
    javascript-indent-level
    js-indent-level
    js2-basic-offset
    web-mode-markup-indent-offset
    web-mode-css-indent-offset
    web-mode-code-indent-offset)
  "Variables of indent width"
  :type  '(repeat symbol)
  :group 'judge-indent)

(defvar judge-indent-indent-width judge-indent-default-indent-width
  "Indent width")
(defvar judge-indent-tab-width judge-indent-default-tab-width
  "Tab width")
(defvar judge-indent-count-one-tab-indents 0
  "Count of one-tab indents")
(defvar judge-indent-count-two-space-indents 0
  "Count of two-space indents")
(defvar judge-indent-count-four-space-indents 0
  "Count of four-space indents")
(defvar judge-indent-count-eight-space-indents 0
  "Count of eight-space indents")
(defvar judge-indent-minor-mode-lighter " JI"
  "Minor mode lighter")

(defun judge-indent-set-minor-mode-lighter (indent tab)
  "Set minor mode lighter"
  (setq judge-indent-minor-mode-lighter
        (concat " JI:"
                (number-to-string indent)
                "["
                (if (= tab 0) "-" (number-to-string tab))
                "]"))
  (setcar (cdr (assq 'judge-indent-mode minor-mode-alist))
          'judge-indent-minor-mode-lighter))

(defun judge-indent-set-for-variables (variables value)
  "Set value for variables"
  (when variables
    (when (boundp (car variables)) (set (car variables) value))
    (judge-indent-set-for-variables (cdr variables) value)))

;; Set indent width

(defun judge-indent-set-indent-width-without-message (indent)
  "Set indent width without message"
  (setq judge-indent-indent-width indent)
  (judge-indent-set-for-variables judge-indent-variables-indent-width indent))

(defun judge-indent-set-indent-width (indent)
  "Set indent width"
  (interactive "nIndent width: ")
  (message (concat "Set indent width to "
                   (number-to-string indent)
                   "..."))
  (judge-indent-set-minor-mode-lighter indent judge-indent-tab-width)
  (judge-indent-set-indent-width-without-message indent))

(defun judge-indent-set-default-indent-width ()
  "Set default indent width"
  (interactive)
  (judge-indent-set-indent-width judge-indent-default-indent-width))

(defun judge-indent-set-indent-width2 ()
  "Set indent width to 2"
  (interactive)
  (judge-indent-set-indent-width 2))

(defun judge-indent-set-indent-width4 ()
  "Set indent width to 4"
  (interactive)
  (judge-indent-set-indent-width 4))

(defun judge-indent-set-indent-width8 ()
  "Set indent width to 8"
  (interactive)
  (judge-indent-set-indent-width 8))

;; Set tab width

(defun judge-indent-set-tab-width-without-message (tab)
  "Set tab width without message"
  (setq judge-indent-tab-width tab)
  (if (= tab 0)
      (progn
        (setq indent-tabs-mode nil)
        (setq tab-width judge-indent-default-tab-width))
    (setq indent-tabs-mode t)
    (setq tab-width tab))
  (setq tab-stop-list
        '((* tab-width  1) (* tab-width  2) (* tab-width  3)
          (* tab-width  4) (* tab-width  5) (* tab-width  6)
          (* tab-width  7) (* tab-width  8) (* tab-width  9)
          (* tab-width 10) (* tab-width 11) (* tab-width 12)
          (* tab-width 13) (* tab-width 14) (* tab-width 15))))

(defun judge-indent-set-tab-width (tab)
  "Set tab width"
  (interactive "nTab width: ")
  (message (concat "Set tab "
                   (if (= tab 0) "disabled"
                     (concat "width to " (number-to-string tab)))
                   "..."))
  (judge-indent-set-minor-mode-lighter judge-indent-indent-width tab)
  (judge-indent-set-tab-width-without-message tab))

(defun judge-indent-set-default-tab-width ()
  "Set default tab width"
  (interactive)
  (judge-indent-set-tab-width judge-indent-default-tab-width))

(defun judge-indent-set-tab-disabled ()
  "Set tab disabled"
  (interactive)
  (judge-indent-set-tab-width 0))

(defun judge-indent-set-tab-width2 ()
  "Set tab width to 2"
  (interactive)
  (judge-indent-set-tab-width 2))

(defun judge-indent-set-tab-width4 ()
  "Set tab width to 4"
  (interactive)
  (judge-indent-set-tab-width 4))

(defun judge-indent-set-tab-width8 ()
  "Set tab width to 8"
  (interactive)
  (judge-indent-set-tab-width 8))

;; Set indent and tab widths

(defun judge-indent-set-indent-tab-widths (indent tab)
  "Set indent and tab widths"
  (interactive "nIndent Width: \nnTab width: ")
  (message (concat "Set indent width to "
                   (number-to-string indent)
                   " and tab "
                   (if (= tab 0) "disabled"
                     (concat "width to " (number-to-string tab)))
                   "..."))
  (judge-indent-set-minor-mode-lighter indent tab)
  (judge-indent-set-indent-width-without-message indent)
  (judge-indent-set-tab-width-without-message tab))

(defun judge-indent-set-default-indent-tab-widths ()
  "Set default indent and tab widths"
  (interactive)
  (judge-indent-set-indent-tab-widths judge-indent-default-indent-width
                                      judge-indent-default-tab-width))

(defun judge-indent-set-indent-width2-tab-disabled ()
  "Set indent width to 2 and tab disabled"
  (interactive)
  (judge-indent-set-indent-tab-widths 2 0))

(defun judge-indent-set-indent-width4-tab-disabled ()
  "Set indent width to 4 and tab disabled"
  (interactive)
  (judge-indent-set-indent-tab-widths 4 0))

(defun judge-indent-set-indent-width8-tab-disabled ()
  "Set indent width to 8 and tab disabled"
  (interactive)
  (judge-indent-set-indent-tab-widths 8 0))

(defun judge-indent-set-indent-width2-tab-width2 ()
  "Set indent width to 2 and tab width to 2"
  (interactive)
  (judge-indent-set-indent-tab-widths 2 2))

(defun judge-indent-set-indent-width2-tab-width4 ()
  "Set indent width to 2 and tab width to 4"
  (interactive)
  (judge-indent-set-indent-tab-widths 2 4))

(defun judge-indent-set-indent-width4-tab-width4 ()
  "Set indent width to 4 and tab width to 4"
  (interactive)
  (judge-indent-set-indent-tab-widths 4 4))

(defun judge-indent-set-indent-width2-tab-width8 ()
  "Set indent width to 2 and tab width to 8"
  (interactive)
  (judge-indent-set-indent-tab-widths 2 8))

(defun judge-indent-set-indent-width4-tab-width8 ()
  "Set indent width to 4 and tab width to 8"
  (interactive)
  (judge-indent-set-indent-tab-widths 4 8))

(defun judge-indent-set-indent-width8-tab-width8 ()
  "Set indent width to 8 and tab width to 8"
  (interactive)
  (judge-indent-set-indent-tab-widths 8 8))

;; Set and apply indent width

(defun judge-indent-apply-indent-width (indent)
  "Apply indent width"
  (indent-region (point-min) (point-max)))

(defun judge-indent-set-apply-indent-width (indent)
  "Set and apply indent width"
  (interactive "nIndent width: ")
  (message (concat "Set and apply indent width to "
                   (number-to-string indent)
                   "..."))
  (judge-indent-set-indent-width indent)
  (judge-indent-apply-indent-width indent))

(defun judge-indent-set-apply-default-indent-width ()
  "Set and apply default indent width"
  (interactive)
  (judge-indent-set-apply-indent-width judge-indent-default-indent-width))

(defun judge-indent-set-apply-indent-width2 ()
  "Set and apply indent width to 2"
  (interactive)
  (judge-indent-set-apply-indent-width 2))

(defun judge-indent-set-apply-indent-width4 ()
  "Set and apply indent width to 4"
  (interactive)
  (judge-indent-set-apply-indent-width 4))

(defun judge-indent-set-apply-indent-width8 ()
  "Set and apply indent width to 8"
  (interactive)
  (judge-indent-set-apply-indent-width 8))

;; Set and apply tab width

(defun judge-indent-apply-tab-width (tab)
  "Apply tab width"
  (if (= tab 0) (untabify (point-min) (point-max))
    (tabify (point-min) (point-max))))

(defun judge-indent-set-apply-tab-width (tab)
  "Set and apply tab width"
  (interactive "nTab width: ")
  (message (concat "Set and apply tab "
                   (if (= tab 0) "disabled"
                     (concat "width to " (number-to-string tab)))
                   "..."))
  (judge-indent-set-tab-width tab)
  (judge-indent-apply-tab-width tab))

(defun judge-indent-set-apply-default-tab-width ()
  "Set and apply default tab width"
  (interactive)
  (judge-indent-set-apply-tab-width judge-indent-default-tab-width))

(defun judge-indent-set-apply-tab-disabled ()
  "Set and apply tab disabled"
  (interactive)
  (judge-indent-set-apply-tab-width 0))

(defun judge-indent-set-apply-tab-width2 ()
  "Set and apply tab width to 2"
  (interactive)
  (judge-indent-set-apply-tab-width 2))

(defun judge-indent-set-apply-tab-width4 ()
  "Set and apply tab width to 4"
  (interactive)
  (judge-indent-set-apply-tab-width 4))

(defun judge-indent-set-apply-tab-width8 ()
  "Set and apply tab width to 8"
  (interactive)
  (judge-indent-set-apply-tab-width 8))

;; Set and apply indent and tab widths

(defun judge-indent-set-apply-indent-tab-widths (indent tab)
  "Set and apply indent and tab widths"
  (interactive "nIndent Width: \nnTab width: ")
  (message (concat "Set and apply indent width to "
                   (number-to-string indent)
                   " and tab "
                   (if (= tab 0) "disabled"
                     (concat "width to " (number-to-string tab)))
                   "..."))
  (judge-indent-set-indent-tab-widths indent tab)
  (judge-indent-apply-indent-width indent)
  (judge-indent-apply-tab-width tab))

(defun judge-indent-set-apply-default-indent-tab-widths ()
  "Set and apply default indent and tab widths"
  (interactive)
  (judge-indent-set-apply-indent-tab-widths judge-indent-default-indent-width
                                            judge-indent-default-tab-width))

(defun judge-indent-set-apply-indent-width2-tab-disabled ()
  "Set and apply indent width to 2 and tab disabled"
  (interactive)
  (judge-indent-set-apply-indent-tab-widths 2 0))

(defun judge-indent-set-apply-indent-width4-tab-disabled ()
  "Set and apply indent width to 4 and tab disabled"
  (interactive)
  (judge-indent-set-apply-indent-tab-widths 4 0))

(defun judge-indent-set-apply-indent-width8-tab-disabled ()
  "Set and apply indent width to 8 and tab disabled"
  (interactive)
  (judge-indent-set-apply-indent-tab-widths 8 0))

(defun judge-indent-set-apply-indent-width2-tab-width2 ()
  "Set and apply indent width to 2 and tab width to 2"
  (interactive)
  (judge-indent-set-apply-indent-tab-widths 2 2))

(defun judge-indent-set-apply-indent-width2-tab-width4 ()
  "Set and apply indent width to 2 and tab width to 4"
  (interactive)
  (judge-indent-set-apply-indent-tab-widths 2 4))

(defun judge-indent-set-apply-indent-width4-tab-width4 ()
  "Set and apply indent width to 4 and tab width to 4"
  (interactive)
  (judge-indent-set-apply-indent-tab-widths 4 4))

(defun judge-indent-set-apply-indent-width2-tab-width8 ()
  "Set and apply indent width to 2 and tab width to 8"
  (interactive)
  (judge-indent-set-apply-indent-tab-widths 2 8))

(defun judge-indent-set-apply-indent-width4-tab-width8 ()
  "Set and apply indent width to 4 and tab width to 8"
  (interactive)
  (judge-indent-set-apply-indent-tab-widths 4 8))

(defun judge-indent-set-apply-indent-width8-tab-width8 ()
  "Set and apply indent width to 8 and tab width to 8"
  (interactive)
  (judge-indent-set-apply-indent-tab-widths 8 8))

;; Judge indent and tab widths

(defun judge-indent-judge-from-indent-counts ()
  "Judge indent from indent counts"
  (let ((tolerance (/ (* (+ judge-indent-count-one-tab-indents
                            judge-indent-count-two-space-indents
                            judge-indent-count-four-space-indents
                            judge-indent-count-eight-space-indents)
                         judge-indent-relative-tolerance)
                      100)))
    (if (and (= judge-indent-count-one-tab-indents 0)
             (= judge-indent-count-two-space-indents 0)
             (= judge-indent-count-four-space-indents 0)
             (= judge-indent-count-eight-space-indents 0))
        (judge-indent-set-indent-tab-widths
         judge-indent-default-indent-width
         (if judge-indent-prefer-tabs-mode
             judge-indent-default-tab-width 0))
      (if (<= judge-indent-count-one-tab-indents tolerance)
          (if (and (<= judge-indent-count-four-space-indents tolerance)
                   (<= judge-indent-count-two-space-indents tolerance))
              ;; Indent width is 8
              (judge-indent-set-indent-tab-widths 8 0)
            (if (<= judge-indent-count-two-space-indents tolerance)
                ;; Indent width is 4
                (judge-indent-set-indent-tab-widths
                 4
                 (if (and (= judge-indent-count-eight-space-indents 0)
                          judge-indent-prefer-tabs-mode)
                     8 0))
              ;; Indent width is 2
              (judge-indent-set-indent-tab-widths
               2
               (if (and (= judge-indent-count-four-space-indents 0)
                        (= judge-indent-count-eight-space-indents 0)
                        judge-indent-prefer-tabs-mode)
                   judge-indent-default-tab-width
                 (if (and (= judge-indent-count-eight-space-indents 0)
                          judge-indent-prefer-tabs-mode)
                     8 0)))))
        (if (and (<= judge-indent-count-two-space-indents tolerance)
                 (<= judge-indent-count-four-space-indents tolerance)
                 (<= judge-indent-count-eight-space-indents tolerance))
            ;; Indent width equals tab width
            (judge-indent-set-indent-tab-widths
             judge-indent-default-tab-width
             judge-indent-default-tab-width)
          (if (and (<= judge-indent-count-four-space-indents tolerance)
                   (<= judge-indent-count-eight-space-indents tolerance))
              ;; Indent width is 2 and tab width is 4
              (judge-indent-set-indent-tab-widths 2 4)
            (if (<= judge-indent-count-two-space-indents tolerance)
                ;; Indent width is 4 and tab width is 8
                (judge-indent-set-indent-tab-widths 4 8)
              ;; Indent width is 2 and tab width is 8
              (judge-indent-set-indent-tab-widths 2 8))))))))

(defun judge-indent-count-certain-indent (string pos1 pos2)
  "Count certain indent"
  (let ((count 0))
    (save-excursion
      (goto-char pos1)
      (while (and (search-forward
                   (concat "\n" string) judge-indent-search-limit t)
                  (<= (point) pos2))
        (unless (or (eobp)
                    (char-equal (char-after) ?\ )
                    (char-equal (char-after) ?\t)
                    (char-equal (char-after) ?\n)
                    (char-equal (char-after) ?\r))
          (incf count)))
      (goto-char pos1)
      (while (and (search-forward
                   (concat "\r" string) judge-indent-search-limit t)
                  (<= (point) pos2))
        (unless (or (eobp)
                    (char-equal (char-after) ?\ )
                    (char-equal (char-after) ?\t)
                    (char-equal (char-after) ?\n)
                    (char-equal (char-after) ?\r))
          (incf count))))
    count))

(defun judge-indent-count-indents (pos1 pos2)
  "Count indents"
  (setq judge-indent-count-one-tab-indents
        (judge-indent-count-certain-indent "\t" pos1 pos2))
  (setq judge-indent-count-two-space-indents
        (judge-indent-count-certain-indent "  " pos1 pos2))
  (setq judge-indent-count-four-space-indents
        (judge-indent-count-certain-indent "    " pos1 pos2))
  (setq judge-indent-count-eight-space-indents
        (judge-indent-count-certain-indent "        " pos1 pos2)))

(defun judge-indent-buffer ()
  "Judge indent and tab widths from buffer"
  (interactive)
  (judge-indent-count-indents (point-min) (point-max))
  (judge-indent-judge-from-indent-counts))

(defun judge-indent-region ()
  "Judge indent and tab widths from region"
  (interactive)
  (judge-indent-count-indents (region-beginning) (region-end))
  (judge-indent-judge-from-indent-counts))

(defun judge-indent-message-indent-counts ()
  "Message indent counts"
  (message (concat "One-tab: "
                   (number-to-string judge-indent-count-one-tab-indents)
                   "; two-space: "
                   (number-to-string judge-indent-count-two-space-indents)
                   "; four-space: "
                   (number-to-string judge-indent-count-four-space-indents)
                   "; eight-space: "
                   (number-to-string judge-indent-count-eight-space-indents))))

(defun judge-indent-message-indent-counts-buffer ()
  "Message indent counts of buffer"
  (interactive)
  (judge-indent-count-indents (point-min) (point-max))
  (judge-indent-message-indent-counts))

(defun judge-indent-message-indent-counts-region ()
  "Message indent counts of region"
  (interactive)
  (judge-indent-count-indents (region-beginning) (region-end))
  (judge-indent-message-indent-counts))

;; Minor mode

;;;###autoload
(define-minor-mode judge-indent-mode
  "Judge indent mode"
  :lighter " JI"
  :group   'judge-indent
  (if judge-indent-mode
      (progn
        (make-local-variable 'judge-indent-minor-indent-width)
        (make-local-variable 'judge-indent-minor-tab-width)
        (make-local-variable 'judge-indent-minor-mode-lighter)
        (judge-indent-buffer))
    (kill-local-variable 'judge-indent-minor-indent-width)
    (kill-local-variable 'judge-indent-minor-tab-width)
    (kill-local-variable 'judge-indent-minor-mode-lighter)))

;;;###autoload
(define-global-minor-mode global-judge-indent-mode
  judge-indent-mode
  (lambda ()
    (when (memq major-mode judge-indent-major-modes)
      (judge-indent-mode 1))))

(provide 'judge-indent)

;;; judge-indent.el ends here
