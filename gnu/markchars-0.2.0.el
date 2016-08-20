;;; markchars.el --- Mark chars fitting certain characteristics
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Contributhor: Ted Zlatanov <tzz@lifelogs.com>
;; Created: 2010-03-22 Mon
;; Version: 0.2.0
;; Last-Updated: 2011-04-15
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that can be used by this library:
;;
;;   `idn'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Mark special chars, by default nonascii, non-IDN chars, in modes
;; where they may be confused with regular chars. See `markchars-mode'
;; and `markchars-what'.  There are two modes: confusable detection
;; (where we look for mixed scripts within a word, without using the
;; http://www.unicode.org/reports/tr39/ confusable tables) and pattern
;; detection (where any regular expressions can be matched).
;;
;; The marked text will have the 'markchars property set to either
;; 'confusable or 'pattern and the face set to either
;; `markchars-face-confusable' or `markchars-face-pattern'
;; respectively.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'idn nil t)

;;;###autoload
(defgroup markchars nil
  "Customization group for `markchars-mode'."
  :group 'convenience)

(defface markchars-light
  '((t (:underline "light blue")))
  "Light face for `markchars-mode' char marking."
  :group 'markchars)

(defface markchars-heavy
  '((t (:underline "magenta")))
  "Heavy face for `markchars-mode' char marking."
  :group 'markchars)

(defface markchars-white
  '((t (:underline "white")))
  "White face for `markchars-mode' char marking."
  :group 'markchars)

(defcustom markchars-face-pattern 'markchars-heavy
  "Pointer to face used for marking matched patterns."
  :type 'face
  :group 'markchars)

(defcustom markchars-face-confusable 'markchars-light
  "Pointer to face used for marking confusables."
  :type 'face
  :group 'markchars)

(defcustom markchars-face-nonidn 'markchars-white
  "Pointer to face used for marking non-IDN characters."
  :type 'face
  :group 'markchars)

(defcustom markchars-simple-pattern "[[:nonascii:]]+"
  "Regexp for characters to mark, a simple pattern.

By default it matches nonascii-chars."
  :type 'regexp
  :group 'markchars)

(defcustom markchars-what
  `(markchars-simple-pattern
    markchars-confusables
    ,@(when (fboundp 'idn-is-recommended) '(markchars-nonidn-fun)))
  "Things to mark, a list of regular expressions or symbols."
  :type `(repeat (choice :tag "Marking choices"
                         (const
                          :tag "Non IDN chars (Unicode.org tr39 suggestions)"
                          markchars-nonidn-fun)
                         (const :tag "Confusables" markchars-confusables)
                         (const :tag "`markchars-simple-pattern'"
                                markchars-simple-pattern)
                         (regexp :tag "Arbitrary pattern")))
  :group 'markchars)

(make-obsolete-variable 'markchars-keywords 'markchars-what "markchars.el 0.2")

(defvar markchars-used-keywords nil
  "Keywords for font lock.")
(put 'markchars-used-keywords 'permanent-local t)

(defun markchars-set-keywords ()
  "Set `markchars-used-keywords' from options."
  (set (make-local-variable 'markchars-used-keywords)
       (delq nil (mapcar (lambda (what)
                           (when (eq what 'markchars-simple-pattern)
                             (setq what markchars-simple-pattern))
                           (cond
                            ((eq what 'markchars-nonidn-fun)
                             (list
                              "\\<\\w+\\>"
                              (list 0 '(markchars--render-nonidn
                                        (match-beginning 0)
                                        (match-end 0)))))
                            ((eq what 'confusables)
                             (list
                              "\\<\\w+\\>"
                              (list 0 '(markchars--render-confusables
                                        (match-beginning 0)
                                        (match-end 0)))))
                            ((stringp what)
                             (list
                              what
                              (list 0 '(markchars--render-pattern
                                        (match-beginning 0)
                                        (match-end 0)))))))
                         markchars-what))))

(defun markchars--render-pattern (beg end)
  "Assign markchars pattern properties between BEG and END."
  (put-text-property beg end 'face markchars-face-pattern)
  (put-text-property beg end 'markchars 'pattern))

(defun markchars--render-confusables (beg end)
  "Assign markchars confusable properties between BEG and END."
  (let* ((text (buffer-substring-no-properties beg end))
         (scripts (mapcar
                  '(lambda (c) (aref char-script-table c))
                  (string-to-list text)))
         ;; `scripts-extra' is not nil is there was more than one script
         (scripts-extra (delq (car scripts) scripts)))
    (when scripts-extra
      (put-text-property beg end 'markchars 'confusable)
      (put-text-property beg end 'face markchars-face-confusable))))

(defun markchars--render-nonidn (beg end)
  "Assign markchars confusable properties between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (<= (point) end)
      (let ((c (char-after)))
        (when (and (> c 256)
                   (not (idn-is-recommended c)))
          (put-text-property (point) (1+ (point)) 'markchars 'nonidn)
          (put-text-property (point) (1+ (point)) 'face markchars-face-nonidn)))
      (forward-char))))

;;;###autoload
(define-minor-mode markchars-mode
  "Mark special characters.
Which characters to mark are defined by `markchars-pattern'.

The default is to mark nonascii chars with a magenta underline."
  :group 'markchars
  :lighter " Mchar"
  (if markchars-mode
      (progn
        (markchars-set-keywords)
        (let ((props (make-local-variable 'font-lock-extra-managed-props)))
          (add-to-list props 'markchars))
        (font-lock-add-keywords nil markchars-used-keywords))
    (font-lock-remove-keywords nil markchars-used-keywords))
  (font-lock-fontify-buffer))

;;;###autoload
(define-globalized-minor-mode markchars-global-mode markchars-mode
  (lambda () (markchars-mode 1))
  :group 'markchars)

;;;; ChangeLog:

;; 2012-07-18  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/markchars/markchars.el: Provide itself (bug#11915).
;; 
;; 2011-07-01  Chong Yidong  <cyd@stupidchicken.com>
;; 
;; 	Reorganize repository layout, allowing site installation.
;; 	
;; 	A Makefile with "site", "archive" and "archive-full" rules can now be
;; 	used for site-installation, partial archive deployment, and full
;; 	archive deployment respectively.
;; 	
;; 	Rewrite the admin/archive-contents.el script to handle these changes.
;; 


(provide 'markchars)
;;; markchars.el ends here
