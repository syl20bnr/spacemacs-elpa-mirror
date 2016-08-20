;;; auto-complete-chunk.el --- Auto-completion for dot.separated.words.
;;
;; Filename: auto-complete-chunk.el
;; Description: Auto-completion for dot.separated.words.
;; Author: ARAKAKI, Takafumi
;; Maintainer: ARAKAKI, Takafumi
;; Created: Wed Dec 7 17:23:39 2011 +0100
;; Package-Requires: ((auto-complete "1.4"))
;; Package-Version: 20140225.146
;; Version: 0.1.0
;; URL: https://github.com/tkf/auto-complete-chunk
;;
;; This file is NOT part of GNU Emacs.

;;; License:
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

;;; Commentary:

;; Example usage:
;;
;;    (add-hook
;;     'python-mode
;;     (lambda ()
;;       ;; Make sure `ac-source-chunk-list' comes first.
;;       (setq ac-sources (append '(ac-source-chunk-list) ac-sources))
;;       (setq ac-chunk-list
;;             '("os.path.abspath" "os.path.altsep" "os.path.basename"))))

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'auto-complete)


;;; Core

(defvar ac-chunk-regex
  (rx (group (| (syntax whitespace)
                (syntax open-parenthesis)
                (syntax close-parenthesis)
                bol))
      (* (+ (| (syntax word) (syntax symbol)))
         (syntax punctuation))
      (+ (| (syntax word) (syntax symbol)))
      (? (syntax punctuation))
      point)
  "A regexp that matches to a \"chunk\" containing words and dots.")

(defun ac-chunk-beginning ()
  "Return the position where the chunk begins."
  (ignore-errors
    (save-excursion
      (+ (re-search-backward ac-chunk-regex) (length (match-string 1))))))

(defun ac-chunk-candidates-from-list (chunk-list)
  "Return matched candidates in CHUNK-LIST."
  (let* ((start (ac-chunk-beginning)))
    (when start
      (loop with prefix = (buffer-substring start (point))
            for cc in chunk-list
            when (string-prefix-p prefix cc)
            collect cc))))


;;; `ac-source-chunk-list'

(defvar ac-chunk-list nil
  "Dictionary used from `ac-source-chunk-list'.  List of strings.")
(make-variable-buffer-local 'ac-chunk-list)

(defun ac-chunk-list ()
  "Util function to access the variable `ac-chunk-list'."
  ac-chunk-list)

(defun ac-chunk-list-candidates ()
  "Create candidates from a buffer local variable `ac-chunk-list'."
  (ac-chunk-candidates-from-list ac-chunk-list))

(ac-define-source chunk-list
  '((candidates . ac-chunk-list-candidates)
    (prefix . ac-chunk-beginning)
    (symbol . "c")))


;;; `ac-source-dictionary-chunk'

(defun ac-dictionary-chunk-candidates ()
  "Create candidates from dictionary (variable `ac-buffer-dictionary')."
  (ac-chunk-candidates-from-list (ac-buffer-dictionary)))

(ac-define-source dictionary-chunk
  '((candidates . ac-dictionary-chunk-candidates)
    (prefix . ac-chunk-beginning)
    (symbol . "c")))

(defun ac-use-dictionary-chunk ()
  "Swap `ac-source-dictionary' with `ac-source-dictionary-chunk'."
  (setq ac-sources (delq 'ac-source-dictionary ac-sources))
  (add-to-list 'ac-sources 'ac-source-dictionary-chunk))

(provide 'auto-complete-chunk)

;;; auto-complete-chunk.el ends here
