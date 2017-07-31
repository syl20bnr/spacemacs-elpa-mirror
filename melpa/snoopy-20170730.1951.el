;;; snoopy.el --- minor mode for number row unshifted character insertion -*- Mode: Emacs-Lisp; lexical-binding: t -*-

;; Copyright (C) 2017 António Nuno Monteiro, Russell McQueeney

;; Author: António Nuno Monteiro <anmonteiro@gmail.com>
;; Version: 0.1.1
;; Package-Version: 20170730.1951
;; Package-Requires: ((emacs "24"))
;; Created: 2017-07-29
;; Keywords: lisp

;; Snoopy is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Snoopy is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with snoopy-mode.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; The currently released version of snoopy-mode is available at
;;;   <https://raw.githubusercontent.com/anmonteiro/snoopy-mode/v0.1.1/snoopy.el>
;;;
;;; The latest version of snoopy-mode is available at
;;;   <https://raw.githubusercontent.com/anmonteiro/snoopy-mode/master/snoopy.el>
;;;
;;; The Git repository for snoopy-mode is available at
;;;   <https://github.com/anmonteiro/snoopy-mode>
;;;
;;; Release notes are available at
;;;   <https://github.com/anmonteiro/snoopy-mode/blob/master/CHANGELOG.md>

;;; Install snoopy-mode by placing `snoopy.el' in `/path/to/elisp', a
;;; directory of your choice, and adding to your .emacs file:
;;;
;;;   (add-to-list 'load-path "/path/to/elisp")
;;;   (autoload 'snoopy-mode "snoopy"
;;;     "Turn on unshifted mode for characters in the keyboard number row."
;;;     t)
;;;
;;; Start Snoopy Mode on the fly with `M-x snoopy-mode RET',
;;; or always enable it in a major mode `M' (e.g., `lisp') with:
;;;
;;;   (add-hook 'M-mode-hook 'snoopy-mode)
;;;
;;; Customize snoopy-mode using `eval-after-load':
;;;
;;;   (eval-after-load 'snoopy
;;;     '(progn
;;;        (define-key snoopy-mode-map (kbd "1")
;;;          (lambda () (insert-char \! 1)))))
;;;
;;; Send questions, bug reports, comments, feature suggestions, &c.,
;;; via email to the author.
;;;

;;; The snoopy minor mode, Snoopy Mode, binds keys in the keyboard's number
;;; row, such as `1', `2', `3', etc, to commands that insert their shifted
;;; versions, e.g. `!', `@' and `#', respectively.
;;;

;;; Code:

(defcustom snoopy-enabled-in-prefix-arg nil
  "When non-nil, enable Snoopy Mode in prefix arguments."
  :group 'snoopy
  :type 'boolean)

(defun snoopy-insert-char (char)
  "Generate a function that will insert CHAR."
  (lambda ()
    (interactive)
    (insert-char char 1)))

(defvar snoopy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "1") (snoopy-insert-char ?!))
    (define-key map (kbd "2") (snoopy-insert-char ?@))
    (define-key map (kbd "3") (snoopy-insert-char ?#))
    (define-key map (kbd "4") (snoopy-insert-char ?$))
    (define-key map (kbd "5") (snoopy-insert-char ?%))
    (define-key map (kbd "6") (snoopy-insert-char ?^))
    (define-key map (kbd "7") (snoopy-insert-char ?&))
    (define-key map (kbd "8") (snoopy-insert-char ?*))
    (define-key input-decode-map (kbd "9") 'snoopy-insert-special)
    (define-key input-decode-map (kbd "0") 'snoopy-insert-special)
    (define-key map (kbd "!") (snoopy-insert-char ?1))
    (define-key map (kbd "@") (snoopy-insert-char ?2))
    (define-key map (kbd "#") (snoopy-insert-char ?3))
    (define-key map (kbd "$") (snoopy-insert-char ?4))
    (define-key map (kbd "%") (snoopy-insert-char ?5))
    (define-key map (kbd "^") (snoopy-insert-char ?6))
    (define-key map (kbd "&") (snoopy-insert-char ?7))
    (define-key map (kbd "*") (snoopy-insert-char ?8))
    (define-key input-decode-map (kbd "(") 'snoopy-insert-special)
    (define-key input-decode-map (kbd ")") 'snoopy-insert-special)
    (define-key map (kbd "<kp-1>") (snoopy-insert-char ?1))
    (define-key map (kbd "<kp-2>") (snoopy-insert-char ?2))
    (define-key map (kbd "<kp-3>") (snoopy-insert-char ?3))
    (define-key map (kbd "<kp-4>") (snoopy-insert-char ?4))
    (define-key map (kbd "<kp-5>") (snoopy-insert-char ?5))
    (define-key map (kbd "<kp-6>") (snoopy-insert-char ?6))
    (define-key map (kbd "<kp-7>") (snoopy-insert-char ?7))
    (define-key map (kbd "<kp-8>") (snoopy-insert-char ?8))
    (define-key map (kbd "<kp-9>") (snoopy-insert-char ?9))
    (define-key map (kbd "<kp-0>") (snoopy-insert-char ?0))
    map))

(defvar snoopy-lighter " Snoopy"
  "Mode line lighter for Snoopy Mode.")

;;;###autoload
(define-minor-mode snoopy-mode
  "Minor mode for number row unshifted character insertion.
With a prefix argument, enable Snoopy Mode.
\\<snoopy-mode-map>"
  :lighter snoopy-lighter
  :group 'snoopy
  :keymap snoopy-mode-map)

(defun snoopy-insert-special (_prompt)
  "Insert a special character.

This function is called for opening and
closing parentheses, `9' and `0', to make interaction with other minor
modes such as Paredit work."
  (let* ((cmd-ks (this-command-keys-vector))
         (len (length cmd-ks)))
    (if (and (= len 1)
             snoopy-mode
             (or (null prefix-arg)
                 snoopy-enabled-in-prefix-arg))
        (pcase (aref cmd-ks 0)
          (?9 (kbd "("))
          (?0 (kbd ")"))
          (?\( (kbd "9"))
          (?\) (kbd "0")))
      (vector (aref cmd-ks (1- len))))))

(provide 'snoopy)

;;; snoopy.el ends here
