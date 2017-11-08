;;; vigenere.el --- Run a vigenere cipher on a block of text ; -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: Ian Dunn <dunni@gnu.org>
;; Maintainer: Ian Dunn <dunni@gnu.org>
;; Keywords: data, vigenere, cipher
;; URL: https://elpa.gnu.org/packages/vigenere.html
;; Package-Requires: ((emacs "25.1"))
;; Version: 1.0

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A vigenere cipher is similar to a series of shift ciphers.  A key is repeated
;; through the plain text, then a shift cipher is used for each letter depending
;; on the letter in the key at that position.

;; Example:

;; The following uses the key EMACS (because what else would my example key be?).

;; Plain text: This is some plain text

;; First, we repeat the key through the plain text

;; This is some plain text
;; EMAC SE MACS EMACS EMAC

;; Then each letter in the plain text is shifted based on the key character at
;; that point:

;; This is some plain text
;; EMAC SE MACS EMACS EMAC
;; Xtiu aw eoow txakf xqxv

;; The specific shift amount for each character is the 0-based offset from A.
;; Therefore, A is 0, B is 1, etc.

;; The key in our case is case-insensitive.  EMACS is Emacs is emacs is EmACs.

;; One more note, is that this package only operates on letters, and the case of
;; the original text is preserved.

;; A vigenere cipher should not be considered cryptographically secure.  This
;; package is for recreational use only, not for securing sensitive information.

;;; Code:

(defun vigenere-process-region (key begin end enc-or-dec)
  "Process the region from BEGIN to END in the current buffer using KEY.

ENC-OR-DEC is either 'encrypt or 'decrypt, which determines the
mode of operation.

KEY is case-insensitive, and case is preserved in the original
text."
  (save-excursion
    (goto-char begin)
    (let ((key-pos 0)
          (key-len (length key))
          (op (if (eq enc-or-dec 'encrypt) '+ '-)))
      ;; Only operate on letters.
      (while (re-search-forward "[[:alpha:]]" end t)
        (let* ((c (- (string-to-char (downcase (match-string-no-properties 0))) ?a))
               (k (- (string-to-char (downcase (substring key key-pos))) ?a))
               (d (char-to-string (+ (mod (funcall op c k) 26) ?a)))
               (case-fold-search nil))
          (replace-match d))
        ;; Update position in key
        (setq key-pos (mod (1+ key-pos) key-len))))))

;;;###autoload
(defun vigenere-decrypt-region (key begin end)
  "Decrypt region from BEGIN to END with a Vigenere cipher using KEY.

Note: A Vigenere cipher should not be considered
cryptographically secure."
  (interactive "sKey:\nr")
  (vigenere-process-region key begin end 'decrypt))

;;;###autoload
(defun vigenere-encrypt-region (key begin end)
  "Encrypt region from BEGIN to END with a Vigenere cipher using KEY.

Note: A Vigenere cipher should not be considered
cryptographically secure."
  (interactive "sKey:\nr")
  (vigenere-process-region key begin end 'encrypt))

;;;###autoload
(defun vigenere-decrypt-buffer (key)
  "Decrypt the current buffer with a Vigenere cipher using KEY.

Note: A Vigenere cipher should not be considered
cryptographically secure."
  (interactive "sKey: ")
  (vigenere-decrypt-region key (point-min) (point-max)))

;;;###autoload
(defun vigenere-encrypt-buffer (key)
  "Encrypt the current buffer with a Vigenere cipher using KEY.

Note: A Vigenere cipher should not be considered
cryptographically secure."
  (interactive "sKey: ")
  (vigenere-encrypt-region key (point-min) (point-max)))

(defun vigenere-decrypt-string (key string)
  "Decrypt STRING with a Vigenere cipher using KEY.

Note: A Vigenere cipher should not be considered
cryptographically secure."
  (with-temp-buffer
    (insert string)
    (vigenere-decrypt-buffer key)
    (buffer-string)))

(defun vigenere-encrypt-string (key string)
  "Encrypt STRING with a Vigenere cipher using KEY.

Note: A Vigenere cipher should not be considered
cryptographically secure."
  (with-temp-buffer
    (insert string)
    (vigenere-encrypt-buffer key)
    (buffer-string)))

;;;; ChangeLog:

;; 2017-08-31  Ian Dunn  <dunni@gnu.org>
;; 
;; 	Added vigenere package
;; 
;; 	Small package to run a Vigenere cipher on a block of text.
;; 
;; 	* packages/vigenere/vigenere.el: Added.
;; 


(provide 'vigenere)

;;; vigenere.el ends here
