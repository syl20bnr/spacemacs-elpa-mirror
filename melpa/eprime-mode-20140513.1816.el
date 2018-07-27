;;; eprime-mode.el --- An E-prime checking mode for Emacs

;; Copyright (C) 2014 Andrew Hynes

;; Filename: eprime-mode.el
;; Author: Andrew Hynes <andrewhynes@openmailbox.org>
;; URL: https://github.com/AndrewHynes/eprime-mode
;; Package-Version: 20140513.1816
;; Description: An E-prime checking mode for Emacs that highlights non-conforming text.
;; Version: 1.1.2
;; Keywords: E-prime, English, grammar

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; 
;; * About eprime-mode
;;
;; An E-prime checking mode for Emacs. 
;; Read more here - https://en.wikipedia.org/wiki/E-prime.
;; Naturally, all of this file that can, conforms to E'.
;;
;; Adds the following functionality: 
;;
;;   - M-x eprime-check-buffer to check the buffer
;; 
;;   - A minor mode, eprime-mode, which checks the buffer and
;;     any text you enter thereafter.
;; 
;;   - M-x eprime-remove corrections to remove its corrections
;; 
;;   - Customisable face for banned words. eprime-banned-words-face
;; 
;;   - M-x eprime-check-word to check only the current word
;;
;;   - Can customise banned words (by pushing onto eprime-baned-words)
;;
;;   - Default different face than FlySpell for ease of use together
;; 

(require 'cl)

(defvar eprime-ignore-case t
  "Defines whether eprime-mode should ignore case. Defaults to true.
  Set to \"nil\" if you want to turn this off.")

(defvar eprime-banned-words
  '("be" "being" "been" "am" "is" "isn't" "are" "aren't" "was" "wasn't" "were" "weren't"
    "I'm" "i'm" "you're" "we're" "they're" "he's" "she's" "it's" "there's" "here's"
    "where's" "how's" "what's" "who's" "what's" "ain't" "hain't" "whatcha" "yer")
  "The default banned words for eprime-mode, used by all of the functions in the mode.")

;;Note - FlySpell uses "OrangeRed" foreground
(defface eprime-banned-words-face
  '((((class color)) (:foreground "firebrick2" :weight bold :underline t))
      (t (:weight bold)))
  "Face used for marking a word banned by E-prime. For reference, FlySpell uses
  OrangeRed as its forground. The foreground for E' mode currently has the value \"firebrick2\"."
  :group 'eprime)

(defun eprime-check-thing (thing start)
  "Checks something returned by thing-at-point and corrects it if necessary.
  Whilst the variable eprime-ignore-case remains t, it will ignore case, else it won't.
  The default has the property of t."
  ;;can do it in less lines, but this way feels clearer to me
  (let ((end (point)))
    (if eprime-ignore-case
	(if (member (downcase thing) eprime-banned-words)
	    (let ((new-ov (make-overlay start end)))
	      (overlay-put new-ov 'face 'eprime-banned-words-face))
	  (remove-overlays start end))
      (if (member thing eprime-banned-words)
	  (let ((new-ov (make-overlay start end)))
	    (overlay-put new-ov 'face 'eprime-banned-words-face))
	(remove-overlays start end)))))

;;;###autoload
(defun eprime-check-buffer ()
  "Checks the current buffer for banned words and applies a face
   to them."
  (interactive)
  (let* ((orig-syntax (char-to-string (char-syntax ?'))))
    (modify-syntax-entry ?' "w")
    (unwind-protect
        (save-excursion
          (goto-char (point-min))
          (forward-word 1)
          (forward-word -1)
          (catch 'break
            (while (not (eobp))
              (let ((current (thing-at-point 'word))
                    (start-point-pos (point)))
                (forward-word 1)
                (eprime-check-thing current start-point-pos))
              (forward-word 1)
              (when (eobp) (throw 'break "Finished!"))
              (forward-word -1))))
      (modify-syntax-entry ?' orig-syntax))))

;;;###autoload
(defun eprime-check-word ()
  "Checks the word that's currently entering."
  (interactive)
  (let* ((orig-syntax (char-to-string (char-syntax ?'))))
  (save-excursion
    (forward-word -1)
    (let ((current (thing-at-point 'word))
	  (start-point-pos (point)))
      (forward-word 1)
      (eprime-check-thing current start-point-pos)))
  (modify-syntax-entry ?' orig-syntax)))

(defun eprime-update (beg end length)
  "Scans around where the user types and informs if incorrect.
  Intended to invoke as the user types."
  (if (<= length 1)
      (eprime-check-word)
    (save-excursion
      (while (> (point) beg)
	(forward-word -1))
      (while (< (point) end)
	(eprime-check-word)
	(forward-word 1)))))

;;;###autoload
(defun eprime-remove-corrections ()
  "Removes the overlayed corrections on words."
  (interactive)
  (remove-overlays))

(defun eprime-init ()
  "Initialises the mode."
  (eprime-check-buffer)
  (add-hook 'after-change-functions 'eprime-update)
  (modify-syntax-entry ?' "w"))

(defun eprime-cleanup (old-syntax)
  "Cleans up after the mode."
  (eprime-remove-corrections)
  (remove-hook 'after-change-functions 'eprime-update)
  (modify-syntax-entry ?' old-syntax))

;;;###autoload
(define-minor-mode eprime-mode
  "Minor mode for checking text conforms to E'. Change eprime-banned-words-face
  to change what banned words look like, and use (setq eprime-ignore-case nil) if you
  do not want it to match upper case words.
  (eprime-check-buffer), when invoked, can check a buffer without turning the mode on."
  :lighter " [E' Mode]"
  :init-value nil
  :keymap nil
  :global nil

  (let* ((orig-syntax (char-to-string (char-syntax ?'))))
    ;;the true = enabled, false = disabled
    (if eprime-mode 
	(eprime-init)
      (eprime-cleanup orig-syntax))))

(provide 'eprime-mode)

;;; eprime-mode.el ends here

