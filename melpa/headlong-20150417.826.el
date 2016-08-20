;;; headlong.el --- reckless completion

;; Copyright (C) 2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/headlong
;; Package-Version: 20150417.826
;; Version: 0.1.0
;; Keywords: completion

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package, akin to `ido-mode', modifies the built-in Emacs
;; completion, except it takes it in a reckless direction: as you type
;; in each character, it exits the minibuffer as soon as there is a
;; unique candidate.  This leaves no room for error, but in return you
;; get more speed.  Bookmarks can benefit from this completion method
;; the most.

;;; Code:

(require 'bookmark)

(defmacro headlong-with (&rest forms)
  "Execute FORMS with completion headlong."
  `(if (window-minibuffer-p)
       (user-error "Already in minibuffer")
     (let ((minibuffer-local-must-match-map headlong-minibuffer-map)
           (completing-read-function 'completing-read-default))
       ,@forms)))

(defvar headlong-minibuffer-map
  (let ((map (copy-keymap minibuffer-local-must-match-map)))
    (define-key map [remap self-insert-command] 'headlong-self-insert-complete-and-exit)
    (define-key map "\C-i" (lambda ()
                             (interactive)
                             (minibuffer-complete)
                             (minibuffer-completion-help)))
    map)
  "Keymap for headlong minibuffer completion.")

(defun headlong-self-insert-complete-and-exit (n)
  "Insert the character you type and try to complete.

If this results in:
- zero candidates: remove char and show completions
- one candidate: immediately exit the minibuffer.

N is passed to `self-insert-command'."
  (interactive "p")
  (self-insert-command n)
  (let ((candidates (completion-all-sorted-completions)))
    (cond
      ((null candidates)
       (backward-delete-char-untabify 1)
       (minibuffer-complete))
      ((eq 1 (safe-length candidates))
       (minibuffer-complete-and-exit)))))

;;;###autoload
(defun headlong-bookmark-jump (bookmark)
  "Jump to BOOKMARK headlong."
  (interactive
   (list (headlong-with
          (completing-read "Jump to bookmark: " bookmark-alist nil t))))
  (ignore-errors
    (bookmark-jump bookmark)))

;;;###autoload
(defun headlong-bookmark-jump-other (bookmark)
  "Jump to BOOKMARK headlong in other window."
  (interactive
   (list (headlong-with
          (completing-read "Jump to bookmark: " bookmark-alist nil t))))
  (ignore-errors
    (bookmark-jump bookmark 'pop-to-buffer)))

;; This is just a proof-of-concept. `smex' is much better.
;;;###autoload
(defun headlong-M-x (prefixarg &optional command-name)
  "Wrap around `(execute-extended-command PREFIXARG COMMAND-NAME)'."
  (interactive (list current-prefix-arg
                     (headlong-with (read-extended-command))))
  (execute-extended-command prefixarg command-name))

(provide 'headlong)

;;; headlong.el ends here
