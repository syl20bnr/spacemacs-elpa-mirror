;;; evil-avy.el --- set-based completion -*- lexical-binding: t -*-

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Yufan Lou <loganlyf@gmail.com>
;; URL: https://github.com/louy2/evil-avy
;; Package-Version: 20150908.48
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1") (cl-lib "0.5") (avy "0.3.0") (evil "1.2.3"))
;; Keywords: point, location, evil, vim

;; This file is part of GNU Emacs.

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
;; This package provides keybindings of avy in evil (vim) format.

;;; Code:
(require 'avy)
(require 'evil)

(defun avy-forward-char-in-line (char &optional back)
  "Jump forward to the currently visible CHAR in the current line.
If BACK is t, jump backward."
  (interactive (list (read-char "char: " t)))

  (let ((avy-all-windows nil))
    (avy-with avy-goto-char
      (avy--process
       (save-restriction
         (if (null back)
             (narrow-to-region (point)
                               (line-end-position))
           (narrow-to-region (line-beginning-position)
                             (point))
           )
         (avy--regex-candidates (regexp-quote (string char))))
       (avy--style-fn avy-style)))))

(evil-define-motion evil-avy-find-char (count char)
  "Use avy to move forward to char in line."
  :jump t
  :type inclusive
  (interactive "<c><C>")
  (if (null count) (avy-forward-char-in-line char)
    (evil-find-char count char)))

(evil-define-motion evil-avy-find-char-to (count char)
  "Use avy to move till char in line"
  :jump t
  :type inclusive
  (interactive "<c><C>")
  (if (null count)
      (progn
        (avy-forward-char-in-line char)
        (backward-char))
    (evil-find-char-to count char)))

(evil-define-motion evil-avy-find-char-backward (count char)
  "Use avy to move backward to char in line."
  :jump t
  :type exclusive
  (interactive "<c><C>")
  (if (null count)
      (avy-forward-char-in-line char t)
    (evil-find-char-backward count char)))

(evil-define-motion evil-avy-find-char-to-backward (count char)
  "Use avy to move backward till char in line."
  :jump t
  :type exclusive
  (interactive "<c><C>")
  (if (null count)
      (progn
        (avy-forward-char-in-line char t)
        (forward-char))
    (evil-find-char-to-backward count char)))

;; Replace motions

(evil-define-key 'normal evil-avy-mode-map
  "f" 'evil-avy-find-char
  "F" 'evil-avy-find-char-backward
  "t" 'evil-avy-find-char-to
  "T" 'evil-avy-find-char-to-backward
  )

(evil-define-key 'operator evil-avy-mode-map
  "f" 'evil-avy-find-char
  "F" 'evil-avy-find-char-backward
  "t" 'evil-avy-find-char-to
  "T" 'evil-avy-find-char-to-backward
  )

(evil-define-key 'visual evil-avy-mode-map
  "f" 'evil-avy-find-char
  "F" 'evil-avy-find-char-backward
  "t" 'evil-avy-find-char-to
  "T" 'evil-avy-find-char-to-backward
  )

(evil-define-key 'motion evil-avy-mode-map
  "f" 'evil-avy-find-char
  "F" 'evil-avy-find-char-backward
  "t" 'evil-avy-find-char-to
  "T" 'evil-avy-find-char-to-backward
  )

;;;###autoload
(define-minor-mode evil-avy-mode
  "Toggle evil-avy-mode.
Interactively with no argument, this command toggles the mode. A
positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode,`toggle' toggles the state.

When evil-avy-mode is active, it replaces some the normal, visual, operator
and motion state keybindings to invoke avy commands."

  :init-value nil
  :lighter nil
  :keymap (make-sparse-keymap)
  :global t
  :group 'avy)

(provide 'evil-avy)
;;; evil-avy.el ends here
