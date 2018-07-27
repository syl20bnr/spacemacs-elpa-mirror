;;; mo-vi-ment-mode.el --- Provide vi-like cursor movement that's easy on the fingers

;; Copyright (C) 2013 Ajay MT

;; Author: Ajay MT <ajay.tatachar@gmail.com> (http://ajaymt.github.com)
;; Keywords: convenience
;; Package-Version: 20131029.633
;; Version: 0.5

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.


;;; Code:

;; Variables
(defvar mo-vi-ment-jump-length 5
  "The number of characters/lines the point will jump when you call mo-vi-ment-jump-down/up/left/right.")

;; Functions
(defun mo-vi-ment-jump-down ()
  "Move the point down by `mo-vi-ment-jump-length' lines. Useful for scrolling quickly."
  (interactive)
  (goto-line (+ (line-number-at-pos) mo-vi-ment-jump-length)))

(defun mo-vi-ment-jump-up ()
  "Move the point up by `mo-vi-ment-jump-length' lines. Useful for scrolling quickly."
  (interactive)
  (goto-line (- (line-number-at-pos) mo-vi-ment-jump-length)))

(defun mo-vi-ment-jump-left ()
  "Move the point left by `mo-vi-ment-jump-length' characters. Useful for moving quickly."
  (interactive)
  (backward-char mo-vi-ment-jump-length))

(defun mo-vi-ment-jump-right ()
  "Move the point right by `mo-vi-ment-jump-length' characters. Useful for moving quickly."
  (interactive)
  (forward-char mo-vi-ment-jump-length))

;; Keymap
(defvar mo-vi-ment-mode-map (make-sparse-keymap) "mo-vi-ment-mode keymap")
(define-key mo-vi-ment-mode-map (kbd "M-j") 'backward-char)
(define-key mo-vi-ment-mode-map (kbd "M-;") 'forward-char)
(define-key mo-vi-ment-mode-map (kbd "M-k") 'next-line)
(define-key mo-vi-ment-mode-map (kbd "M-l") 'previous-line)
(define-key mo-vi-ment-mode-map (kbd "M-J") 'move-beginning-of-line)
(define-key mo-vi-ment-mode-map (kbd "M-:") 'move-end-of-line)
(define-key mo-vi-ment-mode-map (kbd "M-K") 'scroll-up-command)
(define-key mo-vi-ment-mode-map (kbd "M-L") 'scroll-down-command)
(define-key mo-vi-ment-mode-map (kbd "C-M-j") 'mo-vi-ment-jump-left)
(define-key mo-vi-ment-mode-map (kbd "C-M-;") 'mo-vi-ment-jump-right)
(define-key mo-vi-ment-mode-map (kbd "C-M-k") 'mo-vi-ment-jump-down)
(define-key mo-vi-ment-mode-map (kbd "C-M-l") 'mo-vi-ment-jump-up)

;;;###autoload
(define-minor-mode mo-vi-ment-mode
  ;; Description
  "Toggle Mo-vi-ment mode. 
Interactively with no argument, this command toggles mo-vi-ment mode.
A positive prefix argument enables the mode, any other prefix
argument disables it. From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When mo-vi-ment mode is enabled, the following key bindings are set -

    \\{mo-vi-ment-mode-map}"
  
  ;; Initial value
  nil
  
  ;; The indicator for the mode-line
  " mo-vi-ment"
  
  ;; Keybindings
  mo-vi-ment-mode-map
  
  ;; Globalize it
  :global t)

(provide 'mo-vi-ment-mode)

;;; mo-vi-ment-mode.el ends here
