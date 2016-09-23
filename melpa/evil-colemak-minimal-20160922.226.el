;;; evil-colemak-minimal.el --- Minimal Colemak key bindings for evil-mode

;; Author: Bryan Allred <bryan@revolvingcow.com>
;; Version: 1.0.0
;; Package-Version: 20160922.226
;; Package-Requires: ((emacs "24") (evil "1.2.12"))
;; Keywords: colemak evil
;; URL: https://github.com/bmallred/evil-colemak-minimal
;;
;; This file is not part of GNU Emacs.

;;; License:

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; This package provides minimal key rebindings for evil-mode with the
;; Colemak keyboard layout.  See the README for more information.
;;
;; To enable globally, use:
;;
;;   (global-evil-colemak-minimal-mode)
;;
;; To enable for just a single buffer, use:
;;
;;   (evil-colemak-minimal-mode)

;;; Code:

(require 'evil)

;;;###autoload
(define-minor-mode evil-colemak-minimal-mode
  "Minor mode with evil-mode enhancements for the Colemak keyboard layout."
  :keymap (make-sparse-keymap)
  :lighter " colemak")

;;;###autoload
(define-globalized-minor-mode global-evil-colemak-minimal-mode
  evil-colemak-minimal-mode
  (lambda () (evil-colemak-minimal-mode t))
  "Global minor mode with evil-mode enhancements for the Colemak keyboard layout.")

;; Helper functions to set maps
(defun evil-colemak-minimal-set-for-all (key def &optional maps)
  (unless maps
    (setq maps (list 'normal
                     'visual
                     'insert
                     'emacs
                     'motion)))
  (while maps
    (evil-define-minor-mode-key (pop maps) 'evil-colemak-minimal-mode key def)))

(defun evil-colemak-minimal-set-for-all-but-insert (key def)
  (evil-colemak-minimal-set-for-all key def (list 'normal
                                                  'visual
                                                  'emacs
                                                  'motion)))

(defun evil-colemak-minimal-set-for-all-but-insert-and-motion (key def)
  (evil-colemak-minimal-set-for-all key def (list 'normal
                                                  'visual
                                                  'emacs)))

(defun evil-colemak-minimal-set-for-normal (key def)
  (evil-colemak-minimal-set-for-all key def (list 'normal)))

;; Major keys which were replaced
(defconst evil-colemak-minimal-left "s"
  "Movement left normally mapped to 'h'")
(defconst evil-colemak-minimal-down "n"
  "Movement down normally mapped to 'j'")
(defconst evil-colemak-minimal-up "e"
  "Movement up normally mapped to 'k'")
(defconst evil-colemak-minimal-right "t"
  "Movement right normally mapped to 'l'")
(defconst evil-colemak-minimal-next "h"
  "Movement to the next/previous search result normally mapped to 'n'")
(defconst evil-colemak-minimal-end "l"
  "Movement to the end of a word normally mapped to 'e'")
(defconst evil-colemak-minimal-towards "k"
  "Movement towards a character normally mapped to 't'")

;; Left, Down, Up, Right
(evil-colemak-minimal-set-for-all-but-insert evil-colemak-minimal-left 'evil-backward-char)
(evil-colemak-minimal-set-for-all-but-insert evil-colemak-minimal-down 'evil-next-line)
(evil-colemak-minimal-set-for-all-but-insert evil-colemak-minimal-up 'evil-previous-line)
(evil-colemak-minimal-set-for-all-but-insert evil-colemak-minimal-right 'evil-forward-char)

;; Window handling
;; C-w (not C-r as in Shai's mappings) prefixes window commands
(define-key evil-window-map evil-colemak-minimal-left 'evil-window-left)
(define-key evil-window-map (upcase evil-colemak-minimal-left) 'evil-window-move-far-left)
(define-key evil-window-map evil-colemak-minimal-down 'evil-window-down)
(define-key evil-window-map (upcase evil-colemak-minimal-down) 'evil-window-move-very-bottom)
(define-key evil-window-map evil-colemak-minimal-up 'evil-window-up)
(define-key evil-window-map (upcase evil-colemak-minimal-up) 'evil-window-move-very-top)
(define-key evil-window-map evil-colemak-minimal-right 'evil-window-right)
(define-key evil-window-map (upcase evil-colemak-minimal-right) 'evil-window-move-far-right)

;; (H)op to next/previous search
(evil-colemak-minimal-set-for-all-but-insert evil-colemak-minimal-next 'evil-search-next)
(evil-colemak-minimal-set-for-all-but-insert (upcase evil-colemak-minimal-next) 'evil-search-previous)

;; (L)ast character in word/WORD
(evil-colemak-minimal-set-for-all-but-insert evil-colemak-minimal-end 'evil-forward-word-end)
(evil-colemak-minimal-set-for-all-but-insert (upcase evil-colemak-minimal-end) 'evil-forward-WORD-end)

;; (K)in of character
(evil-colemak-minimal-set-for-all-but-insert evil-colemak-minimal-towards 'evil-find-char-to)
(evil-colemak-minimal-set-for-all-but-insert (upcase evil-colemak-minimal-towards) 'evil-find-char-to-backward)

;; Code folding
(evil-colemak-minimal-set-for-normal (kbd "zM") 'hs-hide-all)
(evil-colemak-minimal-set-for-normal (kbd "zm") 'hs-hide-level)
(evil-colemak-minimal-set-for-normal (kbd "zR") 'hs-show-all)
(evil-colemak-minimal-set-for-normal (kbd "zr") 'hs-show-block)

(provide 'evil-colemak-minimal)

;;; evil-colemak-minimal.el ends here
