;;; seethru.el --- Easily change Emacs' transparency

;; Copyright (C) 2014 Benaiah Mischenko

;; Author: Benaiah Mischenko <benaiah@mischenko.com>
;; Maintainer: Benaiah Mischenko <benaiah@mischenko.com>
;; URL: http://github.com/benaiah/seethru
;; Package-Version: 20150218.1829
;; Created: 11th November 2014
;; Version: 0.3
;; Keywords: lisp, tools, alpha, transparency
;; Package-Requires: ((shadchen "1.4"))

;; This program is free software: you can redistribute it and/or modify
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
;; Seethru: Easily change Emacs' frame transparency
;;
;; The use of seethru is very simple. To set the transparency to an
;; absolute value:
;;
;;     (seethru 55)
;;
;; To set the transparency relative to its current value:
;;
;;     (seethru-relative -10)
;;
;; This is usually used to bind to a key, like so:
;;
;;     (global-set-key (kbd "<M-wheel-up>")
;;                     (lambda ()
;;                       (seethru-relative -10)))
;;
;; To set up recommended keybindings, which are `C-c 8' to reduce
;; transparency and `C-c 9' to increase it, as well as shifted
;; keybinds which do the same, but slower:
;;
;;     (seethru-recommended-keybinds)
;;
;; To set up mouse bindings, which are wheel-up to increase
;; transparency and wheel-down to decrease it:
;;
;;     (seethru-mouse-bindings)
;;
;; You can optionally change the modifier used by either
;; `seethru-recommended-keybinds' or `seethru-mouse-bindings' simply
;; by passing an argument in, for example:
;;
;;     (seethru-recommended-keybinds "C-x") ;; "C-x 8" and "C-x 9"
;;     (seethru-mouse-bindings "C") ;; hold control while wheeling
;;                                  ;; mouse to change transparency

;;; Code:

(require 'shadchen)

;;;###autoload
(defun seethru (value)
  "Sets the transparency of the currently selected Emacs
frame (0-100, where 0 is transparent and 100 is opaque)"
  (interactive "nSet transparency (0 is transparent - 100 is opaque)")
  (set-frame-parameter (selected-frame) 'alpha value))

;;;###autoload
(defun seethru-relative (value)
  (let* ((current-transparency
          (let ((alph (frame-parameter (selected-frame) 'alpha)))
            (if alph alph 100)))
         (summed-transparency (+ current-transparency value)))
    (seethru (match summed-transparency
               ((? (lambda (x) (< x 0)) x) 0)
               ((? (lambda (x) (> x 100)) x) 100)
               (val val)))))

;;;###autoload
(defun seethru-recommended-keybinds (&optional prefix)
  (let ((pre (or prefix "C-c")))
    (global-set-key (kbd (concat pre " 8"))
                    (lambda () (interactive) (seethru-relative 10)))
    (global-set-key (kbd (concat pre " *"))
                    (lambda () (interactive) (seethru-relative 5)))
    (global-set-key (kbd (concat pre " 9"))
                    (lambda () (interactive) (seethru-relative -10)))
    (global-set-key (kbd (concat pre " ("))
                    (lambda () (interactive) (seethru-relative -5)))))

;;;###autoload
(defun seethru-mouse-bindings (&optional prefix)
  (let ((pre (or prefix "M")))
    (global-set-key (kbd (concat "<" pre "-wheel-down>"))
                    (lambda () (interactive) (seethru-relative 1)))
    (global-set-key (kbd (concat "<" pre "-wheel-up>"))
                    (lambda () (interactive) (seethru-relative -1)))))

(provide 'seethru)

;;; seethru.el ends here
