;;; erc-scrolltoplace.el --- An Erc module to scroll to bottom better -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jay Kamat
;; Author: Jay Kamat <jaygkamat@gmail.com>
;; Version: 0.0.1
;; Package-Version: 20171102.2213
;; Keywords: erc, module, comm, scrolltobottom, keep-place
;; URL: http://github.com/jgkamat/erc-scrolltoplace
;; Package-Requires: ((emacs "24.0") (switch-buffer-functions "0.0.1"))

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

;;; Commentary:
;; erc-scrolltoplace provices an erc module called 'scrolltoplace' which
;; tries to keep as many messages visible as possible in ERC while never moving
;; point.
;;
;; Add erc-scrolltoplace to `erc-modules' and run `erc-update-modules' to enable this!

;;; Constants:

;;; Code:
;;;; Dependencies:
(require 'erc)
(require 'switch-buffer-functions)

;;;; Module Definition:
(define-erc-module scrolltoplace nil
  "Leave point above un-viewed text in other channels."
  ((add-hook 'erc-insert-post-hook 'erc-scroll-to-place)
   (add-hook 'switch-buffer-functions 'erc--scroll-to-place-check-erc))
  ((remove-hook 'erc-insert-post-hook 'erc-scroll-to-place)
   (remove-hook 'switch-buffer-functions 'erc--scroll-to-place-check-erc)))

;;;; Functions:
(defun erc--scroll-to-place-check-erc (_from _to)
  "Run `erc-scroll-to-place' if we are switching to an erc buffer."
  (when (eq major-mode 'erc-mode)
    (erc-scroll-to-place)))

(defun erc-scroll-to-place ()
  "Recenter WINDOW so that `point' is visible, but we can see as much conversation as possible."

  ;; Temporarily bind resize-mini-windows to nil so that users who have it
  ;; set to a non-nil value will not suffer from premature minibuffer
  ;; shrinkage due to the below recenter call.  I have no idea why this
  ;; works, but it solves the problem, and has no negative side effects.
  ;; (Fran Litterio, 2003/01/07)
  (let ((resize-mini-windows nil))
    ;; Only run if current buffer is visible
    (when (get-buffer-window (current-buffer))
      (save-restriction
        (widen)
        ;; If window is visible but not focused, run as if we are there
        (with-selected-window (get-buffer-window (current-buffer))
          ;; Go to bottom, recenter to bottom is available, then restore point,
          ;; so we have to see it
          (save-excursion
            (goto-char (point-max))
            (recenter -1)))))))

;;;; Footer:
(provide 'erc-scrolltoplace)

;;; erc-scrolltoplace.el ends here
