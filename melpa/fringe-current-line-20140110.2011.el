;;; fringe-current-line.el --- show current line on the fringe.
;;
;; Copyright: (C) 2014 Kouhei Yanagita
;;
;; Author: Kouhei Yanagita <yanagi@shakenbu.org>
;; Github: http://github.com/kyanagi/fringe-current-line
;; URL: http://github.com/kyanagi/fringe-current-line/raw/master/fringe-current-line.el
;; Package-Version: 20140110.2011
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;
;;; Commentary:
;;
;; fringe-current-line is a package to indicate current line on the fringe.
;; You can use by the following steps.
;;
;; 1. Place this file on your load-path.
;;
;; 2. Add the following code into your init file:
;;
;;   (require 'fringe-current-line)
;;
;; 3. Activate the mode.
;;
;; * To enable it only in certain buffer, run `M-x fringe-current-line-mode'.
;;   Run again and you can disable it.
;;
;; * To enable it globally, add the following into your init file:
;;
;;     (global-fringe-current-line-mode 1)
;;
;;   You can toggle it by running `M-x global-fringe-current-line-mode'.
;;
;;
;; ## Limitation
;;
;; The indicator is not shown if the cursor is at the end of the buffer and the last line is empty.
;;
;;; Code:

(defvar fcl-fringe-bitmap 'vertical-bar
  "Bitmap to indicate current line.")

;;; internal variable
(defvar fcl-fringe-overlay nil
  "Hold an overlay for the fringe bitmap.")
(make-variable-buffer-local 'fcl-fringe-overlay)

(defun fcl-display-fringe-bitmap-at-current-line ()
  (let ((s "x")
        (point (point)))
    (fcl-clear-fringe-bitmap)
    ;; Somehow the cursor disappears at the end of the buffer if the last line is empty.
    ;; So don't display the bitmap on the fringe in that case.
    (unless (and (eobp) (bolp))
      (setq fcl-fringe-overlay (make-overlay point (1+ point)))
      (put-text-property 0 1 'display (list 'left-fringe fcl-fringe-bitmap) s)
      (overlay-put fcl-fringe-overlay 'before-string s))))

(defun fcl-clear-fringe-bitmap ()
  (when fcl-fringe-overlay
    (delete-overlay fcl-fringe-overlay)
    (setq fcl-fringe-overlay nil)))

(defun fringe-current-line-mode-on ()
  (add-hook 'pre-command-hook 'fcl-clear-fringe-bitmap)
  (add-hook 'post-command-hook 'fcl-display-fringe-bitmap-at-current-line nil t)
  )

(defun fringe-current-line-mode-off ()
  (fcl-clear-fringe-bitmap)
  (remove-hook 'pre-command-hook 'fcl-clear-fringe-bitmap t)
  (remove-hook 'post-command-hook 'fcl-display-fringe-bitmap-at-current-line t)
  )

(define-minor-mode fringe-current-line-mode
  "Indicate current line on the fringe."
  :global nil
  (if fringe-current-line-mode
      (fringe-current-line-mode-on)
    (fringe-current-line-mode-off)))

(define-global-minor-mode global-fringe-current-line-mode
  fringe-current-line-mode
  (lambda ()
    (unless (minibufferp)
      (fringe-current-line-mode 1))))

(provide 'fringe-current-line)

;;; fringe-current-line.el ends here
