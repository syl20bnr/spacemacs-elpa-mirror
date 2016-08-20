;;; elwm.el --- Minimalistic window manager for emacs

;; Copyright (C) 2013 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 6 Apr 2013
;; Keywords: docs
;; Package-Version: 20150817.307
;; Version: 0.0.2
;; Package-Requires: ((dash "1.1.0"))
;; URL: https://github.com/Fuco1/elwm

;; This file is not part of GNU Emacs.

;;; License:

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

;; See github readme at https://github.com/Fuco1/elwm

;; This package works great in combination with `golden-ratio' package:
;; https://github.com/roman/golden-ratio.el

;; Layouts:

;; - tile, vertical, left

;; +-----------+-------------+
;; |           |      1      |
;; |           +-------------+
;; |  master   |      2      |
;; |           +-------------+
;; |           |      3      |
;; +-----------+-------------+

;; - tile, horizontal, top

;; +-------+---------+-------+
;; |   1   |    2    |   3   |
;; |       |         |       |
;; +-------+---------+-------+
;; |                         |
;; |         master          |
;; +-------------------------+

;; Windows in the master area can be again split in the same direction
;; as the stack windows, but only in one column.

;;; Code:

(require 'dash)

(require 'windmove)

(defvar elwm-current-layout 'tile-vertical-left
  "Current layout.")

(defvar elwm-layout-list '(
                           tile-vertical-left
                           tile-horizontal-top
                           )
  "List of available layouts.")

(defvar elwm-last-swap nil
  "Last transpose.

The format is (from to move-focus).")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility

(defun elwm--rotate-list (arg list)
  "Rotate a list ARG places to the right.

With ARG negative, rotate to the left."
  (let ((r list))
    (if (> arg 0)
        (while (> arg 0)
          (setq r (append (last r) (butlast r)))
          (setq arg (1- arg)))
      (while (< arg 0)
        (setq r (append (cdr r) (list (car r))))
        (setq arg (1+ arg))))
    r))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; various kinds of lists of windows

(defun elwm--window-list ()
  (window-list nil 0))

(defun elwm--sorted-window-list (&optional layout)
  "Return `window-list' sorted according to the LAYOUT.

The first master window is in car of returned list."
  (setq layout (or layout elwm-current-layout))
  (let ((sort-fn
         (cond
          ((eq layout 'tile-vertical-left)
           (lambda (x y)
             (let ((ex (window-edges x))
                   (ey (window-edges y)))
               (if (= (nth 0 ex) (nth 0 ey))
                   (< (nth 1 ex) (nth 1 ey))
                 (< (nth 0 ex) (nth 0 ey))))))
          ((eq layout 'tile-horizontal-top)
           (lambda (x y)
             (let ((ex (window-edges x))
                   (ey (window-edges y)))
               (if (= (nth 1 ex) (nth 1 ey))
                   (< (nth 0 ex) (nth 0 ey))
                 (> (nth 1 ex) (nth 1 ey)))))))))
    (sort (elwm--window-list) sort-fn)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; master/stack window/area stuff

;; TODO: extract the layout-specific stuff into "deflayout" macro.
(defun elwm--get-master-windows (&optional layout)
  "Return list of windows in the master area."
  (setq layout (or layout elwm-current-layout))
  (let* ((windows (elwm--sorted-window-list layout))
         (tw (car windows)))
    (cond
     ((eq layout 'tile-vertical-left)
      (--filter (= (nth 0 (window-edges it)) (nth 0 (window-edges tw))) windows))
     ((eq layout 'tile-horizontal-top)
      (--filter (= (nth 1 (window-edges it)) (nth 1 (window-edges tw))) windows)))))

(defun elwm--get-stack-windows (&optional layout)
  "Return list of windows in the stack area."
  (setq layout (or layout elwm-current-layout))
  (let* ((windows (elwm--sorted-window-list layout))
         (tw (car windows)))
    (cond
     ((eq layout 'tile-vertical-left)
      (--remove (= (nth 0 (window-edges it)) (nth 0 (window-edges tw))) windows))
     ((eq layout 'tile-horizontal-top)
      (--remove (= (nth 1 (window-edges it)) (nth 1 (window-edges tw))) windows)))))

(defun elwm--get-master-window (&optional layout arg)
  "Return the first window in the master area.

With optional argument ARG return ARGth master window."
  (nth (1- (or arg 1)) (elwm--get-master-windows layout)))

(defun elwm--in-master-area-p (&optional window layout)
  (setq window (or window (selected-window)))
  (member window (elwm--get-master-windows layout)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interactive functions

;;;###autoload
(defun elwm-rotate-window (&optional arg)
  "Rotate the windows clockwise.

With ARG positive, rotate the windows that many times.

With ARG negative, rotate the windows -ARG times in
counter-clockwise direction."
  (interactive "p")
  (let ((start-positions (elwm--rotate-list arg (mapcar 'window-start (elwm--window-list))))
        (buffers (elwm--rotate-list arg (mapcar 'window-buffer (elwm--window-list))))
        (windows (elwm--window-list)))
    (while windows
      (set-window-buffer (car windows) (car buffers))
      (set-window-start (car windows) (car start-positions))
      (!cdr start-positions)
      (!cdr buffers)
      (!cdr windows))))

;;;###autoload
(defun elwm-transpose-window (&optional arg)
  "Transpose the current window with the next one clockwise.

With ARG positive, repeat that many times.

With ARG negative, repeat -ARG times transposing in
counter-clockwise direction.

With ARG raw prefix argument \\[universal-argument], transpose
current window with first window in the master area.  If the
current window is in the master area, ask for a window from stack
area to be transposed here.

With ARG raw prefix \\[universal-argument] \\[universal-argument]
repeat the last swap."
  (interactive "P")
  (cond
   ((equal arg '(16))
    (let* ((to-window (nth 0 elwm-last-swap))
           (from-window (nth 1 elwm-last-swap))
           (to-buffer (window-buffer to-window))
           (from-buffer (window-buffer from-window)))
      (set-window-buffer from-window to-buffer)
      (set-window-buffer to-window from-buffer)
      (setq elwm-last-swap (list from-window to-window (nth 2 elwm-last-swap)))
      (when (nth 2 elwm-last-swap)
        (select-window to-window))))
   ((equal arg '(4))
    (if (elwm--in-master-area-p)
        (let* ((stack-windows (elwm--get-stack-windows))
               (this-buffer (window-buffer))
               (next-window (nth
                             (1- (string-to-number
                                  (ido-completing-read
                                   "Swap with window: "
                                   (mapcar 'int-to-string
                                           (number-sequence 1 (length stack-windows))))))
                             stack-windows))
               (next-buffer (window-buffer next-window)))
          (set-window-buffer (selected-window) next-buffer)
          (set-window-buffer next-window this-buffer)
          (setq elwm-last-swap (list (selected-window) next-window nil)))
      (let* ((master-window (elwm--get-master-window))
             (this-buffer (window-buffer))
             (next-buffer (window-buffer master-window)))
        (set-window-buffer (selected-window) next-buffer)
        (set-window-buffer master-window this-buffer)
        (setq elwm-last-swap (list (selected-window) master-window t))
        (select-window master-window))))
   (t
    (setq arg (prefix-numeric-value arg))
    (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
      (while (/= arg 0)
        (let ((this-buffer (window-buffer))
              (next-buffer (window-buffer (funcall selector))))
          (set-window-buffer (selected-window) next-buffer)
          (set-window-buffer (funcall selector) this-buffer)
          (setq elwm-last-swap (list (selected-window) (funcall selector) t))
          (select-window (funcall selector)))
        (setq arg (if (> arg 0) (1- arg) (1+ arg))))))))

;;;###autoload
(defun elwm-activate-window (&optional arg)
  "Select the next window clockwise.

With ARG positive, select ARGth next window

With ARG negative, select -ARGth previous window.

With ARG raw prefix argument \\[universal-argument], select first
window in the master area."
  (interactive "P")
  (cond
   ((equal arg '(4))
    (select-window (elwm--get-master-window)))
   (t (other-window (prefix-numeric-value arg)))))

;;;###autoload
(defun elwm-split-window (arg)
  "Split window according to the current layout.

Window in the master area can't be split, instead the last window
in the stack will be split.

If selected window is window on the stack, the new window will be
created next to it, according to the current layout.

With argument \\[universal-argument] ask for buffer to select in
the newly created window.

With argument \\[universal-argument] \\[universal-argument] ask
for file to open in the newly created window."
  (interactive "P")
  (let ((buf (cond
              ((equal arg '(4))
               (ido-read-buffer "Select buffer:" (current-buffer)))
              ((equal arg '(16))
               (find-file-noselect (ido-read-file-name "Open file:")))
              (t (current-buffer)))))
    (cond
     ((eq elwm-current-layout 'tile-vertical-left)
      (if (elwm--in-master-area-p)
          ;; split the last window on the stack instead
          (set-window-buffer
           (select-window (split-window (car (last (elwm--sorted-window-list))) nil nil))
           buf)
        (split-window nil nil nil)))
     ((eq elwm-current-layout 'tile-horizontal-top)
      (if (elwm--in-master-area-p)
          ;; split the last window on the stack instead
          (set-window-buffer
           (select-window (split-window (car (last (elwm--sorted-window-list))) nil t))
           buf)
        (split-window nil nil t))))))

;;;;; windmove stuff
(defun elwm-shift-internal (windmove-dir)
  "Internal shift routine.

WINDMOVE-DIR is the direction in which we want to shift."
  (let* ((windmove-wrap-around nil)
         (split-side-existing (pcase windmove-dir
                                (`down 'left)
                                (`up 'left)
                                (`left 'above)
                                (`right 'above)
                                (t (error "Invalid direction"))))
         (split-side-none (pcase windmove-dir
                            (`down 'below)
                            (`up 'above)
                            (`left 'left)
                            (`right 'right)
                            (t (error "Invalid direction"))))
         (this-window (selected-window))
         (other-window (windmove-find-other-window windmove-dir))
         (new-window (cond
                      ;; root split
                      ((or (window-minibuffer-p other-window)
                           (null other-window))
                       (split-window (frame-root-window) nil split-side-none))
                      ;; existing window is split
                      (t
                       (split-window other-window nil split-side-existing)))))
    (set-window-buffer new-window (window-buffer this-window))
    (delete-window this-window)
    (select-window new-window)
    ;; TODO: make this optional
    (balance-windows)))

;;;###autoload
(defun elwm-shift-down ()
  "Shift the current window down, spliting a horizontal split if present."
  (interactive)
  (elwm-shift-internal 'down))

;;;###autoload
(defun elwm-shift-up ()
  "Shift the current window up, spliting a horizontal split if present."
  (interactive)
  (elwm-shift-internal 'up))

;;;###autoload
(defun elwm-shift-right ()
  "Shift the current window right, spliting a horizontal split if present."
  (interactive)
  (elwm-shift-internal 'right))

;;;###autoload
(defun elwm-shift-left ()
  "Shift the current window left, spliting a horizontal split if present."
  (interactive)
  (elwm-shift-internal 'left))

(defun elwm-swap-internal (windmove-dir)
  "Swap current window's buffer with other window's buffer.

WINDMOVE-DIR is the direction in which we want to swap."
  (let* ((windmove-wrap-around nil)
         (this-window (selected-window))
         (other-window (windmove-find-other-window windmove-dir))
         (other-window-buffer (window-buffer other-window)))
    (set-window-buffer other-window (window-buffer this-window))
    (set-window-buffer this-window other-window-buffer)
    (select-window other-window)))

;;;###autoload
(defun elwm-swap-down ()
  "Swap the current window's buffer with one of the window below."
  (interactive)
  (elwm-swap-internal 'down))

;;;###autoload
(defun elwm-swap-up ()
  "Swap the current window's buffer with one of the window above."
  (interactive)
  (elwm-swap-internal 'up))

;;;###autoload
(defun elwm-swap-right ()
  "Swap the current window's buffer with one of the window to the right."
  (interactive)
  (elwm-swap-internal 'right))

;;;###autoload
(defun elwm-swap-left ()
  "Swap the current window's buffer with one of the window to the left."
  (interactive)
  (elwm-swap-internal 'left))

(provide 'elwm)

;;; elwm.el ends here
