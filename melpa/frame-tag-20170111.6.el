;;; frame-tag.el --- Minor mode that assigns a unique number to each frame for easy switching

;; Copyright (C) 2012 Wong Liang Zan

;; Version: 0.1.1
;; Package-Version: 20170111.6
;; Keywords: frame, movement
;; Author: Wong Liang Zan <zan@liangzan.net>
;; URL: http://github.com/liangzan/frame-tag.el
;; Package-Requires: ((cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Usage

;; frame-tag mode allows you to switch between frames quickly. Frames are orderd by their positions
;; The top left frame is assigned the number 1. The second frame is assigned 2 and so on.
;; To switch to the frames, press M-1 to switch to frame 1.
;; It assigns a maximum of 9 frames to switch from.

;;; Installation

;; (add-to-list 'load-path "/path/to/frame-tag")
;; (require 'frame-tag)
;; (frame-tag-mode 1)

;;; Code:

(require 'cl-lib)

(defun frame-tag-frame-left-position (frm)
  "Returns the value of the left parameter of the frame"
  (frame-parameter frm 'left))

(defun frame-tag-frame-top-position (frm)
  "Returns the value of the top parameter of the frame"
  (frame-parameter frm 'top))

(defun frame-tag-frame-relative-position (frm)
  "Adds the left and top parameter of the frames"
  (+ (eval (frame-tag-frame-left-position frm))
     (eval (frame-tag-frame-top-position frm))))

(defun frame-tag-frames-sorted-position ()
  "Returns the sorted relative positions of the frames"
  (sort
   (frame-tag-frames-relative-position)
   '<))

(defun frame-tag-frames-relative-position ()
  "Returns the relative positions of the frames"
  (mapcar 'frame-tag-frame-relative-position (frame-list)))

(defun frame-tag-find-index-position (index)
  "Given the index, returns the position from the sorted relative postions"
  (cl-position (nth index (frame-tag-frames-sorted-position))
               (frame-tag-frames-relative-position)))

(defun frame-tag-find-frame (index)
  "Finds the frame given the index"
  (nth (frame-tag-find-index-position index) (frame-list)))

(defun frame-tag-select-frame-by-number (index)
  "Selects the frame given a number."
  (if (<= index (- (length (frame-list)) 1))
      (select-frame-set-input-focus (frame-tag-find-frame index))
  (error "No frames found")))

;; define interactive functions for keymap
(dotimes (i 10)
  (eval `(defun ,(intern (format "frame-tag-select-frame-%s" i)) (&optional arg)
           ,(format "Select the frame with number %i." i)
           (interactive "P")
           (frame-tag-select-frame-by-number ,(- i 1)))))

(defvar frame-tag-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-1" 'frame-tag-select-frame-1)
    (define-key map "\M-2" 'frame-tag-select-frame-2)
    (define-key map "\M-3" 'frame-tag-select-frame-3)
    (define-key map "\M-4" 'frame-tag-select-frame-4)
    (define-key map "\M-5" 'frame-tag-select-frame-5)
    (define-key map "\M-6" 'frame-tag-select-frame-6)
    (define-key map "\M-7" 'frame-tag-select-frame-7)
    (define-key map "\M-8" 'frame-tag-select-frame-8)
    (define-key map "\M-9" 'frame-tag-select-frame-9)
    map)
  "Keymap used in by `frame-tag-mode'.")

;;;###autoload
(define-minor-mode frame-tag-mode
  "A minor mode that assigns a number to the frames by position order"
  nil nil frame-tag-keymap :global t)

(provide 'frame-tag)

;;; frame-tag.el ends here
