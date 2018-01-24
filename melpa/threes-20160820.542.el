;;; threes.el --- A clone of Threes (a tiny puzzle game)  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang.me@gmail.com>
;; Package-Requires: ((emacs "24") (seq "1.11"))
;; Package-Version: 20160820.542
;; Keywords: games
;; URL: https://github.com/xuchunyang/threes.el

;; This file is not part of GNU Emacs

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

;; To play, type M-x threes, then use the arrow keys to move.

;;; Code:

(require 'seq)

(defconst threes-buffer-name "*Threes*" "Name used for Threes buffer.")

(defgroup threes nil
  "A little puzzle game."
  :group 'games
  :prefix "threes-")

(defface threes-face-0
  '((t . (:background "#bcd7d8")))
  "Face for the empty title."
  :group 'threes)

(defface threes-face-1
  '((t . (:background "#79ccfc" :foreground "white")))
  "Face for the tile 1."
  :group 'threes)

(defface threes-face-2
  '((t . (:background "#ef7986" :foreground "white")))
  "Face for tile 2."
  :group 'threes)

(defface threes-face-3
  '((t . (:background "white" :foreground "black")))
  "Face for tile 3."
  :group 'threes)

(defface threes-face-max
  '((t . (:background "white" :foreground "red")))
  "Face for maximum tile."
  :group 'threes)

(defvar threes-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map [left]  #'threes-left)
    (define-key map [up]    #'threes-up)
    (define-key map [right] #'threes-right)
    (define-key map [down]  #'threes-down)
    map))

(define-derived-mode threes-mode special-mode "threes-mode"
  "A mode for play Threes."
  (setq show-trailing-whitespace nil)
  (buffer-disable-undo))

(defun threes-string-center (len s)
  (let* ((s-len (length s))
         (leading (/ (- len s-len) 2))
         (tailing (- (- len s-len) leading)))
    (concat (make-string leading ?\s)
            s
            (make-string tailing ?\s))))

(defvar threes-cells nil)

(defvar threes-cells-last nil)

(defvar threes-next-number nil)

(defvar threes-game-over-p nil)

(defun threes-cells-max ()
  (apply #'max (apply #'append threes-cells)))

(defun threes-cells-score ()
  (let ((sum 0))
    (dotimes (i 4)
      (let ((row (nth i threes-cells)))
        (dotimes (j 4)
          (let ((val (nth j row)))
            (when (> val 2)
              (setq sum (+ sum (expt 3 (+ 1 (truncate (log (/ val 3) 2)))))))))))
    sum))

(defun threes-cells-transpose (cells)
  (let (res
        (l0 (nth 0 cells))
        (l1 (nth 1 cells))
        (l2 (nth 2 cells))
        (l3 (nth 3 cells)))
    (dotimes (i 4)
      (push (list (nth i l0)
                  (nth i l1)
                  (nth i l2)
                  (nth i l3))
            res))
    (reverse res)))

(defun threes-add-p (n1 n2)
  (or (and (= 1 n1) (= 2 n2))
      (and (= 1 n2) (= 2 n1))
      (and (> n1 2) (= n1 n2))
      (and (= n1 0) (/= n2 0))))

(defun threes-move-1 (nums)
  "Move NUMS left/up."
  (let (j a b)
    (catch 'found-move
      (dotimes (i 3)
        (setq j (+ i 1))
        (setq a (nth i nums)
              b (nth j nums))
        (when (threes-add-p a b)
          (throw 'found-move
                 (append (seq-subseq nums 0 i)
                         (list (+ a b))
                         (seq-subseq nums (+ j 1) 4)
                         (list 0)))))
      nums)))

(defun threes-move-2 (nums)
  "Move NUMS right/down."
  (reverse (threes-move-1 (reverse nums))))

(defun threes-check-before-move ()
  (when threes-game-over-p
    (error "Game Over")))

(defun threes-check-after-move ()
  (unless (catch 'found-add
            (let (col row)
              ;; Left
              (dotimes (i 4)
                (setq row (nth i threes-cells))
                (unless (equal row (threes-move-1 row))
                  (throw 'found-add t)))
              ;; Right
              (dotimes (i 4)
                (setq row (nth i threes-cells))
                (unless (equal row (threes-move-2 row))
                  (throw 'found-add t)))
              ;; Up
              (dotimes (i 4)
                (setq col (nth i (threes-cells-transpose threes-cells)))
                (unless (equal col (threes-move-1 col))
                  (throw 'found-add t)))
              ;; Down
              (dotimes (i 4)
                (setq col (nth i (threes-cells-transpose threes-cells)))
                (unless (equal col (threes-move-2 col))
                  (throw 'found-add t)))))
    (setq threes-game-over-p t)
    (message "Game Over")))

(defun threes-left ()
  (interactive)
  (threes-check-before-move)
  (let (new-cells)
    (dotimes (i 4)
      (push (threes-move-1 (nth i threes-cells)) new-cells))
    (setq threes-cells-last threes-cells
          threes-cells      (nreverse new-cells))

    ;; Insert a new cell
    (let* ((trans-cells (threes-cells-transpose threes-cells))
           (last-col (nth 3 trans-cells))
           (empty-pos '()))
      (dotimes (i 4)
        (when (zerop (nth i last-col))
          (push i empty-pos)))
      (if (zerop (length empty-pos))
          (message "Can't Move")
        (setcar (nthcdr (nth (random (length empty-pos)) empty-pos) last-col)
                (prog1 threes-next-number
                  (setq threes-next-number (+ 1 (random 3)))))
        ;; (message "%s" (threes-cells-transpose trans-cells))
        (setq threes-cells (threes-cells-transpose trans-cells))))

    (threes-check-after-move)
    (threes-print-board)))

(defun threes-right ()
  (interactive)
  (threes-check-before-move)
  (let (new-cells)
    (dotimes (i 4)
      (push (threes-move-2 (nth i threes-cells)) new-cells))
    (setq threes-cells-last threes-cells
          threes-cells      (nreverse new-cells))

    ;; Insert a new cell
    (let* ((trans-cells (threes-cells-transpose threes-cells))
           (last-col (nth 0 trans-cells))
           (empty-pos '()))
      (dotimes (i 4)
        (when (zerop (nth i last-col))
          (push i empty-pos)))
      (if (zerop (length empty-pos))
          (message "Can't Move")
        (setcar (nthcdr (nth (random (length empty-pos)) empty-pos) last-col)
                (prog1 threes-next-number
                  (setq threes-next-number (+ 1 (random 3)))))
        ;; (message "%s" (threes-cells-transpose trans-cells))
        (setq threes-cells (threes-cells-transpose trans-cells))))

    (threes-check-after-move)
    (threes-print-board)))

(defun threes-up ()
  (interactive)
  (threes-check-before-move)
  (let (new-cells)
    (dotimes (i 4)
      (push (threes-move-1 (nth i (threes-cells-transpose threes-cells))) new-cells))
    (setq threes-cells-last threes-cells
          threes-cells  (threes-cells-transpose (nreverse new-cells)))

    ;; Insert a new cell
    (let* ((last-row (nth 3 threes-cells))
           (empty-pos '()))
      (dotimes (i 4)
        (when (zerop (nth i last-row))
          (push i empty-pos)))
      (if (zerop (length empty-pos))
          (message "Can't Move")
        (setcar (nthcdr (nth (random (length empty-pos)) empty-pos) last-row)
                (prog1 threes-next-number
                  (setq threes-next-number (+ 1 (random 3)))))))

    (threes-check-after-move)
    (threes-print-board)))

(defun threes-down ()
  (interactive)
  (threes-check-before-move)
  (let (new-cells)
    (dotimes (i 4)
      (push (threes-move-2 (nth i (threes-cells-transpose threes-cells))) new-cells))
    (setq threes-cells-last threes-cells
          threes-cells (threes-cells-transpose (nreverse new-cells)))

    ;; Insert a new cell
    (let* ((last-row (nth 0 threes-cells))
           (empty-pos '()))
      (dotimes (i 4)
        (when (zerop (nth i last-row))
          (push i empty-pos)))
      (if (zerop (length empty-pos))
          (message "Can't Move")
        (setcar (nthcdr (nth (random (length empty-pos)) empty-pos) last-row)
                (prog1 threes-next-number
                  (setq threes-next-number (+ 1 (random 3)))))))

    (threes-check-after-move)
    (threes-print-board)))

(defun threes-undo ()
  "Undo last move."
  (interactive)
  (setq threes-cells threes-cells-last)
  (threes-print-board))

(defun threes-print-board ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert
     "
+-----+-----+-----+-----+
|     |     |     |     |
|xxxxx|xxxxx|xxxxx|xxxxx|
|     |     |     |     |
+-----+-----+-----+-----+
|     |     |     |     |
|xxxxx|xxxxx|xxxxx|xxxxx|
|     |     |     |     |
+-----+-----+-----+-----+
|     |     |     |     |
|xxxxx|xxxxx|xxxxx|xxxxx|
|     |     |     |     |
+-----+-----+-----+-----+
|     |     |     |     |
|xxxxx|xxxxx|xxxxx|xxxxx|
|     |     |     |     |
+-----+-----+-----+-----+
")
    (goto-char 1)
    (delete-char 1)

    (dotimes (row 4)
      (dotimes (col 4)
        (search-forward "xxxxx" nil 'noerror)
        (let* ((val (nth col (nth row threes-cells)))
               (text (threes-string-center
                      (length "xxxxx")
                      (if (zerop val) "" (number-to-string val))))
               (face (or (cdr (assq val `((0 . threes-face-0)
                                          (1 . threes-face-1)
                                          (2 . threes-face-2)
                                          (3 . threes-face-3)
                                          (,(threes-cells-max) . threes-face-max))))
                         ;; Face for others numbers
                         (and (/= val 0) 'threes-face-3))))

          (when face
            (setq text (propertize text 'face face))
            (let* ((pt (point))
                   ;; 26 is the length of a line
                   (end1 (- pt 26)) (beg1 (- end1 5))
                   (end2 (+ pt 26)) (beg2 (- end2 5)))
              ;; (message ">>> (%s, %s)" beg1 end1)
              (save-excursion
                (delete-region beg1 end1)
                (goto-char beg1)
                (insert (propertize "     " 'face face)))
              (save-excursion
                (delete-region beg2 end2)
                (goto-char beg2)
                (insert (propertize "     " 'face face)))))
          (replace-match text))))

    (goto-char (point-max))

    (if threes-game-over-p
        (progn
          (insert "\n")
          (insert "Score: " (number-to-string (threes-cells-score)))
          (insert " [Game Over!]"))
      (insert "\n\n")
      (let* ((num threes-next-number)
             (face (intern (format "threes-face-%d" num))))
        (insert (format "      %s\n" (propertize "     " 'face face))
                (format "Next: %s\n" (propertize (threes-string-center 5 (number-to-string num)) 'face face))
                (format "      %s\n" (propertize "     " 'face face)))))

    (goto-char 1)))

;;;###autoload
(defun threes ()
  "Play the Threes game."
  (interactive)
  (switch-to-buffer threes-buffer-name)
  (threes-mode)
  (setq threes-cells
        (let ((l (make-list 16 0))
              visited pos)
          (while (< (length visited) 7)
            (setq pos (random 16))
            (unless (memq pos visited)
              (setcar (nthcdr pos l) (+ 1 (random 3)))
              (push pos visited)))
          (seq-partition l 4))
        ;; '((1 2 0 0)
        ;;   (2 0 0 3)
        ;;   (3 3 1 1)
        ;;   (0 3 0 6))
        )
  (setq threes-game-over-p nil)
  (setq threes-next-number (+ 1 (random 3)))
  (threes-print-board))

;;;###autoload
(define-key menu-bar-games-menu [threes] '(menu-item "Threes" threes :help "Play Threes"))

(provide 'threes)
;;; threes.el ends here
