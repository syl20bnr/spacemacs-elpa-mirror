;;; bln-mode.el --- binary line navigation minor mode for cursor movement in long lines

;; Copyright (C) 2016  Maarten Grachten

;;; Author: Maarten Grachten
;;; Keywords: motion, location, cursor, convenience
;; Package-Version: 20170112.1327
;;; URL: https://github.com/mgrachten/bln-mode
;;; Version: 1.0.0

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
;;
;; Navigating the cursor across long lines of text by keyboard in Emacs can be
;; cumbersome, since commands like `forward-char', `backward-char',
;; `forward-word', and `backward-word' move sequentially, and potentially
;; require a lot of repeated executions to arrive at the desired position.  This
;; package provides the binary line navigation minor-mode (`bln-mode'), to
;; address this issue.  It defines the commands `bln-forward-half' and
;; `bln-backward-half', which allow for navigating from any position in a line to
;; any other position in that line by recursive binary subdivision.

;; For instance, if the cursor is at position K, invoking `bln-backward-half' will
;; move the cursor to position K/2. Successively invoking `bln-forward-half'
;; (without moving the cursor in between invocations) will move the cursor to
;; K/2 + K/4, whereas a second invocation of `bln-backward-half' would move the
;; cursor to K/2 - K/4.

;; Below is an illustration of how you can use binary line navigation to reach
;; character `e' at column 10 from character `b' at column 34 in four steps:
;;
;;                   ________________|     `bln-backward-half'
;;          ________|                      `bln-backward-half'
;;         |___                            `bln-forward-half'
;;            _|                           `bln-backward-half'
;; ..........e.......................b.....
;;
;; This approach requires at most log(N) invocations to move from any position
;; to any other position in a line of N characters.  Note that when you move in
;; the wrong direction---by mistakenly invoking `bln-backward-half' instead of
;; `bln-forward-half' or vice versa---you can interrupt the current binary
;; navigation sequence by moving the cursor away from its current position (for
;; example, by `forward-char'). You can then start the binary navigation again
;; from that cursor position.

;; By default the commands `bln-backward-half' and `bln-forward-half' are bound to M-[
;; and M-], respectively.  Depending on your keyboard layout, these keys may not
;; be very convenient.  For more convenient binary line navigation, you could
;; bind to more convenient keys, like M-j and M-k (at the expense of losing the
;; default bindings for `indent-new-comment-line', and `kill-sentence',
;; respectively):
;;
;; (global-set-key (kbd "M-j") 'bln-backward-half)
;; (global-set-key (kbd "M-k") 'bln-forward-half)

;;; Code:

(defvar bln-beg -1)
(defvar bln-end -1)
(defvar bln-prev-mid -1)

;;;###autoload
(defun bln-backward-half ()
  "This function is used in combination with `bln-forward-half' to provide binary line navigation (see `bln-mode')."
  (interactive)
  (if (/= bln-prev-mid (point))
      (setq bln-beg -1 bln-end -1)
    (setq bln-end bln-prev-mid))
  (if (< bln-beg 0) (setq bln-beg (line-beginning-position)
			  bln-end (point)))
  (setq bln-prev-mid (/ (+ bln-beg bln-end) 2))
  (goto-char bln-prev-mid))

;;;###autoload
(defun bln-forward-half ()
  "This function is used in combination with `bln-backward-half' to provide binary line navigation (see `bln-mode')."
  (interactive)
  (if (/= bln-prev-mid (point))
      (setq bln-beg -1 bln-end -1)
    (setq bln-beg bln-prev-mid))
  (if (< bln-end 0) (setq bln-beg (point)
			  bln-end (line-end-position)))
  (setq bln-prev-mid (/ (+ bln-beg bln-end ) 2))
  (goto-char bln-prev-mid))


(defvar bln-mode-map (make-sparse-keymap) "Keymap for bln-mode.")
(define-key bln-mode-map (kbd "M-]") 'bln-forward-half)
(define-key bln-mode-map (kbd "M-[") 'bln-backward-half)

;;;###autoload
(define-minor-mode bln-mode
  "Toggle binary line navigation mode.

Interactively with no argument, this command toggles the mode. A
positive prefix argument enables the mode, any other prefix
argument disables it. From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

Navigating the cursor across long lines of text by keyboard in
Emacs can be cumbersome, since commands like `forward-char',
`backward-char', `forward-word', and `backward-word' move the
cursor linearly, and potentially require a lot of repeated
executions to arrive at the desired position. `bln-mode'
addresses this issue. It defines the commands `bln-forward-half' and
`bln-backward-half' that allow for navigating from any position in a
line to any other position in that line by recursive binary
subdivision.

For instance, if the cursor is at position K, invoking
`bln-backward-half' will move the cursor to position
K/2. Successively invoking `bln-forward-half' will move the cursor to
K/2 + K/4, whereas a second invocation of `bln-backward-half' would
move the cursor to K/2 - K/4.

Below is an illustration of how you can use binary line navigation
to reach character `e' at column 10 from character `b' at column
34 in four steps:

                  ________________|     `bln-backward-half'
         ________|                      `bln-backward-half'
        |___                            `bln-forward-half'
           _|                           `bln-backward-half'
..........e.......................b.....

This approach requires at most log(N) invocations to move from
any position to any other position in a line of N
characters. Note that when you move in the wrong direction---by
mistakenly invoking `bln-backward-half' instead of `bln-forward-half' or
vice versa---you can interrupt the current binary navigation
sequence by moving the cursor away from its current position (for
example, by `forward-char'). You can then start the binary
navigation again from that cursor position.

By default the commands `bln-backward-half' and `bln-forward-half' are
bound to M-[ and M-], respectively.
"
  :lighter " bln"
  :global
  :keymap bln-mode-map
  :group 'bln
  )

(provide 'bln-mode)
;;; bln-mode.el ends here
