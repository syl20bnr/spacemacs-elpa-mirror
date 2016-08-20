;;; delim-kill.el --- Kill text between delimiters.

;; Copyright (C) 2010 Thomas Kappler

;; Author: Thomas Kappler <tkappler@gmail.com>
;; Created: 2010-05
;; Keywords: convenience, languages
;; Package-Version: 20100516.2320
;; URL: <http://github.com/thomas11/delim-kill/tree/master>

;; This file is not part of GNU Emacs.

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

;; Kill text between two delimiters, preserving structure.

;; delim-kill.el contains a single convenience function for editing
;; structured data: delim-kill. Given two characters FROM and TO, it
;; kills the text between the first occurence of FROM before point and
;; the first occurence of TO after point. FROM and TO may be
;; identical.

;; If FROM and TO are not identical, the function preserves the
;; balance between the two characters: For each FROM that is
;; encountered while looking for TO, one additional TO is required;
;; and vice versa. For example, in "{ foo X{bar} baz }", with X being
;; point and "{" and "}" as delimiters, the text "{ foo {bar} baz }"
;; will be killed, not "{ foo {bar}".

;; delim-kill is useful in programming and in editing other files with
;; structure, such as CSV or JSON. In C-style langiuages, for
;; instance, you can use it to easily kill the {}-delimited block you
;; are currently in. In a CSV file you might kill the current field,
;; regardless of where point is.

;; delim-kill was inspired by Damian Conway's course "The productive
;; programmer". Thanks!

;;; Dependencies: none.

;;; Installation:
;; Put the file anywhere in your load path, (require 'delim-kill), and
;; bind the function to a key.

;;; History:
;; 2010-05:    First release.

;;; Code:
(defun delim-kill (from-char to-char orig-point save)
  "Kill the text between two characters, preserving balance.

Kills the text between the first occurence of FROM before point
and the first occurence of TO after point, including FROM and TO.

If FROM and TO are not identical, the function preserves the
balance between the two characters: For each FROM that is
encountered while looking for TO, one additional TO is required;
and vice versa. For example, in \"{ foo X{bar} baz }\", with X
being point and \"{\" and \"}\" as delimiters, the text \" foo
{bar} baz \" will be killed, not \"bar} baz \".

If FROM and TO are identical, and point is on that character when
the function is called, it ignores that single character and
moves to the next one, in both directions.

If beginning or end of buffer are reached, the function stops and
treats point-min resp. point-max as if the character had been
found there."
  (interactive "cFrom: \ncTo: \nd\nP")
  (let* ((from (delim-find-char-balanced-backward from-char to-char))
         (to   (delim-find-char-balanced-forward  from-char to-char)))
    (if (and from to)
        (delim-kill-it from to save)
      (message "Not found!"))))

(defun delim-find-char-balanced (char move-func &optional counter-char)
  (save-excursion
    (let ((skip (if (and (eq (char-after) char)
                         (not (eq counter-char char)))
                    0 1)))
      (while (> skip 0)
        ; We could reach beginning or end of buffer.
        (condition-case nil
            (funcall move-func)
          (error (setq skip 0)))
        ; We need the (cond) instead of just two (when)s because char
        ; and counter-char can be the same character, in which case
        ; both tests would succeed.
        (cond ((eq (char-after) char)         (setq skip (1- skip)))
              ((eq (char-after) counter-char) (setq skip (1+ skip)))))
      (point))))

(defun delim-find-char-balanced-forward (opening closing)
  (let ((pos (delim-find-char-balanced closing 'forward-char opening)))
    ; Advance one character to include the closing character, if we're
    ; not at the end of buffer.
    (if (= pos (point-max))
        pos
      (1+ pos))))

(defun delim-find-char-balanced-backward (opening closing)
  (delim-find-char-balanced opening 'backward-char closing))

(defun delim-kill-it (from to save)
  (message "%s" (buffer-substring from to))
  (if save
      (kill-ring-save from to)
    (kill-region from to)))


;; Unit tests, using el-expectations by rubikitch,
;; <http://www.emacswiki.org/emacs/EmacsLispExpectations>.
;; ---------------------------------------------------------

(eval-when-compile
  (when (fboundp 'expectations)

    (defun kill-in-str (str go-to opening closing)
      (with-temp-buffer 
        (insert str)
        (search-backward go-to) 
        (delim-kill (string-to-char opening)
                    (string-to-char closing)
                    (point) nil)
        (car kill-ring-yank-pointer)))

    (expectations
      (desc "backward")
      (expect "{ foo {bar} baz }"
        (kill-in-str "{ foo {bar} baz }"
                     "baz"
                     "{" "}"))

      (desc "forward")
      (expect "{ foo {bar} baz }"
        (kill-in-str "{ foo {bar} baz }"
                     "foo"
                     "{" "}"))

      (desc "double nesting")
      (expect "{ foo {bar} baz }"
        (kill-in-str "{{ foo {bar} baz }}"
                     "baz"
                     "{" "}"))

      (desc "on opening char")
      (expect "{bar}"
        (kill-in-str "{bar}"
                     "{bar"
                     "{" "}"))

      (desc "on closing char")
      (expect "{bar}"
        (kill-in-str " { foo {bar} baz } "
                     "} b"
                     "{" "}"))

      (desc "same char")
      (expect " bar "
        (kill-in-str " foo bar baz "
                     "r b"
                     " " " "))

      (desc "same char, on char - ignore that one!")
      (expect " bar baz "
        (kill-in-str "foo bar baz foo"
                     " baz"
                     " " " "))

      (desc "beginning of buffer")
      (expect "foo bar "
        (kill-in-str "foo bar baz"
                     " bar"
                     " " " "))

      (desc "end of buffer")
      (expect " bar baz"
        (kill-in-str "foo bar baz"
                     " baz"
                     " " " ")))))

;; End unit tests. -----------------------------------------

(provide 'delim-kill)
;;; delim-kill.el ends here
