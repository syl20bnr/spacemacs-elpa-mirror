;;; visual-ascii-mode.el --- Visualize ascii code (small integer) on buffer.

;; Copyright (C) 2015 by Dewdrops

;; Author: Dewdrops <v_v_4474@126.com>
;; URL: https://github.com/Dewdrops/visual-ascii-mode
;; Package-Version: 20150129.1046
;; Version: 0.1
;; Keywords: presentation
;; Package-Requires:

;; This file is NOT part of GNU Emacs.

;;; License:
;;

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
;;
;; Installation:
;;
;; Use Melpa(https://github.com/milkypostman/melpa) or just put
;; visual-ascii-mode.el somewhere in your load-path and add these lines to your
;; .emacs:
;; (require 'visual-ascii-mode)
;;
;; Usage:
;; Type `M-x visual-ascii-mode' to enable visual-ascii-mode in current buffer.

;;; Code:


(defgroup visual-ascii-mode nil
  "Visualize ascii code on buffer"
  :prefix "visual-ascii-mode"
  :group 'Convenience)

(defcustom visual-ascii-mode-show-unicode nil
  "Non-nil means that any integer less than (`max-char') will be recognized as
 unicode and be visualized."
  :type 'boolean
  :group 'visual-ascii-mode)

(defcustom visual-ascii-mode-display-in-comment nil
  "Non-nil means that ascii code inside comment or string will be visualized."
  :type 'boolean
  :group 'visual-ascii-mode)

(defcustom visual-ascii-mode-show-unprintable-character t
  "This variable controls whether/how to display unprintable characters.

t means to show control characters in C-what style (which returned by `single-key-description'),
`abbrev' means to use ASCII control code abbreviation,
nil means not to display unprintable character."
  :type 'symbol
  :group 'visual-ascii-mode)

(defface visual-ascii-mode-printable-face
  '((t (:foreground "white" :background "red")))
  "Face used in visual-ascii-mode for printable characters"
  :group 'visual-ascii-mode)

(defface visual-ascii-mode-unprintable-face
  '((t (:foreground "black" :background "DeepSkyBlue")))
  "Face used in visual-ascii-mode for printable characters"
  :group 'visual-ascii-mode)

(defvar visual-ascii-mode/number2ascii
  ["NUL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK" "BEL" "BS" "HT"
   "LF" "VT" "FF" "CR" "SO" "SI" "DLE" "DC1" "DC2" "DC3"
   "DC4" "NAK" "SYN" "ETB" "CAN" "EM" "SUB" "ESC" "FS" "GS"
   "RS" "US" "SPC" "!" "\"" "#" "$" "%" "&" "'"
   "(" ")" "*" "+" "," "-" "." "/" "0" "1"
   "2" "3" "4" "5" "6" "7" "8" "9" ":" ";"
   "<" "=" ">" "?" "@" "A" "B" "C" "D" "E"
   "F" "G" "H" "I" "j" "k" "L" "M" "N" "O"
   "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y"
   "Z" "[" "\\" "]" "^" "_" "`" "a" "b" "c"
   "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
   "n" "o" "p" "q" "r" "s" "t" "u" "v" "w"
   "x" "y" "z" "{" "|" "}" "~" "DEL"])

(defvar visual-ascii-mode/number-regexp
  "\\b\\(?:0[xX][0-9a-fA-F]+\\|[0-9]+\\)\\b")

(defvar visual-ascii-mode/overlays nil)


(defun visual-ascii-mode/render ()
  "Render ascii characters."
  (dolist (ov visual-ascii-mode/overlays)
    (overlay-put ov
                 'after-string
                 (overlay-get ov 'visual-ascii-str))))

(defun visual-ascii-mode/cleanup ()
  "Clean visual-ascii-mode overlays."
  (dolist (ov visual-ascii-mode/overlays)
    (delete-overlay ov))
  (setq visual-ascii-mode/overlays nil))

(defun visual-ascii-mode/handler ()
  "Handler called in `post-command-hook'."
  (visual-ascii-mode/cleanup)
  (visual-ascii-mode/populate-overlays (window-start) (window-end))
  (visual-ascii-mode/render))

(defun visual-ascii-mode/comment-or-string-p ()
  "Determine whether inside string/comment cntext."
  (memq (syntax-ppss-context (syntax-ppss)) '(comment string)))

(defun visual-ascii-mode/populate-overlays (beg end)
  "Prepare all overlays to be displayed."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward visual-ascii-mode/number-regexp end t)
      (when (and (or visual-ascii-mode-display-in-comment
                     (not (visual-ascii-mode/comment-or-string-p)))
                 (let ((inhibit-changing-match-data t))
                   (save-excursion
                     (goto-char (match-beginning 0))
                     (looking-back "^\\|[^0-9a-zA-Z.+-]")))
                 (looking-at-p "$\\|[^0-9a-zA-Z.+-]"))
        (visual-ascii-mode/make-overlay (match-beginning 0)
                                     (match-end 0))))))

(defun visual-ascii-mode/str2num (s)
  "Convert string to integer."
  (ignore-errors
    (if (string-match-p "0[xX]" s)
        (string-to-number (substring s 2) 16)
      (string-to-number s))))

(defun visual-ascii-mode/make-overlay (beg end)
  "Make overlay for the number between BEG and END."
  (let ((n (visual-ascii-mode/str2num (buffer-substring-no-properties beg end))))
    (when (and (integerp n) (>= n 0))
      (let ((printable (and 
                        (characterp n)
                        (aref printable-chars n)))
            (desc
             (cond
              ((and (eq visual-ascii-mode-show-unprintable-character 'abbrev)
                    (< n 128))
               (aref visual-ascii-mode/number2ascii n))
              ((or (< n 128)
                   (and visual-ascii-mode-show-unicode
                        (characterp n))
                   (and visual-ascii-mode-show-unprintable-character
                        ;; 260046848 = 0b1111110000000000000000000000
                        (logand 260046848 n)
                        ;; 4194303 = 0b1111111111111111111111
                        (< (logand 4194303 n) 128)))
               (single-key-description n)))))
        (when desc
          (let ((ov (make-overlay beg end))
                (face (if printable
                          'visual-ascii-mode-printable-face
                        'visual-ascii-mode-unprintable-face)))
            (overlay-put ov
                         'visual-ascii-str
                         (propertize desc
                                     'face face))
            (add-to-list 'visual-ascii-mode/overlays ov)))))))

;;;###autoload
(define-minor-mode visual-ascii-mode
  "Visualize ascii code on buffer."
  :lighter ""
  (if visual-ascii-mode
      (progn
        (add-hook 'post-command-hook 'visual-ascii-mode/handler t 'local)
        (visual-ascii-mode/populate-overlays (window-start) (window-end))
        (visual-ascii-mode/render))
    (progn
      (remove-hook 'post-command-hook 'visual-ascii-mode/handler 'local)
      (visual-ascii-mode/cleanup))))

;;;###autoload
(defun turn-on-visual-ascii-mode ()
  "Turn on visual-ascii-mode."
  (unless (minibufferp)
   (visual-ascii-mode 1)))

(define-globalized-minor-mode global-visual-ascii-mode
  visual-ascii-mode turn-on-visual-ascii-mode)

;;;###autoload
(autoload 'global-visual-ascii-mode "visual-ascii-mode"
  "The globlized visual-ascii-mode" t)


(provide 'visual-ascii-mode)
;;; visual-ascii-mode.el ends here
