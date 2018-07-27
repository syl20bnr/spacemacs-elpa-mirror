;;; jump-to-line.el --- Jump to line number at point.

;; Copyright (C) 2013 ongaeshi

;; Author: ongaeshi
;; Keywords: jump, line, back, file, ruby, csharp, python, perl
;; Package-Version: 20130122.1653
;; Version: 0.2
;; Package-Requires:

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

;; Jump to line number at point.
:; Back to position before the jump.
;;

;;; Examples of jump:

;; /path/to/a.txt:5 ; Jump to /path/to/a.txt, line 5 (Ruby style)
;; ../a.txt:5       ; Relative path
;; a.txt            ; Only filename
;; a.txt(1,2)       ; C# style
;; a.txt, line 1    ; Python
;; a.txt line 1     ; Perl

;;; URL:
;;   https://github.com/ongaeshi/jump-to-line

;;; Install:
;;   (auto-install-from-url "https://raw.github.com/ongaeshi/jump-to-line/master/jump-to-line.el")

;;; Initlial Setting:

;; (require 'jump-to-line)
;; (global-set-key (kbd "C-c C-j") 'jump-to-line) ; Jump
;; (global-set-key (kbd "C-c b")   'jtl-back)     ; Back

;;; Code:

(defvar jtl-stack nil
  "Stack of mark.")

(defvar jtl-history nil
  "History of commands.")

(defface jtl-highlight-line-face '((t (:background "#66ccff" :underline t)))
  "Face for jump highlight." :group 'jump-to-line)

;;;###autoload
(defun jump-to-line (n)
  "Comment."
  (interactive "P")
  (let ((pair            (jtl-ffap-file-line-at-point))
        (is-other-window (or (consp n)
                             (js-popup-window-p))))
    (jtl-push-stack (point-marker))
    (if pair
        (let ((filename (car pair))
              (lineno   (cdr pair)))
          (jtl-push-history   (format "%s:%s" filename lineno))
          (jtl-find-goto-line filename lineno is-other-window))
      (let* ((input (read-string "Jump to: " (thing-at-point 'filename) 'jtl-history))
             (list  (split-string input ":")))
        (jtl-find-goto-line (nth 0 list) (string-to-number (nth 1 list)) is-other-window)))))

;;;###autoload
(defun jtl-back ()
  "Comment."
  (interactive)
  (if (null jtl-stack)
      (error "jtl-stack is empty"))
  (jtl-jump-mark (jtl-pop-stack)))

;;;###autoload
(defun jtl-push-stack (mark)
  (setq jtl-stack (cons mark jtl-stack)))

;;; Private:

(defun jtl-ffap-file-line-at-point ()
  "a.txt:5       ;-> (a.txt . 5)
a.txt         ;-> (a.txt . 1)
a.txt(1,2)    ;-> (a.txt, 1) (C#)
a.txt, line 1 ;-> (a.txt, 1) (Python)
a.txt line 1  ;-> (a.txt, 1) (Perl)
"
  (let ((it (ffap-file-at-point)))
    (if it
        (save-excursion
          (beginning-of-line)
          (if (and (search-forward it nil t)
                     (looking-at "\\(?::\\|(\\| line \\)\\([0-9]+\\)"))
              (cons it (string-to-number (match-string 1)))
            (cons it 1))))))

(defun jtl-find-goto-line (filename lineno &optional is-other-window)
  (if is-other-window
    (find-file-other-window  filename)
    (find-file filename))
  (jtl-goto-line  lineno)
  (jtl-highlight-line 0.3))

(defun jtl-jump-mark (mark)
  (switch-to-buffer (marker-buffer mark))
  (goto-char        (marker-position mark)))

(defun jtl-goto-line (lineno)
  (goto-char (point-min))
  (goto-char (point-at-bol lineno)))

(defun jtl-pop-stack ()
  (let ((mark (car jtl-stack)))
    (setq jtl-stack (cdr jtl-stack))
    mark))

;; jtl-push-stack is public

(defun jtl-push-history (str)
  (setq jtl-history (cons str jtl-history)))

(defun jtl-highlight-line (seconds)
  (jtl-highlight-line-start)
  (sit-for seconds)
  (jtl-highlight-line-end))

(defvar jtl-match-line-overlay nil)

(defun jtl-highlight-line-start ()
  (let ((args (list (line-beginning-position) (1+ (line-end-position)) nil)))
    (if (not jtl-match-line-overlay)
        (setq jtl-match-line-overlay (apply 'make-overlay args))
      (apply 'move-overlay jtl-match-line-overlay args))
    (overlay-put jtl-match-line-overlay 'face 'jtl-highlight-line-face)))

(defun jtl-highlight-line-end ()
  (when jtl-match-line-overlay
    (delete-overlay jtl-match-line-overlay)
    (setq jtl-match-line-overlay nil)))

(defun js-popup-window-p ()
  (if (and
       (featurep 'popwin)
       (popwin:popup-window-live-p))
      (eq popwin:popup-window (selected-window))))

(provide 'jump-to-line)
;;; jump-to-line.el ends here
