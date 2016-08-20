;;; nhexl-mode.el --- Minor mode to edit files via hex-dump format  -*- lexical-binding: t -*-

;; Copyright (C) 2010, 2012, 2016  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: data
;; Version: 0.2
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

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

;; This package implements NHexl mode, a minor mode for editing files
;; in hex dump format.  The mode command is called `nhexl-mode'.
;;
;; This minor mode implements similar functionality to `hexl-mode',
;; but using a different implementation technique, which makes it
;; usable as a "plain" minor mode.  It works on any buffer, and does
;; not mess with the undo boundary or with the major mode.
;;
;; In theory it could also work just fine even on very large buffers,
;; although in practice it seems to make the display engine suffer.

;;; Todo:
;; - Clicks on the hex side should put point at the right place.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'hexl)                         ;For faces.

(defgroup nhexl nil
  "Edit a file in a hex dump format."
  :group 'data)

(defvar nhexl-line-width 16
  "Number of bytes per line.")

(defvar nhexl--display-table
  (let ((dt (make-display-table)))
    ;; (aset dt ?\n [?␊])
    (aset dt ?\t [?␉])
    dt))

(defvar nhexl--saved-vars nil)
(make-variable-buffer-local 'nhexl--saved-vars)
(defvar nhexl--point nil)
(make-variable-buffer-local 'nhexl--point)

;;;###autoload
(define-minor-mode nhexl-mode
  "Minor mode to edit files via hex-dump format"
  :lighter " NHexl"
  (if (not nhexl-mode)
      (progn
        (dolist (varl nhexl--saved-vars)
          (set (make-local-variable (car varl)) (cdr varl)))
        (kill-local-variable 'nhexl--saved-vars)
        (jit-lock-unregister #'nhexl--jit)
        (remove-hook 'after-change-functions #'nhexl--change-function 'local)
        (remove-hook 'post-command-hook #'nhexl--post-command 'local)
        ;; FIXME: This will conflict with any other use of `display'.
        (with-silent-modifications
          (put-text-property (point-min) (point-max) 'display nil))
        (remove-overlays (point-min) (point-max) 'nhexl t))
    (unless (local-variable-p 'nhexl--saved-vars)
      (dolist (var '(buffer-display-table buffer-invisibility-spec
                     overwrite-mode header-line-format))
        (push (cons var (symbol-value var)) nhexl--saved-vars)))
    (setq nhexl--point (point))
    (setq header-line-format '(:eval (nhexl--header-line)))
    (binary-overwrite-mode 1)
    (setq buffer-invisibility-spec ())
    (set (make-local-variable 'buffer-display-table) nhexl--display-table)
    (jit-lock-register #'nhexl--jit)
    (add-hook 'change-major-mode-hook (lambda () (nhexl-mode -1)) nil 'local)
    (add-hook 'post-command-hook #'nhexl--post-command nil 'local)
    (add-hook 'after-change-functions #'nhexl--change-function nil 'local)))

(defun nhexl--change-function (beg end len)
  ;; Jit-lock already takes care of refreshing the changed area, so we
  ;; only have to make sure the tail's addresses are refreshed when
  ;; text is inserted/removed.
  (when (/= len (- end beg))
    (put-text-property beg (point-max) 'fontified nil)))

(defvar nhexl--overlay-counter 100)
(make-variable-buffer-local 'nhexl--overlay-counter)

(defun nhexl--debug-count-ols ()
  (let ((i 0))
    (dolist (ol (overlays-in (point-min) (point-max)))
      (when (overlay-get ol 'nhexl) (cl-incf i)))
    i))

(defun nhexl--flush-overlays (buffer)
  (with-current-buffer buffer
    (kill-local-variable 'nhexl--overlay-counter)
    ;; We've created many overlays in this buffer, which can slow
    ;; down operations significantly.  Let's flush them.
    ;; An easy way to flush them is
    ;;   (remove-overlays min max 'nhexl t)
    ;;   (put-text-property min max 'fontified nil)
    ;; but if the visible part of the buffer requires more than
    ;; nhexl--overlay-counter overlays, then we'll inf-loop.
    ;; So let's be more careful about removing overlays.
    (let ((windows (get-buffer-window-list nil nil t))
          (start (point-min))
          (zero (save-restriction (widen) (point-min)))
          (debug-count (nhexl--debug-count-ols)))
      (with-silent-modifications
        (while (< start (point-max))
          (let ((end (point-max)))
            (dolist (window windows)
              (cond
               ((< start (1- (window-start window)))
                (setq end (min (1- (window-start window)) end)))
               ((< start (1+ (window-end window)))
                (setq start (1+ (window-end window))))))
            ;; Round to multiple of nhexl-line-width.
            (setq start (+ zero (* (ceiling (- start zero) nhexl-line-width)
                                   nhexl-line-width)))
            (setq end (+ zero (* (truncate (- end zero) nhexl-line-width)
                                 nhexl-line-width)))
            (when (< start end)
              (remove-overlays start end 'nhexl t)
              (put-text-property start end 'fontified nil)
              (setq start (+ end nhexl-line-width))))))
      (let ((debug-new-count (nhexl--debug-count-ols)))
        (message "Flushed %d overlays, %d remaining"
                 (- debug-count debug-new-count) debug-new-count)))))

(defun nhexl--make-line (from next zero)
  (let* ((nextpos (min next (point-max)))
         (bufstr (buffer-substring from nextpos))
         (i -1)
         (s (concat
             (unless (eq zero from) "\n")
             (format (propertize "%08x:" 'face
                                 (if (or (< nhexl--point from)
                                         (>= nhexl--point next))
                                     'hexl-address-region
                                   '(highlight hexl-address-region)))
                     (- from zero))
             (propertize " " 'display '(space :align-to 12))
             (mapconcat (lambda (c)
                          (setq i (1+ i))
                          ;; FIXME: In multibyte buffers,
                          ;; do something clever about
                          ;; non-ascii chars.
                          (let ((s (format "%02x" c)))
                            (when (eq nhexl--point (+ from i))
                              (put-text-property 0 (length s)
                                                 'face 'highlight
                                                 s))
                            (if (zerop (mod i 2))
                                s (concat s " "))))
                        bufstr
                        "")
             (if (> next nextpos)
                 (make-string (+ (/ (1+ (- next nextpos)) 2)
                                 (* (- next nextpos) 2))
                              ?\s))
             (propertize "  " 'display
                         `(space :align-to
                                 ,(+ (/ (* nhexl-line-width 5) 2)
                                     12 3))))))
    (font-lock-append-text-property 0 (length s) 'face 'default s)
    s))

(defun nhexl--jit (from to)
  (let ((zero (save-restriction (widen) (point-min))))
    (setq from (+ zero (* (truncate (- from zero) nhexl-line-width)
                          nhexl-line-width)))
    (setq to (+ zero (* (ceiling (- to zero) nhexl-line-width)
                        nhexl-line-width)))
    (remove-overlays from (min to (point-max)) 'nhexl t)
    (save-excursion
      (goto-char from)
      (while (search-forward "\n" to t)
        (put-text-property (match-beginning 0) (match-end 0)
                           'display (copy-sequence "␊"))))
    (while (< from to)

      (cl-decf nhexl--overlay-counter)
      (when (and (= nhexl--overlay-counter 0)
                 ;; If the user enabled jit-lock-stealth fontification, then
                 ;; removing overlays is just a waste since
                 ;; jit-lock-stealth will restore them anyway.
                 (not jit-lock-stealth-time))
        ;; (run-with-idle-timer 0 nil 'nhexl--flush-overlays (current-buffer))
        )
      
      (let* ((next (+ from nhexl-line-width))
             (ol (make-overlay from next))
             (s (nhexl--make-line from next zero)))
        (overlay-put ol 'nhexl t)
        (overlay-put ol 'face 'hexl-ascii-region)
        (overlay-put ol 'before-string s)
        (setq from next)))))

(defun nhexl--header-line ()
  ;; FIXME: merge with nhexl--make-line.
  (let* ((zero (save-restriction (widen) (point-min)))
         (text
          (let ((tmp ()))
            (dotimes (i nhexl-line-width)
              (push (if (< i 10) (+ i ?0) (+ i -10 ?a)) tmp))
            (apply 'string (nreverse tmp))))
         (pos (mod (- nhexl--point zero) nhexl-line-width))
         (i -1))
    (put-text-property pos (1+ pos) 'face 'highlight text)
    (concat
     (propertize " " 'display '(space :align-to 0))
     "Address:"
     (propertize " " 'display '(space :align-to 12))
     (mapconcat (lambda (c)
                  (setq i (1+ i))
                  (let ((s (string c c)))
                    (when (eq i pos)
                      (put-text-property 0 (length s)
                                         'face 'highlight
                                         s))
                    (if (zerop (mod i 2)) s
                      (concat
                       s (propertize " " 'display
                                     `(space :align-to
                                             ,(+ (/ (* i 5) 2) 12 3)))))))
                text
                "")
     (propertize "  " 'display
                 `(space :align-to
                         ,(+ (/ (* nhexl-line-width 5) 2)
                             12 3)))
     text)))
  

(defun nhexl--post-command ()
  (when (/= (point) nhexl--point)
    (let ((zero (save-restriction (widen) (point-min)))
          (oldpoint nhexl--point))
      (setq nhexl--point (point))
      (with-silent-modifications
        (nhexl--jit (point) (1+ (point)))
        (if (/= (truncate (- (point) zero) nhexl-line-width)
                (truncate (- oldpoint zero) nhexl-line-width))
            (nhexl--jit oldpoint (1+ oldpoint)))))))

;;;; ChangeLog:

;; 2016-08-08  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* nhexl-mode.el: Use cl-lib
;; 
;; 2012-03-25  Chong Yidong  <cyd@gnu.org>
;; 
;; 	nhexl-mode.el: Fix last change.
;; 
;; 2012-03-24  Chong Yidong  <cyd@gnu.org>
;; 
;; 	Commentary tweaks for csv-mode, ioccur, and nhexl-mode packages.
;; 
;; 2012-03-20  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Add nhexl-mode.
;; 

  

(provide 'nhexl-mode)
;;; nhexl-mode.el ends here
