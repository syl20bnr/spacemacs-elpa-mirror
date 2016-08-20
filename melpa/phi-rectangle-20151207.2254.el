;;; phi-rectangle.el --- another rectangle-mark command (rewrite of rect-mark)

;; Copyright (C) 2013-2015 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Package-Version: 20151207.2254
;; Version: 1.1.3

;;; Commentary:

;; Require this script
;;
;;   (require 'phi-rectangle)
;;
;; and call function "phi-rectangle-mode". Then following commands are
;; available

;; - [C-RET] phi-rectangle-set-mark-command
;;
;;   Activate the rectangle mark.

;; - phi-rectangle-kill-region (replaces "kill-region")
;;
;;   A dwim version of "kill-region". If the rectangle mark is active,
;;   kill rectangle. If the normal mark is active, kill region as usual.
;;   Otherwise, kill whole line.

;; - phi-rectangle-kill-ring-save (replaces "kill-ring-save")
;;
;;   A dwim version of "kill-ring-save" like "phi-rectangle-kill-region".

;; - phi-rectangle-yank (replaces "yank")
;;
;;   A dwim version of "yank". If the last killed object is a rectangle,
;;   yank rectangle. Otherwise yank a kill-ring item as usual.

;;; Change Log:

;; 1.0.0 first released
;; 1.1.0 better integration with multiple-cursors
;; 1.1.1 delete trailing whitespaces on rectangle-yank
;; 1.1.2 handle texts copied from other programs
;; 1.1.3 add new option phi-rectangle-collect-fake-cursors-kill-rings
;;       phi-rectangle-yank now accepts a prefix argument

;;; Code:

(require 'rect)
(defconst phi-rectangle-version "1.1.3")

;; + customizable vars

(defvar phi-rectangle-collect-fake-cursors-kill-rings 'rectangle
  "how phi-rectangle should collect fake cursors' kill-rings when
  multiple-cursors-mode is turned off. when nil, fake cursors'
  kill-rings are ignored. when 'rectangle, kill-rings are
  collected as a rectangle. when any other non-nil value,
  kill-rings are collected as a multi-line string.")

;; + keymaps

(defvar phi-rectangle-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "C-<return>") 'phi-rectangle-set-mark-command)
    (define-key kmap [remap kill-region] 'phi-rectangle-kill-region)
    (define-key kmap [remap kill-ring-save] 'phi-rectangle-kill-ring-save)
    (define-key kmap [remap yank] 'phi-rectangle-yank)
    kmap))

(defvar phi-rectangle-mark-active-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap [remap forward-char] 'phi-rectangle-forward-char)
    (define-key kmap [remap previous-line] 'phi-rectangle-previous-line)
    (define-key kmap [remap next-line] 'phi-rectangle-next-line)
    (define-key kmap [remap keyboard-quit] 'phi-rectangle-keyboard-quit)
    kmap))

;; + internal variables

(defvar phi-rectangle-mark-active nil)
(make-variable-buffer-local 'phi-rectangle-mark-active)
(add-to-list 'minor-mode-map-alist
             (cons 'phi-rectangle-mark-active phi-rectangle-mark-active-map))

(defvar phi-rectangle--overlays nil)
(make-variable-buffer-local 'phi-rectangle--overlays)

(defvar phi-rectangle--last-killed-is-rectangle nil)

;; + minor-mode

;;;###autoload
(define-minor-mode phi-rectangle-mode
  "minor mode to set rectangle-mark"
  :init-value nil
  :global t
  :keymap phi-rectangle-mode-map)

;; + utility functions

(defun phi-rectangle--delete-rectangle (start end)
  (exchange-point-and-mark)
  (if (> start end)
      (delete-rectangle end start)
    (delete-rectangle start end))
  (phi-rectangle-deactivate-mark))

(defun phi-rectangle--copy-rectangle (start end)
  (setq killed-rectangle (if (> start end)
                             (extract-rectangle end start)
                           (extract-rectangle start end))
        phi-rectangle--last-killed-is-rectangle t)
  (phi-rectangle-deactivate-mark))

(defun phi-rectangle--kill-rectangle (start end)
  (phi-rectangle--copy-rectangle start end)
  (phi-rectangle--delete-rectangle start end)
  (phi-rectangle-deactivate-mark))

(defun phi-rectangle--delete-trailing-whitespaces (start end)
  (save-restriction
    (narrow-to-region start
                      (save-excursion (goto-char end) (point-at-eol)))
    (delete-trailing-whitespace)))

(defun phi-rectangle--handle-interprogram-paste ()
  "handle texts copied from other programs"
  ;; copied from current-kill
  (let ((interprogram-cut-function nil)
        (interprogram-paste (and interprogram-paste-function
                                 (funcall interprogram-paste-function))))
    (when interprogram-paste
      (if (listp interprogram-paste)
          (mapc 'kill-new (nreverse interprogram-paste))
        (kill-new interprogram-paste))
      (setq phi-rectangle--last-killed-is-rectangle nil))))

;; + hook functions

(defun phi-rectangle--post-command ()
  (if deactivate-mark
      (phi-rectangle-deactivate-mark)
    (phi-rectangle--update)))

(defun phi-rectangle--update ()
  (mapc 'delete-overlay phi-rectangle--overlays)
  (setq phi-rectangle--overlays nil)
  (save-excursion
    (let* ((start (save-excursion
                    (goto-char (region-beginning))
                    (cons (line-number-at-pos) (current-column))))
           (end (save-excursion
                  (goto-char (region-end))
                  (cons (line-number-at-pos) (current-column))))
           (start-lin (max (car start)
                           (line-number-at-pos (window-start nil))))
           (end-lin (min (car end)
                         (line-number-at-pos (window-end nil t))))
           (start-col (min (cdr start) (cdr end)))
           (end-col (max (cdr start) (cdr end)))
           deactivate-mark ov)
      (goto-char (point-min))
      (forward-line (1- start-lin))
      (dotimes (_ (1+ (- end-lin start-lin)))
        (setq ov
              (make-overlay (progn (move-to-column start-col t)
                                   (point))
                            (progn (move-to-column end-col t)
                                   (point))))
        (overlay-put ov 'face 'region)
        (setq phi-rectangle--overlays
              (cons ov phi-rectangle--overlays))
        (forward-line)))))

;; + motion commands

(defun phi-rectangle-previous-line ()
  (interactive)
  (let ((col (current-column))
        deactivate-mark)
    (when (zerop (forward-line -1))
      (move-to-column col t))))

(defun phi-rectangle-next-line ()
  (interactive)
  (let ((col (current-column))
        deactivate-mark)
    (unless (progn (forward-line 1) (eobp))
      (move-to-column col t))))

(defun phi-rectangle-forward-char ()
  (interactive)
  (let (deactivate-mark)
    (if (eolp)
        (insert " ")
      (forward-char))))

;; + kill-ring commands

(defun phi-rectangle-kill-ring-save ()
  "when region is active, copy region as usual. when rectangle-region is
active, copy rectangle. otherwise, copy whole line."
  (interactive)
  (phi-rectangle--handle-interprogram-paste)
  (cond (phi-rectangle-mark-active
         (phi-rectangle--copy-rectangle (region-beginning) (region-end))
         (phi-rectangle--delete-trailing-whitespaces (region-beginning) (region-end)))
        ((use-region-p)
         (kill-ring-save (region-beginning) (region-end)))
        (t
         (copy-region-as-kill
          (line-beginning-position)
          (line-beginning-position 2)))))

(defun phi-rectangle-kill-region ()
  "when region is active, kill region as usual. when rectangle-region is
active, kill rectangle. otherwise, kill whole line."
  (interactive)
  (phi-rectangle--handle-interprogram-paste)
  (cond (phi-rectangle-mark-active
         (phi-rectangle--kill-rectangle (region-beginning) (region-end)))
        ((use-region-p)
         (kill-region (region-beginning) (region-end)))
        (t
         (kill-whole-line))))

(defun phi-rectangle-yank (&optional arg)
  "when rectangle is killed recently, yank rectangle. otherwise yank as usual."
  (interactive "*P")
  (phi-rectangle--handle-interprogram-paste)
  (setq arg (or arg 1))
  (cond ((not phi-rectangle--last-killed-is-rectangle)
         (yank arg))
        ((consp arg)
         (let ((pos (point)))
           (yank-rectangle)
           (push-mark)
           (goto-char pos))
         (phi-rectangle--delete-trailing-whitespaces (point) (mark)))
        ((= arg 1)
         (push-mark)
         (yank-rectangle)
         (phi-rectangle--delete-trailing-whitespaces (mark) (point)))
        (t
         (yank (- arg 1)))))

(defadvice kill-new (after phi-rectangle-kill-new-ad activate)
  (setq phi-rectangle--last-killed-is-rectangle nil))

;; + activate/deactivate mark

(defun phi-rectangle-keyboard-quit (start end)
  (interactive "r")
  (phi-rectangle--delete-trailing-whitespaces start end)
  (phi-rectangle-deactivate-mark)
  (keyboard-quit))

(defun phi-rectangle-set-mark-command ()
  (interactive)
  (unless phi-rectangle-mark-active
    (setq phi-rectangle-mark-active t)
    (set (make-local-variable 'transient-mark-mode) nil)
    (add-hook 'post-command-hook 'phi-rectangle--post-command nil t)
    (add-hook 'deactivate-mark-hook 'phi-rectangle-deactivate-mark nil t))
  (push-mark nil nil t))

(defun phi-rectangle-deactivate-mark (&optional dont-clean)
  (mapc 'delete-overlay phi-rectangle--overlays)
  (setq phi-rectangle--overlays nil
        phi-rectangle-mark-active nil)
  (kill-local-variable 'transient-mark-mode)
  (when (and transient-mark-mode mark-active)
    (deactivate-mark))
  (remove-hook 'post-command-hook 'phi-rectangle--post-command t)
  (remove-hook 'deactivate-mark-hook 'phi-rectangle-deactivate-mark t))

;; + delsel workaround

(eval-after-load "delsel"
  '(progn

     (defun delete-active-region (&optional killp)
       (interactive)
       (if killp
           (if phi-rectangle-mark-active
               (phi-rectangle--kill-rectangle (point) (mark))
             (kill-region (point) (mark)))
         (if phi-rectangle-mark-active
             (phi-rectangle--delete-rectangle (point) (mark))
           (delete-region (point) (mark))))
       t)

     (defadvice delete-selection-pre-hook (around phi-delsel-workaround activate)
       (let ((old-tmm transient-mark-mode)
             (old-tmm-localp (local-variable-p 'transient-mark-mode)))
         (setq transient-mark-mode (or transient-mark-mode phi-rectangle-mark-active))
         ad-do-it
         (unless (and old-tmm-localp
                      (not (local-variable-p 'transient-mark-mode)))
           (setq transient-mark-mode old-tmm))))

     (put 'phi-rectangle-yank 'delete-selection 'yank)

     ))

;; + multiple-cursors integration

(when (locate-library "multiple-cursors")

  (eval-after-load "delsel"
    '(progn
       (autoload 'phi-rectangle-mc/edit-lines "multiple-cursors")
       (defadvice delete-active-region (around phi-mc activate)
         (let ((lines (- (line-number-at-pos (point))
                         (line-number-at-pos (mark))))
               (old-rma phi-rectangle-mark-active))
           ad-do-it
           (when old-rma
             (phi-rectangle-mc/edit-lines lines))))
       ))

  (eval-after-load "multiple-cursors"
    '(progn

       (defun phi-rectangle-mc/edit-lines (lines)
         (mc/remove-fake-cursors)
         (deactivate-mark)
         (let ((direction (if (< lines 0) -1 1))
               (col (current-column)))
           (save-excursion
             (dotimes (_ (abs lines))
               (forward-line direction)
               (move-to-column col t)
               (mc/create-fake-cursor-at-point))))
         (multiple-cursors-mode 1))

       (defun mc--maybe-set-killed-rectangle ()
         (let ((entries (mc--kill-ring-entries)))
           (unless (mc--all-equal entries)
             (when phi-rectangle-collect-fake-cursors-kill-rings
               (if (eq phi-rectangle-collect-fake-cursors-kill-rings 'rectangle)
                   (setq killed-rectangle entries
                         phi-rectangle--last-killed-is-rectangle t)
                 (kill-new (mapconcat 'identity entries "\n")))))))

       (defadvice phi-rectangle-yank (before phi-mc activate)
         (when (and multiple-cursors-mode
                    phi-rectangle--last-killed-is-rectangle)
           (let* ((n 0))
             (mc/for-each-cursor-ordered
              (let ((kill-ring (overlay-get cursor 'kill-ring))
                    (kill-ring-yank-pointer (overlay-get cursor 'kill-ring-yank-pointer)))
                (kill-new (or (replace-regexp-in-string "[\s\t]*$" ""
                                                        (nth n killed-rectangle))
                              ""))
                (overlay-put cursor 'kill-ring kill-ring)
                (overlay-put cursor 'kill-ring-yank-pointer kill-ring-yank-pointer)
                (setq n (1+ n)))))
           (setq phi-rectangle--last-killed-is-rectangle nil)))
       ))
  )

;; + provide

(provide 'phi-rectangle)

;;; phi-rectangle.el ends here
