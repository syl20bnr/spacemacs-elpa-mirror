;;; nlinum-hl.el --- highlight current line number for nlinum
;;
;; Copyright (C) 2017 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: Jun 03, 2017
;; Modified: Jun 05, 2017
;; Version: 1.0.2
;; Package-Version: 20170605.324
;; Keywords: nlinum highlight current line faces
;; Homepage: https://github.com/hlissner/emacs-nlinum-hl
;; Package-Requires: ((emacs "24.4") (nlinum "1.6") (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Extends nlinum to provide current-line-number highlighting, and tries to
;; mitigate disappearing line numbers (a known issue with nlinum).
;;
;;; Installation:
;;
;; M-x package-install RET nlinum-hl
;;
;;   (require 'nlinum-hl)
;;   (add-hook 'nlinum-mode-hook #'nlinum-hl-mode))
;;
;; Alternatively, use `use-package':
;;
;;   (use-package nlinum-hl
;;     :after nlinum
;;     :config
;;     (add-hook 'nlinum-mode-hook #'nlinum-hl-mode))
;;
;;; Code:

(require 'cl-lib)
(require 'nlinum)

(defgroup nlinum-hl nil
  "Options for nlinum-hl."
  :group 'faces) ; FIXME :group

(defcustom nlinum-hl-redraw 'window
  "Determines what nlinum-hl should do when it encounters a missing line number.

If nil, do nothing about it.
If 'line', fix only that line number (fastest).
If 'window', redraw only the visible line numbers in the current window.
If 'buffer', redraw all line numbers in that buffer.
If t, redraw nlinum across all buffers (slowest)."
  :group 'nlinum-hl
  :type '(choice (const :tag "Current line" 'line)
                 (const :tag "Visible window" 'window)
                 (const :tag "Whole buffer" 'buffer)
                 (const :tag "All windows" t)
                 (const :tag "Do nothing" nil)))

(defface nlinum-hl-face
  '((((background dark))  (:inherit linum :weight bold :foreground "white"))
    (((background white)) (:inherit linum :weight bold :foreground "black")))
  "Face for the highlighted line number."
  :group 'nlinum-hl)

(defvar nlinum-hl--overlay nil)
(defvar nlinum-hl--line "0")

;;
(defun nlinum-hl-overlay-p (ov)
  "Return t if OV (an overlay) is an nlinum overlay."
  (overlay-get ov 'nlinum))

(defun nlinum-hl--this-overlay (beg end)
  "Get the nlinum overlay for the current line."
  (cl-find-if #'nlinum-hl-overlay-p (overlays-in beg end)))

(defun nlinum-hl-line (&optional force-p)
  "Highlight the current nlinum line number."
  (while-no-input
    (let ((lineno (format-mode-line "%l")))
      (when (or force-p (not (equal nlinum-hl--line lineno)))
        (let* ((pbol (line-beginning-position))
               (peol (min (1+ pbol) (point-max))))
          (setq nlinum-hl--line lineno)
          ;; (jit-lock-fontify-now pbol peol)
          ;; Unhighlight previous highlight
          (when nlinum-hl--overlay
            (let ((str (nth 1 (get-text-property 0 'display (overlay-get nlinum-hl--overlay 'before-string)))))
              (put-text-property 0 (length str) 'face 'linum str)
              (setq nlinum-hl--overlay nil)))
          (let ((ov (nlinum-hl--this-overlay pbol peol)))
            ;; highlight current line number
            (if ov
                (nlinum--region pbol peol)
              ;; Try to deal with evaporated line numbers
              (unless (eobp)
                (cond ((eq nlinum-hl-redraw 'line)
                       (nlinum--region pbol peol))
                      ((eq nlinum-hl-redraw 'window)
                       (nlinum-hl-flush-region (window-start) (window-end)))
                      ((eq nlinum-hl-redraw 'buffer)
                       (nlinum-hl-flush-window nil t))
                      ((eq nlinum-hl-redraw t)
                       (nlinum-hl-flush-all-windows)))))
            (when (setq ov (nlinum-hl--this-overlay pbol peol))
              (overlay-put ov 'nlinum-hl t)
              (let ((str (nth 1 (get-text-property 0 'display (overlay-get ov 'before-string)))))
                (put-text-property 0 (length str) 'face 'nlinum-hl-face str)
                (setq nlinum-hl--overlay ov)))))))))

(defun nlinum-hl-flush-region (&optional beg end preserve-hl-p)
  "Redraw nlinum within the region BEG and END (points).

If PRESERVE-HL-P, then don't affect the current line highlight."
  (when nlinum-mode
    (let ((beg (or beg (point-min)))
          (end (or end (point-max))))
      (if (not preserve-hl-p)
          (nlinum--region beg end)
        ;; done in two steps to leave current line number highlighting alone
        (nlinum--region beg (max 1 (1- (line-beginning-position))))
        (nlinum--region (min wend (1+ (line-end-position))) end)))))

(defun nlinum-hl-flush-window (&optional window force-p)
  "If line numbers are missing in the current WINDOW, force nlinum to redraw.

If FORCE-P (universal argument), then do it regardless of whether line numbers
are missing or not."
  (interactive "iP")
  (let ((window (or window (selected-window)))
        (orig-win (selected-window)))
    (with-selected-window window
      (when nlinum-mode
        (let ((wbeg (window-start))
              (wend (window-end))
              (lines 0)
              (ovs 0))
          (unless force-p
            (save-excursion
              (goto-char wbeg)
              (while (and (<= (point) wend)
                          (not (eobp)))
                (let ((lbeg (line-beginning-position)))
                  (when (cl-some (lambda (ov) (overlay-get ov 'nlinum)) (overlays-in lbeg (1+ lbeg)))
                    (cl-incf ovs)))
                (forward-line 1)
                (cl-incf lines))))
          (when (or force-p (not (= ovs lines)))
            (nlinum-hl-flush-region
             (point-min) (point-max)
             (or force-p (not (= ovs lines))))))))))

(defun nlinum-hl-flush-all-windows (&rest _)
  "Flush nlinum in all windows."
  (mapc #'nlinum-hl-flush-window (window-list))
  nil)

(defun nlinum-hl-do-flush (&optional _ norecord)
  "Advice for `select-window' to flush nlinum before/after switching."
  ;; norecord check is necessary to prevent infinite recursion in
  ;; `select-window'
  (when (not norecord)
    (nlinum-hl-flush-window)))

;;;###autoload
(define-minor-mode nlinum-hl-mode
  "Highlight current line in current buffer, using nlinum-mode."
  :lighter "" ; should be obvious it's on
  :init-value nil
  (cond (nlinum-hl-mode
         (nlinum-mode +1)
         (add-hook 'post-command-hook #'nlinum-hl-line nil t))

        (t
         (remove-hook 'post-command-hook #'nlinum-hl-line t))))

;; Changing fonts can upset nlinum overlays; this forces them to resize.
(advice-add #'set-frame-font :after #'nlinum-hl-flush-all-windows)

;; Folding in `web-mode' can cause nlinum glitching. This attempts to fix it.
(advice-add #'web-mode-fold-or-unfold :after #'nlinum-hl-do-flush)

(provide 'nlinum-hl)
;;; nlinum-hl.el ends here
