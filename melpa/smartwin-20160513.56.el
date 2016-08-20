;;; smartwin.el --- A minor mode shows shell like buffers. -*- lexical-binding:t -*-

;; Copyright (C) 2015 GuanghuiXu
;; Copyright (C) 2011-2015 Tomohiro Matsuyama

;; Author: GuanghuiXu gh_xu@qq.com
;;         Tomohiro Matsuyama <m2ym.pub@gmail.com>
;; Maintainer: GuanghuiXu gh_xu@qq.com
;; Created: 2015-4-28
;; Keywords: convenience
;; Version: 0.1
;; URL: https://github.com/jerryxgh/smartwin
;; Package-Version: 20160513.56
;; Package-X-Original-Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Smartwin is a window for shell buffers, temp buffers and etc.

;; This minor mode let shell like buffers share a window, called smart window,
;; the smart window is always at the bottom of Emacs window.  Besides that, when
;; point move into or out the smart window, it will be enlarged or shrinked
;; automaticly.  Warning: this package can not work with popwin.el.
;;
;; To use smartwin, place this file to load path and add this to .emacs:
;;
;; (require 'smartwin)
;; (smartwin-mode 1)
;;
;; or run M-x smartwin-mode to toggle it.
;;
;; To switch between buffers of smart window, you can bind keys like:
;;     (define-key smartwin-mode-map (kbd "C-c s") 'smartwin-switch-buffer)
;;
;; Then try run M-x shell or or eshell, if you want to show more buffers in
;; smart window, please customize variable: smartwin-buffers.

;;; Change Log:

;;; TODO:
;; 1. Replace advices to others means

;;; BUGS:
;; 1. can not work with popwin or packages using popwin like guide-key

;;; Code:

(require 'ido)
(require 'esh-mode)

;; for byte-compile warnings
(declare-function evil-window-move-very-top "evil-commands")
(declare-function evil-window-move-very-bottom "evil-commands")
(declare-function evil-window-move-far-left "evil-commands")
(declare-function evil-window-move-far-right "evil-commands")

(defgroup smartwin nil
  "A minor mode for emacs to show shell like buffers"
  :prefix "smartwin-"
  :version "0.1"
  :group 'convenience
  :group 'windows)

(defvar smartwin-previous-buffer nil
  "If smart window is hidden, this variable store previous buffer shown in it.")

(defvar smartwin-window-status 0 "0: hidden, 1: shrinked, 2: enlarged.")

(defvar smartwin-max-window-height (/ (frame-height) 3)
  "Maximum hight of smart window.
But smart window can be higher if run `delete-other-window' when is is already
  to this height.")

(defvar smartwin-min-window-height (+ 1 window-min-height)
  "Minimum hight of smart window.")

(defcustom smartwin-buffers
  '(;; Emacs
    "*scratch*"
    ;; shell and eshell buffers
    ("^\\*e?shell\\*\\(<.*>\\)?$" :regexp t)
    compilation-mode
    )
  "Configuration of buffers to be shown in smart window."
  :type '(repeat
          (cons :tag "Config"
                (choice :tag "Pattern"
                        (string :tag "Buffer Name")
                        (symbol :tag "Major Mode"))
                (plist :tag "Keywords"
                       :value (:regexp nil)
                       :options
                       ((:regexp (boolean :tag "On/Off"))))))
  :get (lambda (symbol)
         (mapcar (lambda (element)
                   (if (consp element)
                       element
                     (list element)))
                 (default-value symbol)))
  :group 'smartwin)

(defun smartwin--match-buffer (buffer-or-name)
  "Judge whether to use smartwin to show BUFFER-OR-NAME."
  (let ((buffer (get-buffer buffer-or-name)))
    (if buffer
        (let ((name (buffer-name buffer))
              (mode (buffer-local-value 'major-mode buffer)))
          (catch 'break
            (dolist (config smartwin-buffers)
              (let ((pattern (if (consp config) (car config) config))
                    (keywords (if (consp config) (cdr config) nil)))
                (if (cond ((eq pattern t) (not (buffer-file-name buffer)))
                          ((and (stringp pattern) (plist-get keywords :regexp))
                           (string-match pattern name))
                          ((stringp pattern)
                           (string= pattern name))
                          ((symbolp pattern)
                           (eq pattern mode))
                          ((functionp pattern)
                           (funcall pattern buffer))
                          (t (error "Invalid pattern: %s" pattern)))
                    (throw 'break config)))))))))

(defun smartwin--kill-buffer-when-shell-exit (&optional buffer)
  "Kill the buffer on exit of interactive shell.
if BUFFER is nil, use `current-buffer'."
  (let* ((buf (or buffer (current-buffer)))
         (process (get-buffer-process buf)))
    (if process
        (set-process-sentinel
         process
         #'(lambda (process state)
             (when (or (string-match "exited abnormally with code." state)
                       (string-match "\\(finished\\|exited\\)" state))
               (kill-buffer (process-buffer process))
               ))))))

(defun smartwin--shrink-window (window &optional force)
  "Try to shrink smart WINDOW if `smartwin-window-status' is not 1.
If FORCE is not nil, do shrink even `smartwin-window-status' is 1."
  (when (or force (not (eq 1 smartwin-window-status)))
    (setq smartwin-window-status 1)
    (with-selected-window window
      (let ((size-fixed window-size-fixed))
        (setq window-size-fixed nil)
        (if (> (window-height window) smartwin-min-window-height)
            (minimize-window window))
        (setq window-size-fixed size-fixed)))))

(defun smartwin--enlarge-window (window &optional enforce-max-p)
  "Try to enlarge smart WINDOW if `smartwin-window-status' is not 2.
If ENFORCE-MAX-P is not nil, try to maximize WINDOW."
  (with-selected-window window
    (when (or enforce-max-p (and (not (eq 2 smartwin-window-status))
                                 (not (eq major-mode 'fundamental-mode))))
      (setq smartwin-window-status 2)
      (let ((height-before (window-height window))
            (window-start (window-start))
            (window-end (window-end))
            max-delta)
        (when (or enforce-max-p (< (window-height window) smartwin-max-window-height))
          (setq window-size-fixed nil)
          (setq max-delta (if enforce-max-p (window-max-delta window)
                            (- smartwin-max-window-height height-before)))
          (if (window-resizable (selected-window) max-delta)
              (window-resize (selected-window) max-delta))
          (setq window-size-fixed t))
        ;; set smart window start
        (if (> (window-height window) height-before)
            (let ((forward-line-num (- height-before (window-height window)))
                  left-to-forward)
              (set-window-start
               window
               (let (return done)
                 (while (not done)
                   (save-excursion
                     (goto-char window-start)
                     (setq left-to-forward (forward-line forward-line-num))
                     (setq return (window-point window))
                     (if (or (eq window-end (window-end))
                             left-to-forward
                             (>= forward-line-num 0))
                         (setq done t)
                       (setq forward-line-num (+ forward-line-num 1)))
                     ))
                 return)
               t)))))))

(defun smartwin--get-bottom-window ()
  "Try to get bottom window.
A bottom is such a window: it not the only normal(non minibuffer) window and
it's width is equal to frame width."
  (let ((root-tree (car (window-tree (selected-frame)))))
    (if (and (listp root-tree) (car root-tree))
        (let ((bottom-window (car (last root-tree))))
          (if (and (> (length (window-list)) 1) (windowp bottom-window))
              bottom-window
            nil)))))

(defun smartwin--smart-window-p (window)
  "To judge whether WINDOW is smart window or not."
  (and window
       (windowp window)
       (eq window (smartwin--get-bottom-window))
       (smartwin--match-buffer (window-buffer window))))

(defun smartwin--get-smart-window ()
  "Find smart window among all live windows.
If found, return the window, else return nil."
  (let ((bottom-window (window-last-child (frame-root-window))))
    (if (smartwin--smart-window-p bottom-window)
        bottom-window
      nil)))

(defun smartwin--get-befitting-window ()
  "Try to get an existing befitting bottom window as smart window."
  (let ((bottom-window (smartwin--get-bottom-window)))
    (if (and (windowp bottom-window)
             (<= (window-height bottom-window)
                 smartwin-min-window-height))
        bottom-window
      nil)))

(defun smartwin--create-befitting-window ()
  "Try to split root window to create smart window."
  (let* ((root-win (frame-root-window))
         (root-height (window-height root-win)))
    (if (frame-parameter nil 'unsplittable)
        nil
      (with-demoted-errors "Warning: %S"
        (split-window root-win (- root-height smartwin-min-window-height)
                      'below)))))

(defun smartwin--set-window-buffer (window buffer)
  "Set WINDOW's buffer to BUFFER."
  (let ((old-buffer (window-buffer window)))
    (unless (eq old-buffer buffer)
      ;; set old buffer window-size-fixed to nil
      (with-current-buffer old-buffer (setq window-size-fixed nil))
      (with-current-buffer buffer (setq window-size-fixed t))
      (set-window-dedicated-p window nil)
      (set-window-buffer window buffer)
      (set-window-dedicated-p window t))))

(defun smartwin--get-or-create-smart-window ()
  "Get or create the smart window.
If succeed, a smart window is returned, else return nil."
  (let ((window (smartwin--get-smart-window)))
    (if window
        window
      (setq window (or (smartwin--get-befitting-window)
                       (smartwin--create-befitting-window)))
      (when window
        (smartwin--set-window-buffer window (or (get-buffer "*scratch*")
                                                (smartwin-create-scratch-buffer)))
        (set-window-prev-buffers window nil))
      window)))

;;; work with others
;; ediff
(add-hook 'ediff-before-setup-hook 'smartwin-hide)

;;; advices

(defadvice evil-window-move-far-left
    (around smartwin-around-evil-window-move-far-left)
  "When `evil-window-move-far-left', ignore smart window."
  (if (not (smartwin--smart-window-p (selected-window)))
      (if (smartwin--get-smart-window)
          (progn
            (smartwin-hide)
            ad-do-it
            (smartwin-show))
        ad-do-it)
    (message "Attempt to move smart window")))

(defadvice evil-window-move-far-right
    (around smartwin-around-evil-window-move-far-right)
  "When `evil-window-move-far-right', ignore smart window."
  (if (not (smartwin--smart-window-p (selected-window)))
      (if (smartwin--get-smart-window)
          (progn
            (smartwin-hide)
            ad-do-it
            (smartwin-show))
        ad-do-it)
    (message "Attempt to move smart window")))

(defadvice evil-window-move-very-top
    (around smartwin-around-evil-window-move-very-top)
  "When `evil-window-move-very-top', ignore smart window."
  (if (not (smartwin--smart-window-p (selected-window)))
      (if (smartwin--get-smart-window)
          (progn
            (smartwin-hide)
            ad-do-it
            (smartwin-show))
        ad-do-it)
    (message "Attempt to move smart window")))

(defadvice evil-window-move-very-bottom
    (around smartwin-around-evil-window-move-very-bottom)
  "When `evil-window-move-very-bottom', ignore smart window."
  (if (not (smartwin--smart-window-p (selected-window)))
      (if (smartwin--get-smart-window)
          (progn
            (smartwin-hide)
            ad-do-it
            (smartwin-show))
        ad-do-it)
    (message "Attempt to move smart window")))

(defadvice gdb (before smartwin-before-gdb)
  "When run `gdb', hide smart window."
  (smartwin-hide))

(defadvice switch-to-buffer (around smartwin-around-switch-to-buffer)
  "When a bffer should be shown in smart window, show it in smart window."
  (let ((buffer-or-name (ad-get-arg 0)))
    (if (smartwin--match-buffer buffer-or-name)
        (let ((window (smartwin--get-or-create-smart-window)))
          (smartwin--set-window-buffer window buffer-or-name)
          (select-window window))
      ad-do-it)))

(defadvice switch-to-buffer-other-window
    (around smartwin-around-switch-to-buffer-other-window)
  "When a bffer should be show in smart window, chage window to smart window."
  (let ((buffer-or-name (ad-get-arg 0)))
    (if (smartwin--match-buffer buffer-or-name)
        (let ((window (smartwin--get-or-create-smart-window)))
          (smartwin--set-window-buffer window buffer-or-name)
          (select-window window))
      ad-do-it)))

(defadvice delete-other-windows (around smartwin-around-delete-other-windows)
  "If in smart window, just enlarge it instead of `delete-other-windows'."
  (let ((win-to-del (or (ad-get-arg 0) (selected-window)))
        (smart-window (smartwin--get-smart-window)))
    (if (eq win-to-del smart-window)
        ;; just enlarge smart window, do not leave it as only window
        (smartwin--enlarge-window smart-window t)
      (if smart-window
          ;; try to keep smart window if number of windows is larger than 2
          (if (= (length (window-list)) 2) ;; need to delete smart window
              (progn
                (setq smartwin-previous-buffer (window-buffer smart-window))
                (with-current-buffer smartwin-previous-buffer
                  (setq window-size-fixed nil))
                ad-do-it)
            ;; not delete smart window
            (smartwin-hide)
            ad-do-it
            (smartwin-show))
        ad-do-it))))

(defadvice delete-window (around smartwin-around-delete-window)
  "If the window is last oridnary window(not smart window), do not kill it."
  (let ((window (ad-get-arg 0)))
    (if (not window) (setq window (selected-window)))
    (if (smartwin--smart-window-p window)
        (progn
          (setq smartwin-previous-buffer (window-buffer window))
          (with-current-buffer smartwin-previous-buffer
            (setq window-size-fixed nil))
          ad-do-it)
      (if (and (smartwin--get-smart-window) (eq 2 (length (window-list))))
          (progn
            (message "Attempt to delete last normal(non smart) window")
            (setq ad-return-value nil))
        ad-do-it))))

;;; Minor Mode

(defun smartwin--display-buffer-condition (buffer-or-name _action)
  "Part of pair in `display-buffer-alist'.
BUFFER-OR-NAME is a buffer name to match, _ACTION is not used."
  (smartwin--match-buffer buffer-or-name))

(defun smartwin--display-buffer-action (buffer-or-name _alist)
  "Part of pair in `display-buffer-alist'.
BUFFER-OR-NAME is a buffer to display, _ALIST is not used."
  (let ((buffer (window-normalize-buffer-to-switch-to buffer-or-name))
        (window (smartwin--get-or-create-smart-window)))
    (smartwin--set-window-buffer window buffer)
    window))

(defun smartwin-hide ()
  "Hide smart window, store its buffer as previous buffer."
  (interactive)
  (setq smartwin-window-status 0)
  (let ((window (smartwin--get-smart-window)))
    (when window
      (setq smartwin-previous-buffer (window-buffer window))
      (with-current-buffer smartwin-previous-buffer
        (setq window-size-fixed nil))
      (delete-window window))))

(defun smartwin--make-smart-buffer-list ()
  "Return smart buffer list that not shown in smartwin."
  (let ((visible-buffers (ido-get-buffers-in-frames 'current))
        (buffers (delq nil
                       (mapcar
                        (lambda (b)
                          (let ((name (buffer-name b)))
                            (if (smartwin--match-buffer b)
                                name)))
                        (buffer-list)))))
    ;; move visible buffers to end of buffer list
    (mapc #'(lambda (b)
              (when (memq b buffers)
                (setq buffers (delq b buffers))
                (setq buffers (append buffers (list b)))))
          visible-buffers)
    buffers))

(defun smartwin-switch-buffer ()
  "Pop buffer that can be showed in smartwin.
This function get input by ido."
  (interactive)
  (let* ((smartwin-buffers (smartwin--make-smart-buffer-list))
         (chosen (and smartwin-buffers
                      (ido-completing-read "Smartwin:" smartwin-buffers)))
         (buffer (and chosen
                      (not (string= chosen ""))
                      (get-buffer chosen))))
    (if (not buffer)
        (message (format "Buffer %s not exist" chosen))
      (let ((window (smartwin--get-or-create-smart-window)))
        (smartwin--set-window-buffer window buffer)
        (select-window window)))))

(defun smartwin-create-scratch-buffer ()
  "Create a scratch buffer with the scratch message."
  (interactive)
  (unless (get-buffer "*scratch*")
    (with-current-buffer (get-buffer-create "*scratch*")
      (progn
        (insert initial-scratch-message)
        (goto-char (point-max))
        (lisp-interaction-mode)
        (current-buffer)))))

(defun smartwin--get-C-l-command ()
  "Get the command <tab> should be bound.
According to `major-mode' that yasnippet is not enabled and the
`global-map'."
  (let ((major-mode-map
         (symbol-value (intern-soft (concat (symbol-name major-mode) "-map")))))
    (or (and major-mode-map
             (lookup-key major-mode-map (kbd "C-l")))
        (lookup-key global-map (kbd "C-l")))))

(defun smartwin-clear-shell ()
  "Clear `eshell' or submode of `comint-mode' buffer."
  (interactive)
  (cond ((eq major-mode 'eshell-mode)
         (let ((eshell-buffer-maximum-lines 0))
           (eshell-truncate-buffer)))
        ((derived-mode-p 'comint-mode)
         (let ((comint-buffer-maximum-size 0))
           (comint-truncate-buffer)))
        (t (command-execute (smartwin--get-C-l-command)))))

(defun smartwin-clear-scratch-buffer (&optional buffer-or-name)
  "Clear the scratch buffer and keep the scratch message.
If BUFFER-OR-NAME is not nil, treat is as scratch buffer."
  (interactive)
  (let ((buffer (if (not buffer-or-name)
                    (get-buffer "*scratch*")
                  (get-buffer buffer-or-name))))
    (if initial-scratch-message
        (with-current-buffer buffer
          (delete-region (point-min) (point-max))
          (insert initial-scratch-message)
          (set-buffer-modified-p nil)
          (goto-char (point-max))))))

(defun smartwin--auto-enlarge-shrink-hook ()
  "Automatically enlarge or shrink smart window when do `select-window'."
  (let ((window (smartwin--get-smart-window)))
    (when window
      (if (eq (selected-window) window)
          (smartwin--enlarge-window window)
        (smartwin--shrink-window window)))))

(defun smartwin--protect-scratch-buffer ()
  "Make *scratch* buffer unkillable."
  (let* ((smart-win (smartwin--get-smart-window))
         (scratch-buffer (get-buffer "*scratch*"))
         (window (selected-window))
         (buffer-to-kill (current-buffer))
         (hide-p (and (eq window smart-win)
                      (eq buffer-to-kill (window-buffer smart-win)))))
    (if (eq buffer-to-kill scratch-buffer) ;; not kill scratch buffer, clear it
        (progn (smartwin-clear-scratch-buffer buffer-to-kill) nil)
      (if hide-p (smartwin-hide))
      t)))

;;;###autoload
(define-minor-mode smartwin-mode
  "Toggle smartwin minor mode.
Smartwin is a window for showing shell like buffers, temp buffers and etc."
  :lighter " sw"
  :init-value nil
  :global t
  :group 'smartwin
  ;; The minor mode bindings.
  :keymap (make-sparse-keymap)
  (let ((pair '(smartwin--display-buffer-condition
                smartwin--display-buffer-action)))
    (if smartwin-mode
        (progn
          (push pair display-buffer-alist)

          (ad-activate #'evil-window-move-very-top)
          (ad-activate #'evil-window-move-far-left)
          (ad-activate #'evil-window-move-far-right)
          (ad-activate #'evil-window-move-very-bottom)
          (ad-activate #'gdb)
          (ad-activate #'switch-to-buffer)
          (ad-activate #'switch-to-buffer-other-window)
          (ad-activate #'delete-window)
          (ad-activate #'delete-other-windows)

          (add-hook 'buffer-list-update-hook #'smartwin--auto-enlarge-shrink-hook)
          (add-hook 'comint-mode-hook #'smartwin--kill-buffer-when-shell-exit)
          (add-hook 'kill-emacs-hook #'smartwin-hide)
          (add-hook 'kill-buffer-query-functions #'smartwin--protect-scratch-buffer)

          (smartwin-create-scratch-buffer)

          (if (and (buffer-live-p smartwin-previous-buffer)
                   (smartwin--match-buffer smartwin-previous-buffer)
                   (not (eq (get-buffer "*scratch*") smartwin-previous-buffer)))
              (smartwin-show)))

      (remove-hook 'comint-mode-hook #'smartwin--kill-buffer-when-shell-exit)
      (remove-hook 'kill-emacs-hook #'smartwin-hide)
      (remove-hook 'buffer-list-update-hook #'smartwin--auto-enlarge-shrink-hook)
      (remove-hook 'kill-buffer-query-functions #'smartwin--protect-scratch-buffer)

      (setq display-buffer-alist (delete pair display-buffer-alist))

      (ad-deactivate #'evil-window-move-very-top)
      (ad-deactivate #'evil-window-move-far-left)
      (ad-deactivate #'evil-window-move-far-right)
      (ad-deactivate #'evil-window-move-very-bottom)
      (ad-deactivate #'gdb)
      (ad-deactivate #'switch-to-buffer)
      (ad-deactivate #'switch-to-buffer-other-window)
      (ad-deactivate #'delete-window)
      (ad-deactivate #'delete-other-windows)

      (smartwin-hide))))

(defun smartwin-show ()
  "Show smart window."
  (interactive)
  (if smartwin-mode
      (let ((window (smartwin--get-smart-window)))
        (when (not window)
          (setq window (smartwin--get-or-create-smart-window))
          (if (and smartwin-previous-buffer
                   (buffer-live-p smartwin-previous-buffer))
              (when (not (eq smartwin-previous-buffer (window-buffer window)))
                (smartwin--set-window-buffer window smartwin-previous-buffer))))
        (if (called-interactively-p 'interactive)
            (progn
              (select-window window)
              (smartwin--enlarge-window window))))
    (message "Smartwin-mode is not enabled, do nothing")))

(provide 'smartwin)

;;; smartwin.el ends here
