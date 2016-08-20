;;; seclusion-mode.el --- Edit in seclusion. A Dark Room mode.

;; Copyright 2012 Daniel Leslie
;; Author: Daniel Leslie dan@ironoxide.ca
;; URL: http://github.com/dleslie/seclusion-mode
;; Package-Version: 20121118.1553
;; Version: 1.1.1

;; Licensed under the GPL3
;; A copy of the license can be found at the above URL

;;; Commentary:
;; Not actually a major/minor mode, but a frame configuration for total seclusion. A new frame is opened on the current buffer with as little clutter as possible and in fullscreen.

;;; Code:

(defgroup seclusion-mode
  nil "Seclusion Mode")

(defcustom
  seclusion-hide-minibuffer t
  "Should the minibuffer be hidden?"
  :type 'boolean
  :group 'seclusion-mode)

(defcustom
  seclusion-hide-modeline t
  "Should the minibuffer be hidden?"
  :type 'boolean
  :group 'seclusion-mode)

(defcustom
  seclusion-hide-menu t
  "Should the menubar be hidden?"
  :type 'boolean
  :group 'seclusion-mode)

(defcustom
  seclusion-hide-toolbar t
  "Should the toolbar be hidden?"
  :type 'boolean
  :group 'seclusion-mode)

(defcustom
  seclusion-hide-fringe-clutter t
  "Should the fringe clutter be hidden?"
  :type 'boolean
  :group 'seclusion-mode)

(defcustom
  seclusion-fringe-factor 4
  "The dividend of the 1/n of the screen the fringes consume."
  :type 'integer
  :group 'seclusion-mode)

(defun seclusion-mode (command)
      "Window mode to hide all the clutter and help you focus."
      (interactive "CEnter command to run:")
      ;; New window
      (let ((options '((fullscreen . t))))
        (if seclusion-hide-menu
            (setq options (cons '(menu-bar-lines . 0) options)))
        (if seclusion-hide-minibuffer
            (setq options (cons '(minibuffer . nil) options)))
        (if seclusion-hide-toolbar
            (setq options (cons '(tool-bar-lines . 0) options)))
        (select-frame (make-frame options)))
      ;; Execute the user command, but don't trip and fall on it
      (condition-case err
          (call-interactively command)
          (error (princ (format "Error occurred in executing client command: %s" err))))
      ;; Go full screen (Emacs 24+)
      (set-frame-parameter nil 'fullscreen (when (not (frame-parameter nil 'fullscreen)) 'fullboth))
      ;; Make some nice fat fringes
      (let* ((dim (window-pixel-edges))
             (size (/ (- (nth 2 dim) (nth 0 dim)) seclusion-fringe-factor)))
        (set-window-fringes nil size size))
      ;; Kill the mode line
      (if seclusion-hide-modeline (setq mode-line-format nil))
      ;; Kill the fringe clutter
      (if seclusion-hide-fringe-clutter
          (setq fringe-indicator-alist '((truncation . nil)
                                         (continuation . nil)
                                         (up . nil)
                                         (down . nil)
                                         (top . nil)
                                         (bottom . nil)
                                         (top-bottom .nil)
                                         (empty-line . nil)
                                         (overlay-arrow . nil)))))

(provide 'seclusion-mode)

;;; seclusion-mode.el ends here
