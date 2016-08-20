;;; temp-buffer-browse.el --- temp buffer browse mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2016  Free Software Foundation, Inc.

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 1.5
;; Package-Requires: ((emacs "24"))
;; Keywords: convenience

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

;; Allow keys `SPC', `DEL' and `RET' following a temp buffer popup to
;; scroll up, scroll down and close the temp buffer window,
;; respectively.

;;; Code:

;; fringe not preloaded for tty emacs
(eval-when-compile (require 'fringe))

(eval-and-compile
  (cond
   ((fboundp 'set-transient-map) nil)
   ((fboundp 'set-temporary-overlay-map) ; new in 24.3
    (defalias 'set-transient-map 'set-temporary-overlay-map))
   (t
    (defun set-transient-map (map &optional keep-pred)
      (let* ((clearfunsym (make-symbol "clear-temporary-overlay-map"))
             (overlaysym (make-symbol "t"))
             (alist (list (cons overlaysym map)))
             (clearfun
              `(lambda ()
                 (unless ,(cond ((null keep-pred) nil)
                                ((eq t keep-pred)
                                 `(eq this-command
                                      (lookup-key ',map
                                                  (this-command-keys-vector))))
                                (t `(funcall ',keep-pred)))
                   (set ',overlaysym nil) ;Just in case.
                   (remove-hook 'pre-command-hook ',clearfunsym)
                   (setq emulation-mode-map-alists
                         (delq ',alist emulation-mode-map-alists))))))
        (set overlaysym overlaysym)
        (fset clearfunsym clearfun)
        (add-hook 'pre-command-hook clearfunsym)
        (push alist emulation-mode-map-alists))))))

(defcustom temp-buffer-browse-fringe-bitmap 'centered-vertical-bar
  "Fringe bitmap to use in the temp buffer window."
  :type `(restricted-sexp :match-alternatives
                          (,(lambda (s) (and (symbolp s) (fringe-bitmap-p s)))))
  :group 'help)

(defvar temp-buffer-browse--window nil)

;; See http://debbugs.gnu.org/15497
(when (and (fboundp 'define-fringe-bitmap) ;only defined in GUI.
           (not (fringe-bitmap-p 'centered-vertical-bar)))
  (define-fringe-bitmap 'centered-vertical-bar [24] nil nil '(top t)))

(defvar temp-buffer-browse-map
  (let ((map (make-sparse-keymap))
        (quit (lambda ()
                (interactive)
                (when (window-live-p temp-buffer-browse--window)
                  (quit-window nil temp-buffer-browse--window))))
        (up (lambda ()
              (interactive)
              (when (window-live-p temp-buffer-browse--window)
                (with-selected-window temp-buffer-browse--window
                  (condition-case nil
                      (scroll-up)
                    (end-of-buffer (quit-window)))))))
        (down (lambda ()
                (interactive)
                (when (window-live-p temp-buffer-browse--window)
                  (with-selected-window temp-buffer-browse--window
                    (scroll-up '-))))))
    (define-key map "\C-m" quit)
    (define-key map [return] quit)
    (define-key map " " up)
    (define-key map (kbd "DEL") down)
    (define-key map [delete] down)
    (define-key map [backspace] down)
    map))

(defvar temp-buffer-browse--last-exit #'ignore
  "The \"exit-function\" of the last call to `set-transient-map'.")

;;;###autoload
(defun temp-buffer-browse-activate ()
  "Activate temporary key bindings for current window.
Specifically set up keys `SPC', `DEL' and `RET' to scroll up,
scroll down and close the temp buffer window, respectively."
  (unless (derived-mode-p 'completion-list-mode)
    (setq temp-buffer-browse--window (selected-window))
    ;; When re-using existing window don't call
    ;; `fit-window-to-buffer'. See also (info "(elisp)Window
    ;; Parameters").
    (when (and (window-full-width-p)
               (memq (cadr (window-parameter nil 'quit-restore))
                     '(window frame)))
      (fit-window-to-buffer nil (floor (frame-height) 2))
      ;; In case buffer contents are inserted asynchronously such as
      ;; in `slime-inspector-mode'.
      (add-hook 'after-change-functions
                (let ((time (float-time)))
                  (lambda (&rest _)
                    (when (> (float-time) (+ 0.05 time))
                      (fit-window-to-buffer nil (floor (frame-height) 2))
                      (setq time (float-time)))))
                nil 'local))
    (let ((o (make-overlay (point-min) (point-max))))
      (overlay-put o 'evaporate t)
      (overlay-put o 'window t)
      (overlay-put o 'line-prefix
                   (propertize
                    "|" 'display
                    (unless (zerop (or (frame-parameter nil 'left-fringe) 0))
                      `(left-fringe ,temp-buffer-browse-fringe-bitmap warning))
                    'face 'warning))
      ;; NOTE: breaks `adaptive-wrap-prefix-mode' because overlay's
      ;; wrap-prefix overrides text property's. Overlay's cannot have
      ;; negative priority.
      (unless (bound-and-true-p adaptive-wrap-prefix-mode)
        (overlay-put o 'wrap-prefix (overlay-get o 'line-prefix)))
      ;; Workaround for bug http://debbugs.gnu.org/24149.
      (funcall temp-buffer-browse--last-exit)
      (setq temp-buffer-browse--last-exit
            (set-transient-map
             temp-buffer-browse-map
             (lambda ()
               ;; If uncaught any error will make the keymap active
               ;; forever.
               (condition-case err
                   (or (and (window-live-p temp-buffer-browse--window)
                            (not (member (this-command-keys) '("\C-m" [return])))
                            (eq this-command (lookup-key temp-buffer-browse-map
                                                         (this-command-keys))))
                       (ignore (setq temp-buffer-browse--last-exit #'ignore)
                               (overlay-put o 'line-prefix nil)
                               (overlay-put o 'wrap-prefix nil)))
                 (error (message "%s:%s" this-command (error-message-string err))
                        nil))))))))

;;;###autoload
(define-minor-mode temp-buffer-browse-mode nil
  :lighter ""
  :global t
  ;; Work around http://debbugs.gnu.org/16038
  (let ((activate (lambda ()
                    (unless (derived-mode-p 'fundamental-mode)
                      (temp-buffer-browse-activate)))))
    (if temp-buffer-browse-mode
        (progn
          (add-hook 'temp-buffer-show-hook 'temp-buffer-browse-activate t)
          (add-hook 'temp-buffer-window-show-hook activate t))
      (remove-hook 'temp-buffer-show-hook 'temp-buffer-browse-activate)
      (remove-hook 'temp-buffer-window-show-hook activate))))

;;;; ChangeLog:

;; 2016-08-05  Leo Liu  <sdl.web@gmail.com>
;; 
;; 	Merge branch 'master' of https://github.com/leoliu/temp-buffer-browse
;; 
;; 2014-12-19  Leo Liu  <sdl.web@gmail.com>
;; 
;; 	Merge branch 'master' of https://github.com/leoliu/temp-buffer-browse
;; 
;; 2014-10-16  Leo Liu  <sdl.web@gmail.com>
;; 
;; 	Merge branch 'master' of https://github.com/leoliu/temp-buffer-browse
;; 
;; 2014-02-23  Leo Liu  <sdl.web@gmail.com>
;; 
;; 	Merge remote-tracking branch 'tbb/master'
;; 
;; 2013-11-03  Leo Liu  <sdl.web@gmail.com>
;; 
;; 	Merge branch 'master' of https://github.com/leoliu/temp-buffer-browse
;; 
;; 2013-08-18  Leo Liu  <sdl.web@gmail.com>
;; 
;; 	Merge temp-buffer-browse as our subdirectory
;; 


(provide 'temp-buffer-browse)
;;; temp-buffer-browse.el ends here
