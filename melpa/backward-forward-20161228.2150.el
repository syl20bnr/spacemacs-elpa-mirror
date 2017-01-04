;;; backward-forward.el --- navigation backwards and forwards across marks

;; Copyright (C) 2016 Currell Berry

;; Author: Currell Berry <currellberry@gmail.com>
;; Keywords: navigation convenience backward forward
;; Homepage: https://gitlab.com/vancan1ty/emacs-backward-forward/tree/master
;; Version: 0.1
;; Package-Version: 20161228.2150
;; Package-X-Original-Version: 20161221.1
;; Package-Requires: ((emacs "24.5"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Summary:
;; this package provides eclipse-like forward/backward navigation
;; bound by default to <C-left> (backward-forward-previous-location)
;; and <C-right> (backward-forward-next-location)
;;
;; More Info:
;; backward-forward hooks onto "push-mark" operations and keeps
;; track of all such operations in a global list of marks called backward-forward-mark-ring
;; this enables easy navigation forwards and backwards in your history
;; of marked locations using <C-left> and <C-right> (or feel free to change the keybindings).
;;
;; Many Emacs commands (such as searching or switching buffers with certain packages enabled)
;; invoke push-mark.
;; Other Emacs commands can be configured to invoke push mark using the system below:
;;      (advice-add 'ggtags-find-tag-dwim :before #'backward-forward-push-mark-wrapper)
;;  You can see examples of the above convention below.
;;
;; Use C-h k to see what command a given key sequence is invoking.
;;
;; to use this package, install though the usual Emacs package install mechanism
;; then put the following in your .emacs
;;
;;    ;(setf backward-forward-evil-compatibility-mode t) ;the line to the left is optional,
;;          ; and recommended only if you are using evil mode
;;    (backward-forward-mode t)
;;
;;
;; | Commmand                | Keybinding |
;; |-------------------------+------------|
;; | backward-forward-previous-location | <C-left>   |
;; | backward-forward-next-location     | <C-right>  |

;;; Code:
(require 'cl-lib)

(defvar backward-forward-evil-compatibility-mode nil
  "If true, sets up for better UX when using evil.")

(defvar backward-forward-mark-ring nil
  "The list of saved marks, bringing together the global mark ring and the local mark ring into one ring.")

(defvar backward-forward-mark-ring-max 32
  "Maximum size of overall mark ring.  Start discarding off end if gets this big.")

(defvar backward-forward-mark-ring-traversal-position 0
  "Stores the traversal position within the backward-forward-mark-ring.
Gets modified by backward-forward-previous-location and
backward-forward-next-location.
Gets reset to zero whenever backward-forward-after-push-mark runs.")

(defvar backward-forward-in-progress nil
  "Suppresses generation of marks in backward-forward-ring.
Dynamically bound to during the navigation process.")

;;;###autoload
(define-minor-mode backward-forward-mode
  "enables or disable backward-forward minor mode.

when backward-forward mode is enabled, it keeps track of mark pushes across
all buffers in a variable backward-forward-mark-ring, and allows you to navigate backwards
and forwards across these marks using <C-left> and <C-right>.  to customize
the navigation behavior one must customize the mark pushing behavior --
add 'advice' to a command to make it push a mark before invocation if you
want it to be tracked.  see backward-forward.el for examples and more
information.
"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<C-left>") #'backward-forward-previous-location)
            (define-key map (kbd "<C-right>") #'backward-forward-next-location)
            map
            )
  :global t
  (if backward-forward-mode
      (progn
        (advice-add 'push-mark :after #'backward-forward-after-push-mark)
        (advice-add 'ggtags-find-tag-dwim :before #'backward-forward-push-mark-wrapper)
        (unless backward-forward-evil-compatibility-mode
          (advice-add 'switch-to-buffer :before #'backward-forward-push-mark-wrapper))
        )
    (progn
        (advice-remove 'push-mark #'backward-forward-after-push-mark)
        (advice-remove 'ggtags-find-tag-dwim #'push-mark)
        (advice-remove 'switch-to-buffer #'backward-forward-push-mark-wrapper)
        )))

(defun backward-forward-after-push-mark (&optional location nomsg activate)
  "Handles mark-tracking work for backward-forward.
ignores its arguments LOCATION, NOMSG, ACTIVATE
Uses following steps:
zeros backward-forward-mark-ring-traversal-position
pushes the just-created mark by `push-mark' onto backward-forward-mark-ring
\(If we exceed backward-forward-mark-ring-max then old marks are pushed off\)

note that perhaps this should establish one ring per window in the future"
(if (not backward-forward-in-progress)
    (progn
;;      (message "backward-forward-after-push-mark %S %S %S" location nomsg activate)
      (setf backward-forward-mark-ring-traversal-position 0)
      (unless (null (mark t))
        (let* ((marker (mark-marker))
               (position (marker-position marker))
               (buffer (marker-buffer marker)))
          ;;don't insert duplicate marks
          (if (or (eql (length backward-forward-mark-ring) 0)
                  (not (and (eql position (marker-position (elt backward-forward-mark-ring 0)))
                            (eql buffer (marker-buffer (elt backward-forward-mark-ring 0))))))
              (progn
;;                (message "pushing marker %S" marker)
                (setq backward-forward-mark-ring (cons (copy-marker marker) backward-forward-mark-ring)))))
        ;;purge excess entries from the end of the list
        (when (> (length backward-forward-mark-ring) backward-forward-mark-ring-max)
          (move-marker (car (nthcdr backward-forward-mark-ring-max backward-forward-mark-ring)) nil)
          (setcdr (nthcdr (1- backward-forward-mark-ring-max) backward-forward-mark-ring) nil))))
  ;;  (message "f/b in progress!")
  ))
          
(defun backward-forward-go-to-marker (marker)
  "See pop-to-global-mark for where most of this code came from.
Argument MARKER the marker, in any buffer, to go to."
  (let* ((buffer (marker-buffer marker))
         (position (marker-position marker))
         (backward-forward-in-progress t))
    (if (null buffer)
        (message "buffer no longer exists.")
      (progn
        (if (eql buffer (current-buffer))
            (goto-char marker)
          (progn
            (set-buffer buffer)
            (or (and (>= position (point-min))
                     (<= position (point-max)))
                (if widen-automatically
                    (widen)
                  (error "Global mark position is outside accessible part of buffer")))
            (goto-char position)
            (switch-to-buffer buffer)))))))

(defun backward-forward-previous-location ()
  "Used to navigate to the previous position on backward-forward-mark-ring.
1. Increments backward-forward-mark-ring-traversal-position.
2. Jumps to the mark at that position.
Borrows code from `pop-global-mark'."
  (interactive)
  (if (and (eql backward-forward-mark-ring-traversal-position 0)
           (not
            (and (eql (marker-buffer (elt backward-forward-mark-ring 0)) (current-buffer))
                  (eql (marker-position (elt backward-forward-mark-ring 0)) (point)))))
      ;;then we are at the beginning of our navigation chain and we want to mark the current position
      (push-mark))
  (if (< backward-forward-mark-ring-traversal-position (1- (length backward-forward-mark-ring)))
      (cl-incf backward-forward-mark-ring-traversal-position)
    (message "no more marks to visit!"))
  (let* ((marker (elt backward-forward-mark-ring backward-forward-mark-ring-traversal-position)))
    (backward-forward-go-to-marker marker)))

;;(marker-buffer (elt backward-forward-mark-ring 3))

(defun backward-forward-next-location ()
    "Used to navigate to the next position on backward-forward-mark-ring.
1. Decrements backward-forward-mark-ring-traversal-position.
2. Jumps to the mark at that position.
Borrows code from `pop-global-mark'."
  (interactive)
  (if (> backward-forward-mark-ring-traversal-position 0)
      (cl-decf backward-forward-mark-ring-traversal-position)
    (message "you are already at the most current mark!"))
  (let* ((marker (elt backward-forward-mark-ring backward-forward-mark-ring-traversal-position)))
    (backward-forward-go-to-marker marker)))

(defun backward-forward-push-mark-wrapper (&rest args)
  "Allows one to bind push-mark to various commands of your choosing.
Optional argument ARGS completely ignored"
  (push-mark))

;;(global-set-key (kbd "<C-left>") 'backward-forward-previous-location)
;;(global-set-key (kbd "<C-right>") 'backward-forward-next-location)
;;(defun my-tracing-function (&optional location nomsg activate)
;;  (message "push-mark %S %S %S" location nomsg activate)
;;  (backward-forward-after-push-mark location nomsg activate))

;;(elt backward-forward-mark-ring 0)
;;(define-key (current-global-map) (kbd "C-[") nil)
;;(define-key (current-global-map) (kbd "C-]") nil)
;;(define-key (current-global-map) (kbd "<M-left>") 'backward-forward-previous-location)
;;(define-key (current-global-map) (kbd "<M-right>") 'backward-forward-next-location)
;;(global-set-key (kbd "<M-left>") 'backward-forward-previous-location)
;;(global-set-key (kbd "<M-right>") 'backward-forward-next-location)

;;(advice-remove 'push-mark #'my-tracing-function)
;;(selected-window)
;; possibly need to combine the marking functionality
;; and the buffer-undo-list
;; (self-insert-command) runs post-self-insert-hook after it is done.  need to add something on to that in order to push an entry onto
;; my undo list if necessary
;; listen to mouse-set-point?
;;(defun backward-forward-post-insert-function ()
;;  
;;  )
;;(setf backward-forward-mark-ring nil)

(provide 'backward-forward)
;;; backward-forward.el ends here
