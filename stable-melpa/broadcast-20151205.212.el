;;; broadcast.el --- Links buffers together for simultaneous editing.

;; Copyright (C) 2014-2015 Russell Black

;; Author: Russell Black (killdash9@github)
;; Keywords: convenience, frames, link, cursors
;; Package-Version: 20151205.212
;; URL: https://github.com/killdash9/broadcast.el
;; Created: 9th December 2014
;; Version: 20150303
;; Package-Requires: ((emacs "24.4"))

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

;; This file provides a minor mode to broadcast edits and navigation in
;; one buffer to one or more other buffers.  This is similar to the idea
;; of multiple cursors, but takes place across multiple buffers.  To use
;; it, place two or more buffers in broadcast-mode with
;; 
;; M-x broadcast-mode
;; 
;; This links those buffers together so that edits, cursor navigation,
;; and even kill-ring operations made in one of the broadcast mode
;; buffers are replicated in the other buffers.  If a broadcast-mode
;; buffer is not visible, that is to say "buried," and not currently
;; displayed by any window, then it is not affected by actions performed
;; in other broadcast mode buffers.  Thus you can only edit what you can
;; see.
;; 
;; The Kill Ring
;; -------------
;; The kill rings in broadcast-mode buffers are independent, so each
;; buffer can kill and yank independent text.  At the same time, kill ring
;; operations are shared among buffers where it makes sense, allowing you
;; to kill in a non-broadcast buffer and yank into a broadcast buffer and
;; vice versa.  When killing in a broadcast buffer, each broadcast buffer
;; may place something different on their kill ring.  The text that is killed
;; in the primary broadcast buffer, (then one with focus), will also be placed
;; on the main kill ring for non-broadcast buffers.
;; 
;; Undo Boundaries
;; ---------------
;; In order to synchronize undo behavior between linked buffers, an undo boundary
;; is placed after every command.  This can mean undoing can take a little longer
;; if you're going back very far.

;;; Code:

;;; Macros
;; defining macros first so that the byte compiler will do the right thing.
(defmacro broadcast-foreach-broadcast-buffer (body)
  "Execute BODY for each broadcast buffer."
  `(mapc
    (lambda (buffer) (with-current-buffer buffer (when broadcast-mode ,body)))
    (buffer-list)))

(defmacro broadcast-command (body)
  "Evaluate BODY in all other visible broadcast mode buffers."
  `(let ((primary-buffer (current-buffer)))
     (mapc
      (lambda (buffer)
        (let ((window (get-buffer-window buffer)))
          (when (and window (not (eq buffer primary-buffer)))
            (with-selected-window window (when broadcast-mode ,body)))))
      (buffer-list))))

;;; Minor mode
;;;###autoload
(define-minor-mode broadcast-mode 
  "A minor mode for linking buffers together for simultaneous navigation and 
editing.

This is similar to the idea of multiple cursors, but takes place across
multiple buffers.  To use it, place two or more buffers in broadcast-mode with

    M-x broadcast-mode

This links those buffers together so that edits, cursor navigation,
and even kill-ring operations made in one of the broadcast mode
buffers are replicated in the other buffers.  If a broadcast-mode
buffer is not visible, that is to say \"buried\" and not currenlty
displayed by any window, then it is not affected by actions performed
in other broadcast mode buffers.  Thus you can only edit what you can
see.

The Kill Ring
-------------
The kill rings in broadcast-mode buffers are independent, so each
buffer can kill and yank independent text.  At the same time, kill ring
operations are shared among buffers where it makes sense, allowing you
to kill in a non-broadcast buffer and yank into a broadcast buffer and
vice versa.  When killing in a broadcast buffer, each broadcast buffer
may place something different on their kill ring.  The text that is killed
in the primary broadcast buffer, (then one with focus), will also be placed
on the main kill ring for non-broadcast buffers.

Undo Boundaries
---------------
In order to synchronize undo behavior between linked buffers, an undo boundary
is placed after every command.  This can mean undoing can take a little longer
if you're going back very far.
"
  nil " Broadcast" nil
  (if broadcast-mode
      (progn
        ;; command hooks
        (add-hook 'pre-command-hook 'broadcast-pre nil t)
        (add-hook 'post-command-hook 'broadcast-post t t)

        ;; local kill ring
        (make-local-variable 'kill-ring)
        (make-local-variable 'kill-ring-yank-pointer)

        ;; advice for handling kill events
        (advice-add 'current-kill :before #'broadcast-current-kill-advice)
        (advice-add 'kill-new     :around #'broadcast-kill-advice)
        (advice-add 'kill-append  :around #'broadcast-kill-advice)
        (advice-add 'current-kill :around #'broadcast-kill-advice))

    ;; unregister command hooks
    (remove-hook 'pre-command-hook 'broadcast-pre t)
    (remove-hook 'post-command-hook 'broadcast-post t)
    
    ;; get rid of local kill ring
    (kill-local-variable 'kill-ring)
    (kill-local-variable 'kill-ring-yank-pointer)

    ;; unregister advice when there are no more broadcast buffers
    (when (= 0 (let ((count 0))
                 (broadcast-foreach-broadcast-buffer
                  (setq count (1+ count))) count))
      (advice-remove 'current-kill #'broadcast-current-kill-advice)
      (advice-remove 'kill-new     #'broadcast-kill-advice)
      (advice-remove 'kill-append  #'broadcast-kill-advice)
      (advice-remove 'current-kill #'broadcast-kill-advice))
    
    (when current-prefix-arg
      (broadcast-command (broadcast-mode 0))
      ;; prefix arg of 0 means turn off all broadcast modes in active windows.
      ;; We have to broadcast the command 
      (message "Broadcast mode disabled for all visible buffers"))))

;;; Variables
(defvar broadcast-suppress nil
  "Used to prevent recursive calling of command hooks.")

(defvar broadcast-state nil
  "Holds a snapshot of relevant bits of buffer state.
It is set when the `pre-command-hook' `broadcast-pre' is called.  This is 
checked against the state in the `post-command-hook' `broadcast-post'.  If the 
state matches, the buffer is considered not to have changed significantly and 
the command is not broadcast to other visible broadcast-mode buffers.  Snapshots
are compared using `equals'.  See `broadcast-get-state'")

(defvar broadcast-window-configuration nil
  "Holds a snapshot of the current window configuration.
It is set when the `pre-command-hook' `broadcast-pre' is called.  It is checked 
against the current window configuration in the `post-command-hook' 
`broadcast-post'.  If the window configurations do not match, the command is not
broadcast to other visible broadcast-mode buffers.")

(defvar broadcast-transient-mark-mode nil
  "Holds a snapshot of the value of the variable `transient-mark-mode'.  
It is set when the `pre-command-hook' `broadcast-pre' is called.  Since its 
value may have changed by the time the `post-command-hook' `broadcast-post' 
is called, the variable `transient-mark-mode' is temporarily set via `let' to 
the snapshot value for each of the other visible broadcast-mode buffers.")

(defvar broadcast-suppress-advice nil
  "Ensures that nested advised calls don't do anything.")

(defun broadcast-get-state ()
  "Collects relevant state about the buffer.
Used to detect if a command should be broadcast."
  (list (buffer-modified-tick) (point) (mark) mark-active transient-mark-mode
        (and (boundp 'rectangle-mark-mode ) rectangle-mark-mode)
        kill-ring kill-ring-yank-pointer))

;;; Command Hooks
(defun broadcast-pre ()
  "A `pre-command-hook' that is enabled for all broadcast mode buffers.
It collects buffer state that is considered in the `post-command-hook' 
`broadcast-post'"
  (unless broadcast-suppress
    (setq broadcast-window-configuration (current-window-configuration))
    (setq broadcast-transient-mark-mode transient-mark-mode)
    (setq broadcast-state (broadcast-get-state))
    ;; undo boundary on every command otherwise undo can get buffers out of sync
    (undo-boundary))) 

(defun broadcast-post ()
  "Broadcasts the current command to other visible broaccast-mode buffers.
This function is registered as a `post-command-hook'.  Only certain commands are
broadcast.  The general strategy is to broadcast any command that modifies the 
state collected by `broadcast-get-state'.  We also don't rebroadcast anything 
that modifies the window configuration."
  (unless broadcast-suppress
    (let ((broadcast-suppress t))
      (when (or
             ;; terminal support -- broadcast-get-state can't handle this situation
             (string-prefix-p "term-" (prin1-to-string this-command)) 
             (and
              ;; don't broadcast in isearch mode.
              ;; It would be nice to support this but this is probably tricky
              (not isearch-mode)
              ;; don't broadcast anything that changes the window configuration
              broadcast-window-configuration
              (compare-window-configurations broadcast-window-configuration
                                             (current-window-configuration))
              ;; only broadcast when the state has changed
              (not (equal (broadcast-get-state) broadcast-state))))
        (broadcast-command
         (let ((transient-mark-mode broadcast-transient-mark-mode)
               (interprogram-cut-function nil))
           (condition-case err
               (progn
                 (run-hooks 'pre-command-hook)
                 ;; undo boundary on every command otherwise undo can
                 ;; get buffers out of sync
                 (undo-boundary)
                 (call-interactively real-this-command)
                 (run-hooks 'post-command-hook))
             (error (message "%s in %s"
                             (error-message-string err)
                             (buffer-name)))
             (quit))))))))

;;; Advice functions
(defun broadcast-current-kill-advice (n &optional do-not-move)
  "Places interprogram pasting on all kill rings."
  (let ((interprogram-paste (and (= n 0)
                                 interprogram-paste-function
                                 (funcall interprogram-paste-function))))
    (when interprogram-paste
      ;; Add interprogram-paste to normal kill ring, just
      ;; like current-kill usually does for itself.
      ;; We have to do the work for it tho, since the funcall only returns
      ;; something once. It is not a pure function.
      (let ((interprogram-cut-function nil)
            (broadcast-suppress-advice t))
        (with-temp-buffer ;; use temp buffer to ensure it's the default kill ring
          (if (listp interprogram-paste)
              (mapc 'kill-new (nreverse interprogram-paste))
            (kill-new interprogram-paste)))
        ;; And then add interprogram-paste to the kill-rings
        ;; of all the broadcast buffers too;
        (broadcast-foreach-broadcast-buffer
         (if (listp interprogram-paste)
             (mapc 'kill-new (nreverse interprogram-paste))
           (kill-new interprogram-paste)))))))

(defun broadcast-kill-advice (orig-func &rest args)
  "Advice to coordinate kill across broadcast buffers.
When a kill ring manipulation happens in a non-broadcast-mode buffer,
repeat ORIG-FUNC with ARGS in all the broadcast-mode buffers since their 
`kill-ring' variable is buffer local.  For kill ring manipulations that occur in
a broadcast buffer, repeate ORIG-FUNC with ARGS with the global kill ring."
  (if broadcast-suppress-advice
      (apply orig-func args)
    (let ((retval (apply orig-func args))
          (broadcast-suppress-advice t))
      (if broadcast-mode                               ; if we're in broadcast mode
          (unless broadcast-suppress                   ; then repeat the operation 
            (with-temp-buffer (apply orig-func args))) ; with a global kill ring 
        (broadcast-foreach-broadcast-buffer            ; else repeat it in each
         (apply orig-func args)))                      ; broadcast buffer
      retval)))

(provide 'broadcast)

;;; broadcast.el ends here
