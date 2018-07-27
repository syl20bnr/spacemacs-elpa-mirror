;;; key-intercept.el --- Intercept prefix keys

;; Author: INA Lintaro <tarao.gnn at gmail.com>
;; URL: http://github.com/tarao/key-intercept-el
;; Package-Version: 20140211.749
;; Version: 0.1
;; Keywords: keyboard

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

;; Code:

(defgroup key-intercept nil
  "Key interception."
  :group 'keyboard
  :prefix "key-intercept-")

(defcustom key-intercept-delay 0.5
  "Time in seconds to wait for another input event before interception."
  :type 'number
  :group 'key-intercept)

(defcustom key-intercept-echo-keystrokes 0
  "Time in seconds to wait for echoing keystrokes.

Specifiying larger value than `key-intercept-delay' disables echoing."
  :type 'number
  :group 'key-intercept)

echo-keystrokes

(defcustom key-intercept-fake-keystrokes t
  "Non-nil means echoing keystrokes in a inputted form. If it is nil, then
keystrokes are displayed in a form of internal events."
  :type 'boolean
  :group 'key-intercept)

(defvar key-intercept-mode nil)
(defvar key-intercept-map (make-sparse-keymap))
(defvar key-intercept-prefix t)
(defvar key-intercept-prefix-map (make-keymap))
(defvar key-intercept-global t)
(defvar key-intercept-global-map (make-sparse-keymap))
(defvar key-intercept-pass t)
(defvar key-intercept-pass-map (make-keymap))

(defvar key-intercept-emulation-map-alist
  `((key-intercept-mode . ,key-intercept-map)
    (key-intercept-prefix . ,key-intercept-prefix-map))
  "List of emulation layer key bindings.")
(defvar key-intercept-intercept-map-alist
  `((key-intercept-global . ,key-intercept-global-map)
    (key-intercept-pass . ,key-intercept-pass-map))
  "List of interception command bindings.")

(defun key-intercept-make-local ()
  "Make interceptions buffer local. Calling
`define-intercept-key' or `define-modal-intercept-key' after
using this command makes interception key binding buffer
local. In other words, without calling this command, interception
key binding affects all buffers."
  (interactive)
  (when (not (local-variable-p 'key-intercept-emulation-map-alist))
    (make-local-variable 'key-intercept-emulation-map-alist)
    (make-local-variable 'key-intercept-intercept-map-alist)
    (make-local-variable 'key-intercept-map)
    (make-local-variable 'key-intercept-prefix)
    (make-local-variable 'key-intercept-prefix-map)
    (make-local-variable 'key-intercept-global)
    (make-local-variable 'key-intercept-global-map)
    (make-local-variable 'key-intercept-pass)
    (make-local-variable 'key-intercept-pass-map)

    (setq
     key-intercept-map (copy-keymap key-intercept-map)
     key-intercept-prefix-map (copy-keymap key-intercept-prefix-map)
     key-intercept-global-map (copy-keymap key-intercept-global-map)
     key-intercept-pass-map (copy-keymap key-intercept-pass-map))

    (setq key-intercept-emulation-map-alist
          (append
           (delq 'key-intercept-mode
                 (delq 'key-intercept-prefix
                       key-intercept-emulation-map-alist))
           `((key-intercept-mode . ,key-intercept-map)
             (key-intercept-prefix . ,key-intercept-prefix-map))))
    (setq key-intercept-intercept-map-alist
          (append
           (delq 'key-intercept-global
                 (delq 'key-intercept-pass
                       key-intercept-intercept-map-alist))
           `((key-intercept-global . ,key-intercept-global-map)
             (key-intercept-pass . ,key-intercept-pass-map))))))

(defun key-intercept-make-local-maybe ()
  (setq emulation-mode-map-alists (append emulation-mode-map-alists))
  (when (local-variable-p 'emulation-mode-map-alists)
    (key-intercept-make-local)))

(define-minor-mode key-intercept-mode
  "Minor mode for key interception."
  :group 'key-intercept
  (if key-intercept-mode
      (progn
        (key-intercept-make-local-maybe)
        (setq emulation-mode-map-alists
              (delq 'key-intercept-emulation-map-alist
                    (delq 'key-intercept-intercept-map-alist
                          emulation-mode-map-alists)))
        (add-to-list 'emulation-mode-map-alists
                     'key-intercept-emulation-map-alist)
        (add-to-list 'emulation-mode-map-alists
                     'key-intercept-intercept-map-alist))))

(defun key-intercept-on ()
  (key-intercept-mode 1)
  (remove-hook 'pre-command-hook 'key-intercept-on t))
(put 'key-intercept-on 'permanent-local-hook t)

(defun key-intercept-make-key-str (key)
  (key-description (listify-key-sequence key)))

(defun key-intercept-make-prefix-event (key)
  (intern (concat "prefix-" (key-intercept-make-key-str key))))

(defun key-intercept-make-intercept-event (key)
  (intern (concat "intercept-" (key-intercept-make-key-str key))))

(defun key-intercept-raw-key (key)
  (cond
   ((symbolp key)
    (let* ((s (symbol-name key))
           (r (replace-regexp-in-string "^\\(intercept\\|prefix\\)-" "" s)))
      (if (string= s r)
          (vector key)
        (apply 'vconcat (mapcar 'read-kbd-macro (split-string r " " t))))))
   ((stringp key) key)
   (t (vector key))))

(defun key-intercept-raw-keys (keys)
  (apply 'vconcat (mapcar 'key-intercept-raw-key keys)))

(defun key-intercept-echo-keys (&optional keys)
  (unless keys (setq keys (this-command-keys)))
  (when key-intercept-fake-keystrokes
    (setq keys (key-intercept-raw-keys keys)))
  (let (message-log-max)
    (message (concat (key-intercept-make-key-str keys) "-"))))

(defun key-intercept-read-event (delay)
  (let ((strokes key-intercept-echo-keystrokes) k)
    (if (setq k (read-event nil nil (min delay strokes)))
        k
      (when (< strokes delay)
        (key-intercept-echo-keys)
        (read-event nil nil (- delay strokes))))))

(defun key-intercept-this-command-keys ()
  (let ((keys (this-command-keys)))
    (if current-prefix-arg
        (substring keys universal-argument-num-events) keys)))

(defun key-intercept-command-end ()
  ;; turn off the emulation layer but reactivate it at the next command
  (key-intercept-mode -1)
  (add-hook 'pre-command-hook 'key-intercept-on nil t)
  ;; restore last command
  (setq this-command last-command)
  ;; pass prefix argument
  (setq prefix-arg current-prefix-arg)
  ;; reset this-command-keys
  (reset-this-command-lengths))

(defun key-intercept-replace-kbd-macro (&optional events)
  (when defining-kbd-macro
    (end-kbd-macro)
    (let* ((keys (key-intercept-this-command-keys))
           (prefix (and current-prefix-arg
                        (substring (this-command-keys) 0
                                   universal-argument-num-events)))
           (raw-keys (key-intercept-raw-keys keys))
           (len (length raw-keys)))
      (when (< (length last-kbd-macro) len)
        (setq last-kbd-macro
              (vconcat prefix raw-keys)))
      (when (equal raw-keys (vconcat (substring last-kbd-macro (- len))))
        (setq last-kbd-macro
              (vconcat
               (substring last-kbd-macro 0 (- (length last-kbd-macro) len))
               (or events keys)))))
    (start-kbd-macro t t)))

(defun key-intercept-initial-command (arg)
  (interactive "P")
  (let* ((keys (key-intercept-this-command-keys))
         (pevent (key-intercept-make-prefix-event keys)))
    ;; delegate to key-intercept-prefix-command
    (push pevent unread-command-events)
    (key-intercept-command-end)))

(defun key-intercept-prefix-command (key delay)
  (let* ((keys (key-intercept-this-command-keys))
         (raw-keys (key-intercept-raw-keys keys))
         k)
    (if (and (> delay 0) (setq k (key-intercept-read-event delay)))
        ;; there is another input event
        (let* ((keys (vconcat raw-keys (vector k)))
               (next-pevent (key-intercept-make-prefix-event keys)))
          (if (lookup-key key-intercept-prefix-map (vector next-pevent))
              ;; we may have another interception for the new event
              (push next-pevent unread-command-events)
            ;; fall back to the original event
            (push (cons t k) unread-command-events) ; echo k-
            (setq unread-command-events
                  (append (listify-key-sequence raw-keys)
                          unread-command-events))))
      ;; no input arrived; intercept the key sequence
      (let ((ievent (key-intercept-make-intercept-event key)))
        ;; push event for the intercept command
        (key-intercept-replace-kbd-macro (vector ievent))
        (push ievent unread-command-events)))
    (key-intercept-command-end)))

(defun key-intercept-make-prefix-command (key delay)
  `(lambda (arg)
     (interactive "P")
     (key-intercept-prefix-command ,key ,delay)))

(defun key-intercept-run-original-command (keys)
  ;; push original events
  (setq unread-command-events (append keys unread-command-events))
  (key-intercept-command-end))

(defun key-intercept-pass-through (arg)
  (interactive "P")
  (let ((keys (key-intercept-raw-keys (key-intercept-this-command-keys))))
    (key-intercept-run-original-command keys)))

(defun define-modal-intercept-key (key mode def &optional delay)
  "Modally bind KEY to DEF for MODE. MODE is a minor mode or
variable. If DELAY is non-nil, it overrides `key-intercept-delay'
for the sepcified key."
  (key-intercept-make-local-maybe)

  ;; default values
  (unless mode (setq mode 'key-intercept-global))
  (unless delay (setq delay key-intercept-delay))

  (let* ((keys (listify-key-sequence key))
         (initial (car keys))
         (map (or (cdr (assq mode key-intercept-intercept-map-alist))
                  (make-sparse-keymap))))
    (if (eq (elt key 0) 'remap)
        ;; remapping
        (define-key map key def)
      ;; setup emulation key
      (define-key key-intercept-map (vector initial)
        'key-intercept-initial-command)
      ;; setup prefix key(s)
      (let ((prefix-key []))
        (dolist (k keys)
          (setq prefix-key (vconcat prefix-key (vector k)))
          (let ((pevent (key-intercept-make-prefix-event prefix-key))
                (ievent (key-intercept-make-intercept-event prefix-key)))
            (define-key key-intercept-prefix-map (vector pevent)
              (key-intercept-make-prefix-command prefix-key delay))
            (define-key key-intercept-pass-map (vector ievent)
              'key-intercept-pass-through))))
      ;; setup intercept key
      (let ((ievent (key-intercept-make-intercept-event key)))
        (define-key map (vector ievent) def)))
    (add-to-list 'key-intercept-intercept-map-alist (cons mode map))))

(defun define-intercept-key (key def &optional delay)
  "Bind KEY to DEF for MODE. MODE is a minor mode or variable. If
DELAY is non-nil, it overrides `key-intercept-delay' for the
sepcified key."
  (define-modal-intercept-key key nil def delay))

(provide 'key-intercept)
;;; key-intercept.el ends here
