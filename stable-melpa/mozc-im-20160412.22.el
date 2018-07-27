;;; mozc-im.el --- Mozc with input-method-function interface.

;; Copyright (C) 2015  Daisuke Kobayashi

;; Author: Daisuke Kobayashi <d5884jp@gmail.com>
;; Version: 0.1
;; Package-Version: 20160412.22
;; Keywords: i18n, extentions
;; Package-Requires: ((mozc "0"))

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

;; This package provide `input-method-function' interface into Mozc.
;; The difference from original, this can be used in `isearch-mode' and
;; Char-mode on `ansi-term'.

;;; Usage:

;; Put your init.el below:
;;
;;  (require 'mozc-im)
;;  (setq default-input-method "japanese-mozc-im")
;;
;; and use `toggle-input-method' for activate.
;;
;; Customize variable's are depend on original `mozc-mode'.

;;; Code:

(require 'mozc)

(defgroup mozc-im nil
  "Mozc-im - Mozc with input-method-function interface."
  :group 'mozc)

(defcustom mozc-im-activate-hook nil
  "A list of hooks called on mozc-im activated."
  :type 'hook
  :group 'mozc-im)

(defcustom mozc-im-deactivate-hook nil
  "A list of hooks called on mozc-im deactivated."
  :type 'hook
  :group 'mozc-im)

(defvar mozc-im-edit-state nil
  "Mozc's edit state.")
(make-variable-buffer-local 'mozc-im-edit-state)

(defadvice mozc-session-recv-corresponding-response
    (after mozc-im-peek-edit-state activate)
  "Peek edit state and update `mozc-im-edit-state'."
  (when ad-return-value
    (setq mozc-im-edit-state
          (cond
           ((mozc-protobuf-get ad-return-value 'output 'preedit)
            'preedit)
           ((mozc-protobuf-get ad-return-value 'output 'result)
            'result)
           (t
            'empty)))))

(defun mozc-im-leim-activate (input-method)
  "Activate mozc-im input method.
INPUT-METHOD isn't used."
  (let ((new 'deactivate-current-input-method-function)
        (old 'inactivate-current-input-method-function))
    (set (if (boundp new) new old) #'mozc-im-leim-deactivate))

  (when (eq (selected-window) (minibuffer-window))
    (add-hook 'minibuffer-exit-hook 'mozc-im-exit-from-minibuffer))

  (make-local-variable 'input-method-function)
  (setq input-method-function 'mozc-im-input-method)
  (run-hooks 'mozc-im-activate-hook))

(defun mozc-im-leim-deactivate ()
  "Deactivate mozc-im input method."
  (kill-local-variable 'input-method-function)
  (run-hooks 'mozc-im-deactivate-hook))

(defun mozc-im-exit-from-minibuffer ()
  "Deactivate mozc-im when exit from minibuffer."
  (deactivate-input-method)
  (when (<= (minibuffer-depth) 1)
    (remove-hook 'minibuffer-exit-hook 'mozc-im-exit-from-minibuffer)))

(defun mozc-im-input-method (key)
  "Consume KEY and following events by mozc."
  (if (or buffer-read-only
          overriding-terminal-local-map
          overriding-local-map)
      (list key)
    (let ((input-method-function nil)
          (modified-p (buffer-modified-p))
          (buffer-undo-list t)
          (inhibit-modification-hooks t)
          (inhibit-quit t)
          (echo-keystrokes 0)
          (preedit-start-at (point)))
      (unwind-protect
          (progn
            (mozc-handle-event key)
            (while (not (memq mozc-im-edit-state '(nil empty result)))
              (mozc-handle-event (read-event)))
            (prog1
                (string-to-list (buffer-substring-no-properties
                                 preedit-start-at (point)))
              (delete-region preedit-start-at (point))))
        (mozc-clean-up-session)
        (set-buffer-modified-p modified-p)))))

(defun mozc-im-register-input-method ()
  "Register Mozc-im as a input method."
  (setq minor-mode-map-alist
        (cons (cons 'mozc-mode mozc-mode-map)
              (assq-delete-all 'mozc-mode minor-mode-map-alist)))
  (register-input-method
   "japanese-mozc-im"
   "Japanese"
   'mozc-im-leim-activate
   mozc-leim-title
   "Japanese input method with Mozc."))

(add-hook 'emacs-startup-hook 'mozc-im-register-input-method)

(mozc-im-register-input-method)

(provide 'mozc-im)

;;; mozc-im.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
;; End:
