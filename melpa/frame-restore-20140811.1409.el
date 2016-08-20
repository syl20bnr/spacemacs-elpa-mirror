;;; frame-restore.el --- Restore Emacs frame -*- lexical-binding: t; -*-

;; Copyright (c) 2012, 2013 Sebastian Wiesner <lunaryorn@gmail.com>
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/frame-restore.el
;; Package-Version: 20140811.1409
;; Keywords:  frames convenience
;; Version: 0.6-cvs
;; Package-Requires: ((emacs "24.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Save and restore parameters of Emacs frames.

;; Just call `frame-restore' in your `init.el':
;;
;;    (frame-restore-mode)
;;
;; Note that since r113242 the built-in Desktop Save mode will restore frames.
;; If you are using a Emacs snapshot build later than this revision, you are
;; *strongly* advised to use Desktop Save mode instead:
;;
;;    (desktop-save-mode)
;;
;; Frame Restore mode will display a bold warning if enabled in an Emacs build
;; whose Desktop Save mode can restore frames.

;;; Code:

;;;; Customization
(defgroup frame-restore nil
  "Save and restore frame parameters."
  :group 'frames
  :link '(url-link :tag "Github" "https://github.com/lunaryorn/frame-restore.el")
  :link '(emacs-commentary-link :tag "Commentary" "frame-restore")
  :link '(emacs-library-link :tag "Source" "frame-restore"))

(defcustom frame-restore-parameters-file
  (locate-user-emacs-file "frame-restore-parameters")
  "File to store frame parameters in."
  :type 'file
  :group 'frame-restore)

(defcustom frame-restore-parameters
  '(left top width height maximized fullscreen alpha)
  "Frame parameters to save and restore.

See Info node `(elisp)Frame Parameters' for information about
frame parameters."
  :type '(repeat (symbol :tag "Frame parameter"))
  :group 'frame-restore)

(defcustom frame-restore-initial-frame t
  "Whether to restore the parameters of the initial frame.

If t, restore the frame, otherwise don't."
  :type 'boolean
  :group 'frame-restore)

;;;; Mode definition
(defun frame-restore-desktop-can-save-frames ()
  "Whether `desktop-save-mode' can save and restore frames."
  (and (require 'desktop nil :no-error)
       (boundp 'desktop-restore-frames)))

;;;###autoload
(define-minor-mode frame-restore-mode
  "Toggle Frame Restore Mode.

With a prefix argument ARG, enable Frame Restore mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

If Frame Restore mode is enabled, the state of the initial frame
is saved from one session to another."
  :global t
  :group 'frame-restore
  (when (and frame-restore-mode
             (frame-restore-desktop-can-save-frames))
    ;; Recent Desktop Save Mode can restore frames, too, in a much superior way,
    ;; so we should advice our users to use Desktop Save Mode instead.
    (lwarn 'frame-restore :warning
           "Frame restore is obsolete. Use `desktop-save-mode' instead")))

;;;###autoload
(define-obsolete-function-alias 'frame-restore 'frame-restore-mode "0.2")

;;;; Save and restore of frame
(defun frame-restore-save-parameters ()
  "Save frame parameters of the currently selected frame.

Save parameters in `frame-restore-parameters' to
`frame-restore-parameters-file'.

Return t, if the parameters were saved, or nil otherwise."
  (condition-case nil
      (when (display-graphic-p)         ; GUI frames only!
        (let ((print-level nil)
              (print-length nil)
              params-to-save)
          (dolist (param (frame-parameters))
            (when (memq (car param) frame-restore-parameters)
              (push param params-to-save)))
          (with-temp-file frame-restore-parameters-file
            (prin1 params-to-save (current-buffer))))
        t)
    (file-error nil)))

(defun frame-restore-desktop-restores-frame-p ()
  "Whether or not Deskop Save mode will restore frames."
  (and (bound-and-true-p desktop-save-mode)
       (bound-and-true-p desktop-restore-frames)))

(defun frame-restore-initial-frame ()
  "Restore the frame parameters of the initial frame.

If Frame Restore mode is enabled, load parameters in
`frame-restore-parameters' from `frame-restore-parameters-file'
and update `initial-frame-alist' accordingly.

Do nothing if Frame Restore mode is disabled, as by variable
`frame-restore-mode'.

Return the new `initial-frame-alist', or nil if reading failed."
  (when (and frame-restore-mode
             ;; Don't mess up with Desktop Save mode
             (not (frame-restore-desktop-restores-frame-p)))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents frame-restore-parameters-file)
          (goto-char (point-min))
          (dolist (param (read (current-buffer)))
            (when (memq (car param) frame-restore-parameters)
              (setq initial-frame-alist
                    (cons param
                          (assq-delete-all (car param) initial-frame-alist))))))
      (file-error nil)
      (error (message "Unexpected signal %S while restoring frame: %S"
                      (car err) (cdr err))))))

;; Add our hooks
(unless noninteractive
  (add-hook 'kill-emacs-hook #'frame-restore-save-parameters)
  (add-hook 'after-init-hook #'frame-restore-initial-frame))

(provide 'frame-restore)

;; Local Variables:
;; coding: utf-8
;; End:

;;; frame-restore.el ends here
