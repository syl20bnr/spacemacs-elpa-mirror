;;; frame-mode.el --- Use frames instead of windows -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: frames
;; Package-Version: 20170419.1427
;; URL: https://github.com/IvanMalison/frame-mode
;; Version: 0.0.0
;; Package-Requires: ((s "1.9.0") (emacs "24.4"))

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

;; Use frames instead of windows whenever display-buffer is called.

;;; Code:

(require 's)
(require 'cl-lib)

(defgroup frame-mode ()
  "Frames minor mode."
  :group 'frame-mode
  :prefix "frame-mode-")

(defvar frame-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x 3") 'make-frame)
    (define-key map (kbd "C-x 2") 'make-frame)
    (define-key map (kbd "C-x o") 'frame-mode-other-window)
    (define-key map (kbd "C-x O") '(lambda ()
                                     (interactive)
                                     (frame-mode-other-window -1)))
    (define-key map (kbd "C-c C-f")
      'frame-mode-other-window-or-frame-next-command)
    map))

(define-minor-mode frame-keys-mode
  "Minor mode that replaces familiar window manipulation key bindings with
commands that do similar things with frames."
  :lighter nil
  :keymap frame-mode-keymap
  :global t
  :group 'frame-mode
  :require 'frame-mode)

(defvar frame-mode-display-buffer-alist-entry
  '(frame-mode-enabled . ((frame-mode-display-buffer . ()))))

;;;###autoload
(define-minor-mode frame-mode
  "Minor mode that uses `display-buffer-alist' to ensure that buffers are
displayed using frames intead of windows."
  :lighter nil
  :keymap nil
  :global t
  :group 'frame-mode
  :require 'frame-mode
  (if frame-mode
      (progn (unless (member frame-mode-display-buffer-alist-entry
                             display-buffer-alist)
               (push frame-mode-display-buffer-alist-entry
                     display-buffer-alist))
             (unless pop-up-frames
               (setq pop-up-frames 'graphic-only)))
    (setq pop-up-frames nil)))

(defcustom frame-mode-is-frame-viewable-fn
  'frame-mode-default-is-frame-viewable-fn
  "Predicate that determines whether a frame can be used to pop up a buffer."
  :type 'function)

(defun frame-mode-is-frame-viewable (frame)
  (funcall frame-mode-is-frame-viewable-fn frame))

(defun frame-mode-default-is-frame-viewable-fn (frame)
  (s-contains-p "IsViewable"
                (shell-command-to-string
                 (format "xwininfo -id %s" (frame-parameter frame 'window-id)))))

(defun frame-mode-display-some-frame-predicate (frame)
  (and
   (not (eq frame (selected-frame)))
   (not (window-dedicated-p
         (or
          (get-lru-window frame)
          (frame-first-window frame))))
   (frame-mode-is-frame-viewable frame)))

(defcustom frame-mode-is-frame-viewable-fn
  'frame-mode-default-is-frame-viewable-fn
  "Predicate that determines whether a frame can be used to pop up a buffer."
  :type 'function)



;; These variables are for internal use by `frame-only-mode' only.
(defvar frame-mode-use-new-frame-or-window nil)
(defvar frame-mode-use-new-frame-or-window-next-command nil)
(defmacro frame-mode-force-use-new-frame (&rest forms)
  `(let ((frame-mode-use-new-frame-or-window t))
     ,@forms))

(defun frame-mode-should-use-other-frame-or-window (&rest _args)
  (and frame-mode
       (or frame-mode-use-new-frame-or-window-next-command
           frame-mode-use-new-frame-or-window)))

(defun frame-mode-force-display-buffer-pop-up-frame (&rest args)
  (let ((res (apply 'display-buffer-pop-up-frame args)))
    (when res
      (setq frame-mode-use-new-frame-or-window-next-command nil))
    res))

(defun frame-mode-force-display-buffer-use-some-frame (&rest args)
  (let ((res (apply 'display-buffer-use-some-frame args)))
    (when res
      (setq frame-mode-use-new-frame-or-window-next-command nil))
    res))



(defvar frame-mode-display-buffer-alist
 ;; XXX: helm and popup go first here because its unlikely someone would want to
 ;; control where those buffers show up. This avoids unintentionally
 ;; deactivating the effect of `frame-mode-other-window-or-frame-next-command'.
 '(("\\*helm.*" . (display-buffer-same-window display-buffer-pop-up-window))
   (".*popup\*" . (display-buffer-pop-up-window))
   (frame-mode-should-use-other-frame-or-window .
    ((frame-mode-force-display-buffer-use-some-frame
      frame-mode-force-display-buffer-pop-up-frame) .
      ((inhibit-same-window . t)
       (reusable-frames . t))))
   ("\\*flycheck errors\\*" .
    ((display-buffer-use-some-frame
      display-buffer-pop-up-frame) .
      ((inhibit-same-window . t)
       (frame-predicate . frame-mode-display-some-frame-predicate))))
   (".*magit-diff.*" .
    ((display-buffer-pop-up-window) .
     ((reusable-frames . 0)
      (inhibit-switch-frame . t)
      (inhibit-same-window . t))))
   ("\\*register preview\\*" . ((display-buffer-pop-up-window)))
   (".*" .
    ((display-buffer-same-window
      display-buffer-use-some-frame
      display-buffer-pop-up-frame) .
     ((reusable-frames . t)
      (frame-predicate . frame-mode-display-some-frame-predicate))))))

(defun frame-mode-enabled (&rest _args)
  frame-mode)

(defun frame-mode-display-buffer (buffer alist)
  (cl-destructuring-bind
      (functions . additional-alist)
      (display-buffer-assq-regexp (buffer-name buffer)
                                  frame-mode-display-buffer-alist
                                  alist)
    (let ((full-alist (append alist additional-alist)))
      (cl-loop for fn in functions
               for result = (funcall fn buffer full-alist)
               when result return result))))

;;;###autoload
(defun frame-mode-other-window (count)
  "A version of `other-window' that can jump across frames.

COUNT determines the number of windows to move over."
  (interactive
   (list 1))
  (other-window count 'visible)
  (select-frame-set-input-focus (selected-frame)))

;;;###autoload
(defun frame-mode-other-window-or-frame-next-command ()
  "Use a new frame no matter what when the next call to `display-buffer' occurs."
  (interactive)
  (setq frame-mode-use-new-frame-or-window-next-command
        (not frame-mode-use-new-frame-or-window-next-command))
  (message "using other frame: %s"
           frame-mode-use-new-frame-or-window-next-command))

(provide 'frame-mode)
;;; frame-mode.el ends here
