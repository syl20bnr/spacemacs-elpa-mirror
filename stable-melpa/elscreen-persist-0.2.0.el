;;; elscreen-persist.el --- persist the elscreen across sessions
;; Copyright (C) 2014 Hironori Yoshida

;; Author: Hironori Yoshida <webmaster@robario.com>
;; Keywords: elscreen frames
;; Package-Version: 0.2.0
;; Version: 0.2.0
;; Package-Requires: ((elscreen "1.4.6") (revive "2.19"))

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

;; This makes elscreen persistent.
;;
;; To use this, use customize to turn on `elscreen-persist-mode`
;; or add the following line somewhere in your init file:
;;
;;     (elscreen-persist-mode 1)
;;
;; Or manually, use `elscreen-persist-store` to store,
;; and use `elscreen-persist-restore` to restore.
;;
;; Or manually, use `elscreen-persist-get-data` to get data to store,
;; and use `elscreen-persist-set-data` to set data to restore.
;;
;; Please see README.md from the same repository for documentation.

;;; Code:

(require 'elscreen)
(require 'revive)

(defcustom elscreen-persist-file (locate-user-emacs-file "elscreen")
  "The file where the elscreen configuration is stored."
  :type 'file
  :group 'elscreen)

;;;###autoload
(defun elscreen-persist-get-frame-params ()
  "Determine the frame parameters."
  (let ((frame-parameters (frame-parameters)))
    ;; Delete some unserializable frame parameter.
    (dolist (key '(buffer-list buried-buffer-list minibuffer))
      (delq (assq key frame-parameters) frame-parameters))
    frame-parameters))

;;;###autoload
(defun elscreen-persist-get-screens ()
  "Determine the screens, window configurations."
  (let ((current-screen (elscreen-get-current-screen))
        screen-to-window-configuration-alist)
    ;; Collect all the screen and window configurations.
    ;; - The first element is a last (max screen number) screen configuration.
    ;; - The last element is a current screen configuration.
    (dolist (screen (sort (elscreen-get-screen-list) '<))
      (elscreen-goto screen)
      (let ((screen-to-window-configuration (list (cons screen (current-window-configuration-printable)))))
        (setq screen-to-window-configuration-alist
              (if (eq screen current-screen)
                  (append screen-to-window-configuration-alist screen-to-window-configuration)
                (append screen-to-window-configuration screen-to-window-configuration-alist)))))
    (elscreen-goto current-screen)
    screen-to-window-configuration-alist))

;;;###autoload
(defun elscreen-persist-get-data ()
  "Determine the frame parameters and screens, window configurations."
  (list (elscreen-persist-get-frame-params)
        (elscreen-persist-get-screens)))

(defun elscreen-persist-store ()
  "Store the screens, window configurations and frame parameters."
  (interactive)
  (let ((frame-parameters (elscreen-persist-get-frame-params))
        (screen-to-window-configuration-alist (elscreen-persist-get-screens)))
    ;; Store the configurations.
    (with-temp-file elscreen-persist-file
      (let ((print-length nil)
            (print-level nil))
        (insert (prin1-to-string `((frame-parameters . ,frame-parameters)
                                   (screen-to-window-configuration-alist . ,screen-to-window-configuration-alist))))))))

;;;###autoload
(defun elscreen-persist-set-frame-params (data)
  "Set the frame parameters if necessary."
  (unless (and (boundp 'desktop-restore-frames) desktop-restore-frames
               (fboundp 'desktop-full-lock-name) (file-exists-p (desktop-full-lock-name)))
    (modify-frame-parameters nil data)
    (message "The frame was restored by `elscreen-persist'. Using `desktop' is recommended.")))

;;;###autoload
(defun elscreen-persist-set-screens (data)
  "Set the screens, window configurations."
  (dolist (screen-to-window-configuration data)
    (while (not (elscreen-screen-live-p (car screen-to-window-configuration)))
      (elscreen-create))
    (elscreen-goto (car screen-to-window-configuration))
    (restore-window-configuration (cdr screen-to-window-configuration)))
  ;; Kill unnecessary screens.
  (dolist (screen (elscreen-get-screen-list))
    (unless (assq screen data)
      (elscreen-kill screen))))

;;;###autoload
(defun elscreen-persist-set-data (data)
  "Set the frame parameters and screens, window configurations."
  (elscreen-persist-set-frame-params (car data))
  (elscreen-persist-set-screens (car (cdr data))))

;;;###autoload
(defun elscreen-persist-restore ()
  "Restore the screens, window configurations, and also the frame parameters if necessary."
  (interactive)
  (when (file-exists-p elscreen-persist-file)
    (let ((config (read (with-temp-buffer (insert-file-contents elscreen-persist-file) (buffer-string)))))
      (elscreen-persist-set-frame-params (assoc-default 'frame-parameters config))
      (elscreen-persist-set-screens (assoc-default 'screen-to-window-configuration-alist config)))))

;;;###autoload
(define-minor-mode elscreen-persist-mode
  "Toggle persistent elscreen (ElScreen Persist mode).
With a prefix argument ARG, enable ElScreen Persist mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :group 'elscreen
  :global t
  (if elscreen-persist-mode
      (progn
        (add-hook 'kill-emacs-hook #'elscreen-persist-store t)
        (add-hook 'window-setup-hook #'elscreen-persist-restore t))
    (remove-hook 'kill-emacs-hook #'elscreen-persist-store)
    (remove-hook 'window-setup-hook #'elscreen-persist-restore)))

(provide 'elscreen-persist)
;;; elscreen-persist.el ends here
