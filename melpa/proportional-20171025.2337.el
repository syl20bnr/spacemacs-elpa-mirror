;;; proportional.el --- use a proportional font everywhere

;; Author: Johannes Goslar
;; Created: 30 June 2016
;; Version: 0.1.0
;; Package-Version: 20171025.2337
;; Package-Requires: ((emacs "25.1"))
;; Keywords: faces
;; URL: https://github.com/ksjogo/proportional

;; Copyright (C) 2016 Johannes Goslar

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

;;;; Code:

(defgroup proportional nil
  "Use proportional fonts where appropiate"
  :group 'environment)

(defcustom proportional-font
  "DejaVu Sans-11"
  "Default proportional-font to activate."
  :group 'proportional
  :type 'string)

(defcustom proportional-monospace-font
  "DejaVu Sans Mono-11"
  "Default proportional-font to activate."
  :group 'proportional
  :type 'string)

(defcustom proportional-monospace-hooks
  '(dired-mode-hook
    spacemacs-buffer-mode-hook
    tabulated-list-mode
    package-menu-mode-hook
    magit-popup-mode-hook
    magit-log-mode-hook
    which-key-init-buffer-hook
    mu4e-headers-mode-hook)
  "The list of hooks which shall be monospaced even when proportional mode is on."
  :group 'proportional
  :type '(repeat symbol))

(defcustom proportional-monospace-after-advices
  '(lv-message)
  "The list of functions which have an advice named `proportional',
which then is enabled when proportional is enabled."
  :group 'proportional
  :type '(repeat symbol))

(defun proportional-family (font)
  (replace-regexp-in-string "-.*" "" font))

(defun proportional-use-monospace ()
  (interactive)
  "Switch the current buffer to a monospace font."
  (face-remap-add-relative 'header-line :family (proportional-family proportional-monospace-font))
  (face-remap-add-relative 'mode-line :family (proportional-family proportional-monospace-font))
  (face-remap-add-relative 'default :family (proportional-family proportional-monospace-font)))

;;;###autoload
(define-minor-mode proportional-mode "" :global t
  (if proportional-mode
      (progn
        (add-to-list 'default-frame-alist (cons 'font proportional-font))
        (set-frame-font proportional-font)
        (set-fontset-font "fontset-default" 'symbol proportional-font)
        (set-face-font 'variable-pitch proportional-font)

        (proportional-which-key-fixer)

        (dolist (base proportional-monospace-after-advices)
          (ad-enable-advice base 'after 'proportional))

        (dolist (hook proportional-monospace-hooks)
          (add-hook hook 'proportional-use-monospace)))

    (progn
      (add-to-list 'default-frame-alist (cons 'font proportional-monospace-font))
      (set-frame-font proportional-monospace-font)
      (set-fontset-font "fontset-default" 'symbol proportional-monospace-font)
      (set-face-font 'variable-pitch proportional-monospace-font)

      (proportional-which-key-fixer)

      (dolist (base proportional-monospace-after-advices)
        (ad-disable-advice base 'after 'proportional))

      (dolist (hook proportional-monospace-hooks)
        (remove-hook hook 'proportional-use-monospace)))))

(defun proportional-which-key-fixer ()
  (when (and (fboundp #'which-key--init-buffer) (boundp 'which-key--buffer))
    (if proportional-mode
        (progn
          (which-key--init-buffer)
          (with-current-buffer which-key--buffer
            (proportional-use-monospace)))
      (when which-key--buffer
        (kill-buffer which-key--buffer)
        (setq which-key--buffer nil)))))

(with-eval-after-load 'which-key
  (proportional-which-key-fixer))

(with-eval-after-load 'hydra
  (defadvice lv-message (after proportional)
    (if-let ((buf (get-buffer " *LV*")))
        (with-current-buffer buf
          (buffer-face-mode t)
          (proportional-use-monospace))))
  (when proportional-mode
    (ad-enable-advice 'lv-message 'after 'proportional)))

(provide 'proportional)
;;; proportional.el ends here
