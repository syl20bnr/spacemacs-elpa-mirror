;;; inferior-spim.el --- inferior mode for spim.

;; Copyright (C) 2016 hiddenlotus
;; Author: hiddenlotus <kaihaosw@gmail.com>
;; Git: https://github.com/hiddenlotus/inferior-spim.git
;; Version: 0.0.1
;; Package-Version: 20160810.2132
;; Created: 2016-08-01
;; Keywords: spim, inferior, mips

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Inferior process for spim.

;; If you use `asm-mode', you can configure like this:

;; (require 'asm-mode)
;; (define-key asm-mode-map (kbd "C-`") 'inferior-run-spim)
;; (define-key asm-mode-map (kbd "C-c C-z") 'inferior-switch-to-spim)
;; (define-key asm-mode-map (kbd "C-c C-b") 'inferior-spim-send-buffer)
;; (define-key asm-mode-map (kbd "C-c C-l") 'inferier-spim-load-file)
;; (define-key asm-mode-map (kbd "C-c i") 'inferior-spim-send-reinitialize)
;; (define-key asm-mode-map (kbd "C-c r") 'inferior-spim-send-run)

;;; Code:
(require 'comint)

(defgroup inferior-spim nil
  "Run a spim process in a buffer."
  :group 'asm)

(defcustom inferior-spim-program-command (executable-find "spim")
  "Command for Spim."
  :group 'inferior-spim)

(defcustom inferior-spim-program-arguments '()
  "List of command line arguments."
  :group 'inferior-spim)

(defcustom inferior-spim-mode-hook nil
  "Hooks when running inferior-spim mode."
  :group 'inferior-spim)

(defvar inferior-spim-buffer)

;;;###autoload
(defun inferior-run-spim (&optional dont-switch-p)
  "Run an Inferior Spim process."
  (interactive)
  (unless (comint-check-proc "*spim*")
    (with-current-buffer
        (apply 'make-comint "spim" inferior-spim-program-command
               nil inferior-spim-program-arguments)
      (inferior-spim-mode)))
  (setq inferior-spim-buffer "*spim*")
  (unless dont-switch-p
    (pop-to-buffer inferior-spim-buffer)))

;;;###autoload
(defun inferior-spim-send-reinitialize ()
  (interactive)
  (inferior-run-spim t)
  (comint-send-string inferior-spim-buffer "reinitialize\n"))

;;;###autoload
(defun inferior-spim-send-run ()
  (interactive)
  (inferior-run-spim t)
  (comint-send-string inferior-spim-buffer "run\n"))

;;;###autoload
(defun inferior-spim-send-buffer ()
  (interactive)
  (inferior-run-spim t)
  (comint-send-string
   inferior-spim-buffer
   (format "load \"%s\"\n" (buffer-file-name (current-buffer)))))

;;;###autoload
(defun inferier-spim-load-file (filename)
  "Load a file in the spim process."
  (interactive "f")
  (let ((filename (expand-file-name filename)))
    (inferior-run-spim t)
    (comint-send-string
     inferior-spim-buffer
     (format "load \"%s\"\n" filename))))

;;;###autoload
(defun inferior-switch-to-spim (eob-p)
  (interactive "P")
  (if (and inferior-spim-buffer (get-buffer inferior-spim-buffer))
      (pop-to-buffer inferior-spim-buffer)
    (error "No current process buffer. See variale `inferior-spim-buffer'")))

(define-derived-mode inferior-spim-mode comint-mode "Inferior Spim"
  "Major mode for interacting with an inferior Spim process."
  :group 'inferior-spim)

(define-key inferior-spim-mode-map (kbd "C-c C-l") 'inferier-spim-load-file)
(define-key inferior-spim-mode-map (kbd "C-c i") 'inferior-spim-send-reinitialize)
(define-key inferior-spim-mode-map (kbd "C-c r") 'inferior-spim-send-run)

(provide 'inferior-spim)
;;; inferior-spim.el ends here
