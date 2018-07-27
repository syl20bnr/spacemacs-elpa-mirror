;;; jknav.el --- Automatically enable j/k keys for line-based navigation

;; Copyright (C) 2012 Aaron Culich

;; Author: Aaron Culich <aculich@gmail.com>
;; Maintainer: Aaron Culich <aculich@gmail.com>
;; Version: 0.0.1
;; Package-Version: 20121006.2025
;; Created: 7 Jul 2012
;; Keywords: keyboard navigation

;; This file is not part of GNU Emacs.

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;###autoload
(defun jknav-uninstall-keys ()
  (interactive)
  (local-unset-key (kbd "j"))
  (local-unset-key (kbd "k")))

(defvar jknav-search-key-pattern
  "next\\|prev\\|begin\\|end\\|start\\|finish\\|forward\\|backward\\|up\\|down"
  "Patterns used by `jknav-search-key-bindings' to detect what
  function defintion to bind to jknav keys.")

(defun jknav-search-key-bindings (key &optional match)
  (car (remove nil
               (mapcar (lambda (x)
                         (let ((binding
                                (key-binding (read-kbd-macro x))))
                           (when (string-match
                                  (or match jknav-search-key-pattern)
                                  (if (symbolp binding)
                                      (symbol-name binding)
                                    ""))
                             binding)))
                       (cond ((listp key) key)
                             ((stringp key)
                              (list key
                                    (format "M-%s" key)
                                    (format "C-%s" key))))))))

(eval-after-load "magit"
  '(progn
     (define-key magit-status-mode-map (kbd "K") 'magit-discard-item)
     (define-key magit-status-mode-map (kbd "C-k") 'magit-discard-item)))

(defun jknav-widget-forward (&optional arg)
  (interactive "p")
  (when (and (eq (face-at-point) 'widget-button)
             (save-excursion
               (beginning-of-line)
               (funcall (or (< arg 0)
                            'search-backward-regexp
                            'search-forward-regexp)
                "^\\w" (+ 1 (point))))) t))

(defun jknav-widget-backward (&optional arg)
  (interactive "p")
  (jknav-widget-forward -1))

;;;###autoload
(defun jknav-install-keys-dired (&optional force)
  (interactive)
  (cond
   ((and buffer-read-only
         (eq major-mode 'dired-mode))
    (local-set-key (kbd "C-j") 'dired-goto-file)
    (local-set-key (kbd "j")   'dired-next-line)
    (local-set-key (kbd "k")   'dired-previous-line)
    (local-set-key (kbd "n")   'dired-next-subdir)
    (local-set-key (kbd "p")   'dired-prev-subdir)
    (local-set-key (kbd "'")   'dired-up-directory)
    (local-set-key (kbd "SPC") 'scroll-up-command)
    (local-set-key (kbd ";")   'scroll-down-command))
   (t nil)))

;;;###autoload
(defun jknav-install-keys-help (&optional force)
  (interactive)
  (cond
   ((and buffer-read-only
         (eq major-mode 'help-mode))
    (define-key help-mode-map (kbd "l")     'help-go-back)
    (define-key help-mode-map (kbd ".")     'help-go-forward)
    (define-key help-mode-map (kbd "f")     'help-go-forward)
    (define-key help-mode-map (kbd "j")     'forward-button)
    (define-key help-mode-map (kbd "k")     'backward-button)
    (define-key help-mode-map (kbd ";")     'scroll-down))
   (t nil)))

;;;###autoload
(defun jknav-install-keys (&optional force)
  (interactive)
  (cond
   ((or force buffer-read-only)
    (cond ((eq major-mode 'dired-mode) (jknav-install-keys-dired force))
          ((eq major-mode 'help-mode)  (jknav-install-keys-help  force))
          ((eq major-mode 'calendar-mode)  nil)
          (t
           (let* ((match (if buffer-read-only
                             "self-insert-command\\|undefined"
                           "undefined"))
                  (j (jknav-search-key-bindings "j" match))
                  (k (jknav-search-key-bindings "k" match))
                  (next (jknav-search-key-bindings "n"))
                  (prev (jknav-search-key-bindings "p"))
                  (scroll-up (jknav-search-key-bindings '("SPC" "C-v")))
                  (scroll-down (jknav-search-key-bindings '("DEL" ";" "M-v"))))

             ;; FIXME: should be this:
             ;;     (when (and j k next prev)
             ;; to avoid clashing with existing keys
             (when (and next prev)
               (local-set-key (kbd "j") next)
               (local-set-key (kbd "k") prev)
               (when (and scroll-up scroll-down)
                 (local-set-key (kbd " ") scroll-up)
                 (local-set-key (kbd ";") scroll-down)))))))
   (t (jknav-uninstall-keys))))

(defadvice toggle-read-only (after jknav-update-keys)
  (if buffer-read-only
      (jknav-install-keys)
    (jknav-uninstall-keys)))

;;;###autoload
(defun jknav-initialize ()
  (interactive)
  (add-hook 'after-change-major-mode-hook 'jknav-install-keys t)
  (ad-enable-advice 'toggle-read-only 'after 'jknav-update-keys)
  (jknav-install-keys))

(provide 'jknav)

;;; jknav.el ends here
