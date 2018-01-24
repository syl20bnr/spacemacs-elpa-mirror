;;; syndicate.el --- evil keybindings for org-mode

;; Copyright (c) 2016 Kawin Nikomborirak

;; Author: Kawin Nikomborirak
;; URL: https://github.com/KNX32542/syndicate.git
;; Package-Version: 20160603.823
;; Keywords: evil org bindings
;; Package-Requires: ((evil "1.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

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
;; Syndicate is a package with sane defaults to complement org-mode
;; with evil-mode.

;;; Code:

(require 'evil)
(require 'org)

(define-minor-mode syndicate-mode
  "Buffer-specific minor mode for evil-org."
  :init-value nil
  :lighter " Syn"
  :keymap (make-sparse-keymap)
  :group 'syndicate-mode)

(add-hook 'org-mode-hook 'syndicate-mode)

;; recompute clocks in visual selection

(evil-define-operator syndicate-recompute-clocks (beg end type register yank-handler)
  :keep-visual t
  :move-point nil
  (interactive "<r>")
  (progn
    (message "start!" )
    (save-excursion
      (while (< (point) end)
        (org-evaluate-time-range)
        (forward-line)
        (message "at position %S" (point))))))

(defun syndicate-eol-then (fun)
  "Go to end of line and then execute function FUN."
  (end-of-line)
  (funcall fun)
  (evil-append nil))

(defun insert-item-below ()
  "Try to infer what to insert."
  (if (not (org-in-item-p))
      (insert "\n")
    (org-insert-item)))

;; open org-mode links in visual selection

(defun syndicate-generic-open-links (beg end type register yank-handler incog)
  (progn
    (save-excursion
      (goto-char beg)
      (catch 'break
        (while t
          (org-next-link)
          ;;; break from outer loop when there are no more
          ;;; org links
          (when (or
                 (not (< (point) end))
                 (not (null org-link-search-failed)))
            (throw 'break 0))

          (if (not (null incog))
              (let* ((new-arg
                      ;;; if incog is true, decide which incognito settings to
                      ;;; use dependening on the browser
                      (cond ((not (null (string-match "^.*\\(iceweasel\\|firefox\\).*$" browse-url-generic-program)))  "--private-window")
                            ((not (null (string-match "^.*\\(chrome\\|chromium\\).*$"  browse-url-generic-program)))   "--incognito"     )
                            (t "")
                            ))
                     (old-b (list browse-url-generic-args " " ))
                     (browse-url-generic-args (add-to-ordered-list 'old-b new-arg 0)))
                (progn
                  (org-open-at-point)))
            (let ((browse-url-generic-args '("")))
              (org-open-at-point))))))))

;;; open links in visual selection
(evil-define-operator syndicate-open-links (beg end type register yank-handler)
  :keep-visual t
  :move-point nil
  (interactive "<r>")
  (syndicate-generic-open-links beg end type register yank-handler nil))

;;; open links in visual selection in incognito mode
(evil-define-operator syndicate-open-links-incognito (beg end type register yank-handler)
  :keep-visual t
  :move-point nil
  (interactive "<r>")
  (syndicate-generic-open-links beg end type register yank-handler t))

(evil-define-key 'normal syndicate-mode-map
  "gh" 'outline-up-heading
  "gj" 'org-forward-heading-same-level
  "gk" 'org-backward-heading-same-level
  "gl" 'outline-next-visible-heading
  "<" 'org-metaleft
  ">" 'org-metaright
  "t" 'org-todo
  "o" '(lambda () (interactive) (syndicate-eol-then 'insert-item-below)))

(provide 'syndicate)

;;; syndicate.el ends here
