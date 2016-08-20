;;; anything-sage.el --- An anything extension for sage-shell-mode.

;; Copyright (C) 2012-2014 Sho Takemori.
;; Author: Sho Takemori <stakemorii@gmail.com>
;; URL: https://github.com/stakemori/anything-sage
;; Package-Version: 20141005.413
;; Keywords: Sage, math, anything
;; Version: 0.0.1
;; Package-Requires: ((cl-lib "0.5") (anything "1.3.9") (sage-shell-mode "0.0.1"))

;;; License
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
;; anything-sage provides 3 commands.
;; Bind them to some keys: e.g.
;; (defun anything-sage-set-up ()
;;   (local-set-key (kbd "C-c C-i") 'anything-sage-shell)
;;   (local-set-key (kbd "C-c C-d") 'anything-sage-shell-describe-object-at-point)
;;   (local-set-key (kbd "M-r") 'anything-sage-command-history))
;; (add-hook 'sage-shell-mode-hook 'anything-sage-set-up)

;;; Code:
(require 'cl-lib)
(require 'anything)
(require 'anything-match-plugin)
(require 'sage-shell-mode)

(defgroup anything-sage
  nil "an anything source for `sage-shell-mode'."
  :group 'anything)

(defvar anything-sage-action-alist
  '(("Insert" . anything-sage-objcts-insert-action)
    ("View Docstring" . anything-sage-show-doc)))

(defvar anything-sage-common-alist
  '((init . anything-sage-init)
    (candidates-in-buffer)))

(defvar anything-sage-additional-action-alist
  '(("View Source File" . (lambda (can)
                            (sage-shell:find-source-in-view-mode
                             (sage-shell-cpl:to-objname-to-send can))))))

(defvar anything-c-source-sage-objects
  `(,@anything-sage-common-alist
    (name . "Sage Objects")
    (action . ,(append anything-sage-action-alist
                       anything-sage-additional-action-alist))))

(defvar anything-c-source-sage-help
  `(,@anything-sage-common-alist
    (name . "Sage Documents")
    (action . ,(append (reverse anything-sage-action-alist)
                       anything-sage-additional-action-alist))))

(defcustom anything-sage-candidate-regexp (rx alnum (zero-or-more (or alnum "_")))
  "Regexp used for collecting Sage attributes and functions."
  :group 'anything-sage
  :type 'regexp)

(defconst anything-sage-cands-buf-name " *anything Sage*")

(defun anything-sage-init ()
  (let ((cands (sage-shell-cpl:candidates-sync anything-sage-candidate-regexp)))
    (with-current-buffer (get-buffer-create anything-sage-cands-buf-name)
      (erase-buffer)
      (dolist (can cands)
        (insert (format "%s\n" can)))
      (anything-candidate-buffer (current-buffer)))))

(defun anything-sage-objcts-insert-action (can)
  (with-current-buffer anything-current-buffer
    (sage-shell:insert-action can)))

;;;###autoload
(defun anything-sage-shell ()
  (interactive)
  (anything
   :sources '(anything-c-source-sage-objects)
   :input (sage-shell:word-at-point)
   :buffer "*anything Sage*"))

;;;###autoload
(defun anything-sage-shell-describe-object-at-point ()
  (interactive)
  (anything
   :sources '(anything-c-source-sage-help)
   :input (sage-shell:word-at-point)
   :buffer "*anything Sage*"))

(defun anything-sage-show-doc (can)
  (if (sage-shell:at-top-level-p)
      (sage-shell-help:describe-symbol
       (sage-shell-cpl:to-objname-to-send can))
    (message "Document help is not available here.")))



(defvar anything-sage-commnd-list-cached nil)

(defvar anything-sage-candidates-number-limit 100)

(defvar anything-c-source-sage-command-history
  `((name . "Sage Command History")
    (init . anything-sage-make-command-list)
    (action . (("Insert" . anything-sage-objcts-insert-action)))
    (candidates . (lambda () anything-sage-commnd-list-cached))
    (candidate-number-limit . ,anything-sage-candidates-number-limit)))

(defun anything-sage-make-command-list ()
  (setq anything-sage-commnd-list-cached
        (cl-loop for i from 0 to (ring-size comint-input-ring)
              collect (ring-ref comint-input-ring i))))

;;;###autoload
(defun anything-sage-command-history ()
  (interactive)
  (anything
   :sources '(anything-c-source-sage-command-history)
   :buffer "*anything Sage*"))


(provide 'anything-sage)
;;; anything-sage.el ends here
