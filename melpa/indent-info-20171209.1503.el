;;; indent-info.el --- show indentation information in status bar

;; Copyright (C) 2017 Terje Larsen
;; All rights reserved.

;; Author: Terje Larsen <terlar@gmail.com>
;; URL: https://github.com/terlar/indent-info.el
;; Package-Version: 20171209.1503
;; Keywords: convenience, tools
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; indent-info is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; indent-info is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `indent-info' is a small Emacs minor mode that provides information
;; about currently configured indentation mode as well as tab-width in the
;; status bar.

;;; Code:

(defgroup indent-info nil
  "Display indentation information in mode line."
  :group 'modeline)

(defcustom indent-info-insert-target 'mode-line-position
  "Target list for insertion of `indent-info-mode'."
  :type 'symbol
  :group 'indent-info)

(defcustom indent-info-insert-position 'before
  "Position for insertion of `indent-info-mode'.
Choices are `before', `after'."
  :type '(choice (const :tag "Before insert target" before)
                 (const :tag "After insert target" after))
  :group 'indent-info)

(defcustom indent-info-prefix " "
  "Text to display before the indentation info in the mode line."
  :type 'string
  :group 'indent-info)

(defcustom indent-info-suffix " "
  "Text to display after the indentation info in the mode line."
  :type 'string
  :group 'indent-info)

(defcustom indent-info-tab-format "Tab Size: %s"
  "Tab indentation format."
  :type 'string
  :group 'indent-info)

(defcustom indent-info-space-format "Spaces: %s"
  "Space indentation format."
  :type 'string
  :group 'indent-info)

(defcustom indent-info-use-symbols nil
  "Indicates whether to use symbols for the `tab-width' number or not."
  :type '(choice (boolean :tag "Symbols"))
  :group 'indent-info)

(defcustom indent-info-tab-width-min 2
  "Min `tab-width' for `tab-width' cycling."
  :type 'integer
  :group 'indent-info)

(defcustom indent-info-tab-width-max 8
  "Max `tab-width' for `tab-width' cycling."
  :type 'integer
  :group 'indent-info)

(defcustom indent-info-tab-width-step 2
  "Step to use for `tab-width' cycling."
  :type 'integer
  :group 'indent-info)

(defcustom indent-info-number-symbol-alist
  '((1  . "➀")
    (2  . "②")
    (3  . "➂")
    (4  . "④")
    (5  . "➄")
    (6  . "➅")
    (7  . "➆")
    (8  . "⑧")
    (9  . "➈")
    (10 . "➉"))
  "List of `tab-width' number mappings.
Each element is a list of the form (NUMBER . SYMBOL)."
  :type '(alist :key-type (integer :tag "Number")
                :value-type (string :tag "Symbol"))
  :group 'indent-info)

(defcustom indent-info-minor-mode-text-properties
  '('local-map
    '(keymap
      (mode-line keymap
                 (mouse-1 . indent-info-toggle-indent-mode)
                 (mouse-4 . indent-info-cycle-tab-width-increase)
                 (mouse-5 . indent-info-cycle-tab-width-decrease)))
    'help-echo
    "Indentation\n\ mouse-1: Toggle tabs/spaces\n\ mouse-4: Increase tab-width\n\ mouse-5: Decrease tab-width"
    'mouse-face 'mode-line-highlight)
  "List of text properties to apply to the `indent-info' mode line."
  :type '(repeat sexp)
  :group 'indent-info)

(defvar indent-info-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-~") 'indent-info-toggle-indent-mode)
    (define-key map (kbd "C-M->") 'indent-info-cycle-tab-width-increase)
    (define-key map (kbd "C-M-<") 'indent-info-cycle-tab-width-decrease)
    map)
  "The keymap for `indent-info-mode'.")

(defun indent-info-mode-line ()
  "The mode line with menu and content."
  (concat indent-info-prefix
          (eval
           `(propertize
             ,(indent-info-mode-line-text)
             ,@indent-info-minor-mode-text-properties))
          indent-info-suffix))

(defun indent-info-mode-line-text ()
  "The indentation information text."
  (let ((fmt (if (eq indent-tabs-mode t)
                 indent-info-tab-format
               indent-info-space-format)))
    (if indent-info-use-symbols
        (format fmt (cdr (assoc tab-width indent-info-number-symbol-alist)))
      (format fmt (int-to-string tab-width)))))

(defun indent-info-mode-enable ()
  "Enable `indent-info-mode' in the current buffer."
  (unless (minibufferp)
    (indent-info-mode 1)))

;;;###autoload
(defun indent-info-toggle-indent-mode ()
  "Toggle indentation modes between tabs and spaces."
  (interactive)
  (setq indent-tabs-mode
        (if (eq indent-tabs-mode t) nil t))
  (let ((mode (if (eq indent-tabs-mode t) "tabs" "spaces")))
    (message "Set indentation mode to %s." mode)
    (force-mode-line-update)))

;;;###autoload
(defun indent-info-cycle-tab-width-increase ()
  "Cycle `tab-width' increasing with `indent-info-tab-width-step'.
When reaching `indent-info-tab-width-max' it won't do anything."
  (interactive)
  (let ((width (+ tab-width indent-info-tab-width-step)))
    (when (<= width indent-info-tab-width-max)
      (setq tab-width width)
      (message "Set tab-width to %d." width)
      (force-mode-line-update))))

;;;###autoload
(defun indent-info-cycle-tab-width-decrease ()
  "Cycle `tab-width' decreasing with `indent-info-tab-width-step'.
When reaching `indent-info-tab-width-min' it won't do anything."
  (interactive)
  (let ((width (- tab-width indent-info-tab-width-step)))
    (when (>= width indent-info-tab-width-min)
      (setq tab-width width)
      (message "Set tab-width to %d." width)
      (force-mode-line-update))))

;;;###autoload
(define-minor-mode indent-info-mode
  "Toggle indent-info mode
With no argument, this command toggles the mode.
A non-null prefix argument turns the mode on.
A null prefix argument turns it off.

When enabled, information about the currently configured `indent-tabs-mode' and
`tab-width' is displayed in the mode line."
  :lighter nil
  :global nil
  :keymap indent-info-mode-map
  (if indent-info-mode
      (add-to-list indent-info-insert-target
                   '(indent-info-mode (:eval (indent-info-mode-line)))
                   (eq indent-info-insert-position 'after))
    (set indent-info-insert-target
         (assq-delete-all 'indent-info-mode (symbol-value indent-info-insert-target)))))

;;;###autoload
(define-global-minor-mode global-indent-info-mode
  indent-info-mode indent-info-mode-enable
  :require 'indent-info
  :group 'indent-info)

(provide 'indent-info)

;;; indent-info.el ends here
