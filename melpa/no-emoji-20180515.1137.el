;;; no-emoji.el --- Show :emoji-name: instead of emoji characters -*-coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2018 craven@gmx.net

;; Author: Peter <craven@gmx.net>
;; URL: https://github.com/ecraven/no-emoji
;; Package-Version: 20180515.1137
;; Package-X-Original-Version: 20180515
;; Package-Requires: ((emacs "24"))
;; Version: 0.2
;; Keywords: extensions
;; Created: 2017-11-29

;;; License:

;; Licensed under the GPLv3.

;;; Commentary:
;;
;; Run M-x no-emoji-minor-mode to replace all emoji with :emoji-name: in the current buffer.
;;
;; Run M-x global-no-emoji-minor-mode to replace all emoji with :emoji-name: in all buffers.
;;
;; You can customize the `no-emoji' face to alter the appearance.
;;
;; You can adapt the codepoint ranges in `no-emoji-codepoint-ranges' to customize which codepoints will be replaced.
;;
;; You can redefine `no-emoji-displayable-unicode-name' to change the way the display names are generated.
;; Do this *before* enabling the minor mode.
;;
;; This package sets buffer-display-table locally.
;;
;;; Code:
(defgroup no-emoji nil
  "Minor mode for replacing emoji with their names."
  :group 'multimedia
  :prefix "no-emoji-")

(defcustom no-emoji-codepoint-ranges
  '((#x1f000 . #x1f9ff))
  "A list of codepoint ranges (inclusive) that will be replaced."
  :type '(alist :key-type (character :tag "First character")
                :value-type (character :tag "Last character"))
  :group 'no-emoji)

(defface no-emoji `((t (:inherit dired-header)))
  "Face used to highlight emoji replacement text."
  :group 'no-emoji)

(defun no-emoji-displayable-unicode-name (name)
  "Convert NAME to the string that should be shown.

E.g. convert spaces to -, surround with :."
  (concat ":" (replace-regexp-in-string " " "-" (downcase name)) ":"))

(defun no-emoji--update-display-table (dt fill-p)
  "If FILL-P is true, enter the relevant glyphs into the buffer-local display-table.

If it is false, remove them.

Process every character defined by the ranges in `no-emoji-codepoint-ranges'.
Set `no-emoji' as the face for each glyph."
  (dolist (range no-emoji-codepoint-ranges)
    (dotimes (i (- (cdr range) (car range)))
      (let ((codepoint (+ (car range) i)))
        (let ((name (get-char-code-property codepoint 'name)))
          (when name
            (aset dt
                  codepoint
                  (if fill-p
                      (vconcat (mapcar
                                (lambda (c)
                                  (make-glyph-code c 'no-emoji))
                                (string-to-list (no-emoji-displayable-unicode-name name))))
                    nil)))))))
  dt)

;;;###autoload
(define-minor-mode no-emoji-minor-mode
  "Show emoji as :emoji-name:

Also see `no-emoji-codepoint-ranges' and `no-emoji-displayable-unicode-name'."
  :init-value nil
  :lighter " nemo"
  (progn
    (when no-emoji-minor-mode
        (unless buffer-display-table
          (setq-local no-emoji--no-dt t)
          (setq buffer-display-table (make-display-table))))
    (no-emoji--update-display-table buffer-display-table no-emoji-minor-mode)
    (when (and (not no-emoji-minor-mode)
               no-emoji--no-dt)
      (kill-local-variable 'buffer-display-table)
      (kill-local-variable 'no-emoji--no-dt))))

(define-minor-mode global-no-emoji-minor-mode
  "Show emoji as :emoji-name: in every buffer.

Also see `no-emoji-codepoint-ranges' and `no-emoji-displayable-unicode-name'."
  :global t
  :init-value nil
  :lighter " *nemo"
  (no-emoji--update-display-table standard-display-table global-no-emoji-minor-mode))

(provide 'no-emoji)
;;; no-emoji.el ends here
