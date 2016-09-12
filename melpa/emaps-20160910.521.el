;;; emaps.el --- utilities for working with keymaps.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Ben Moon <guiltydolphin@gmail.com>
;; Version: 0.1.0
;; Package-Version: 20160910.521
;; Package-Requires: ()
;; Keywords: convenience, keyboard, keymap, utility
;; URL: https://github.com/GuiltyDolphin/emaps

;;; Commentary:

;; Emaps provides utilities for working with keymaps and keybindings in Emacs.
;;
;; Emaps provides the `emaps-define-key' function that provides the same
;; functionality as `define-key', but allows multiple keys to be defined
;; at once, for example:
;;
;;    (emaps-define-key keymap
;;      "a" 'fun-a
;;      "b" 'fun-b
;;      "c" 'fun-c) ; etc.
;;
;; Emaps also provides the following functions for viewing keymaps:
;;
;;    * `emaps-describe-keymap-bindings' provides a *Help* buffer similar
;;       to `describe-bindings', but works for any keymap.
;;    * `emaps-describe-keymap' provides a *Help* buffer similar to
;;      `describe-variable', but attempts to normalize character display
;;      where possible.

;;; Code:

(defgroup emaps nil
  "Utilities for working with keymaps."
  :group 'convenience
  :group 'keyboard
  :prefix 'emaps-)

(defcustom emaps-key-face 'font-lock-constant-face
  "Face used by when displaying keys."
  :group 'emaps
  :type 'face)

(defmacro emaps--with-modify-help-buffer (body)
  "Execute BODY with the current help buffer active; allow modifications."
  `(with-current-buffer (get-buffer "*Help*")
     (let ((buffer-read-only nil))
       ,body
       (set-buffer-modified-p nil))))

(defun emaps--completing-read-variable (prompt &optional pred)
  "Prompt the user with PROMPT for a variable that satisfied PRED (if supplied)."
  (let ((v (variable-at-point))
        (enable-recursive-minibuffers t)
        (check
         (lambda (it)
           (and (symbolp it)
                (boundp it)
                (if pred (funcall pred (symbol-value it)) t))))
        vars
        val)
    (mapatoms (lambda (atom) (when (funcall check atom) (push atom vars))))
    (setq val (completing-read
               (if (funcall check v)
                   (format
                    (concat prompt " (default %s): ") v)
                 (concat prompt ": "))
               vars
               check
               t nil nil
               (if (symbolp v) (symbol-name v))))
    (list (if (equal val "") v (intern val)))))

(defun emaps--read-keymap ()
  "Read the name of a keymap from the minibuffer and return it as a symbol."
  (emaps--completing-read-variable "Enter keymap" 'keymapp))

;;;###autoload
(defun emaps-describe-keymap (keymap)
  "Display the full documentation of KEYMAP (a symbol).

Unlike `describe-variable', this will display characters as strings rather than integers."
  (interactive (emaps--read-keymap))
  (describe-variable keymap)
  (emaps--with-modify-help-buffer
   (save-excursion
     (while (search-forward-regexp "(\\([0-9]+\\) ." nil t)
       (let ((keychar (string-to-number (match-string 1))))
         (when (characterp keychar)
           (replace-match (propertize (char-to-string keychar) 'face emaps-key-face) nil t nil 1)))))))

(defun emaps--get-available-binding-as-string (prefix)
  "Repeat PREFIX until there is an available binding (and return it as a string)."
  (let ((repeated prefix))
    (while (key-binding (kbd repeated))
      (setq repeated (concat repeated " " repeated)))
    repeated))

;;;###autoload
(defun emaps-describe-keymap-bindings (keymap)
  "Like `describe-bindings', but only describe bindings in KEYMAP."
  (interactive (emaps--read-keymap))
  (let* ((keymap-name (if (symbolp keymap) (symbol-name keymap) "?"))
         (keymap (if (symbolp keymap) (symbol-value keymap) keymap))
         (temp-map '(keymap))
         (prefix (emaps--get-available-binding-as-string "a")))
    (define-key temp-map (kbd prefix) keymap)
    (with-help-window (help-buffer)
      (with-current-buffer (help-buffer)
        (let ((overriding-terminal-local-map temp-map)
              (overriding-local-map temp-map)
              (overriding-local-map-menu-flag t)
              (check-buffer (current-buffer))
              (buffer-read-only nil))
          (describe-buffer-bindings check-buffer (kbd prefix))
          (goto-char (point-min))
          (save-excursion
            (search-forward-regexp "^key")
            (delete-region (point-min) (match-beginning 0))
            (search-forward-regexp "\C-l\nGlobal")
            (delete-region (match-beginning 0) (point-max)))
          (save-excursion
            (insert (format "Describing bindings for '%s\n" keymap-name))
            (while (search-forward-regexp (format "\\(^\\|..\\)\\(%s \\)" prefix) nil t)
              (replace-match "" nil nil nil 2)))
          (set-buffer-modified-p nil))))))

;;;###autoload
(defun emaps-keymap-for-mode (mode)
  "Return the keymap for MODE (or NIL if none exists)."
  (let ((mode-map-symbol (intern (concat (symbol-name mode) "-map"))))
    (when (boundp mode-map-symbol)
      (symbol-value mode-map-symbol))))

;;;###autoload
(defun emaps-define-key (keymap key def &rest bindings)
  "Create a binding in KEYMAP from KEY to DEF and each key def pair in BINDINGS.

See `define-key' for the forms that KEY and DEF may take."
  (let ((defs (append (list key def) bindings)))
    (dotimes (n (/ (length defs) 2))
      (let ((key (nth (* n 2) defs))
            (def (nth (+ (* n 2) 1) defs)))
        (define-key keymap key def)))))
(put 'emaps-define-key 'lisp-indent-function 'defun)

(provide 'emaps)
;;; emaps.el ends here
