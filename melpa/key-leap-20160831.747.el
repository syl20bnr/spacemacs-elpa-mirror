;;; key-leap.el --- Leap between lines by typing keywords

;; Copyright (C) 2015  Martin Rykfors

;; Author: Martin Rykfors <martinrykfors@gmail.com>
;; Version: 0.4.1
;; Package-Version: 20160831.747
;; URL: https://github.com/MartinRykfors/key-leap
;; Keywords: point, convenience
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a full copy of the GNU General Public License see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; key-leap-mode allows you to quickly jump to any visible line in a
;; window.  When key-leap-mode is enabled, it will populate the margin
;; of every line with an unique keyword.  By calling the interactive
;; command `key-leap-start-matching' the keywords become active.
;; Typing the keyword of a line in this state will move the point to
;; the beginning of that line.

;; You can change the way key-leap-mode generates its keys by setting
;; the variable `key-leap-key-strings'.  This is a list of strings
;; that specify what chars to use for each position in the keys.  For
;; example, adding this to your init file
;;
;; (setq key-leap-key-strings '("htn" "ao" "ht"))
;;
;; will make key-leap-mode generate and use the following keys:
;; hah hat hoh hot tah tat toh tot nah nat noh not

;; You are not restricted to just three-letter keywords.  By providing
;; 4 different strings, for instance, key-leap will use 4 letters for
;; every keyword.

;; You should provide a large enough number of different characters
;; for key-leap to use.  The number of combinations of characters
;; should be bigger than the number of possible visible lines for your
;; setup, but not too much bigger than that.

;; By default, key-leap-mode will generate 125 keywords from the
;; home-row of a qwerty keyboard layout, in a right-left-right
;; fashion.

;; When calling `key-leap-start-matching' it will run the hooks
;; `key-leap-before-leap-hook' and `key-leap-after-leap-hook'. For
;; instance, to make key-leap-mode move to indentation after leaping,
;; add the following to your config:
;;
;; (add-hook 'key-leap-after-leap-hook 'back-to-indentation)

;; When set to nil, `key-leap-upcase-active' will not make the active
;; parts of the keys upper-cased.  The default is t.

;; The faces for the active and inactive parts of the keys are
;; specified by the faces `key-leap-active' and `key-leap-inactive'
;; respectively.

;; Key-leap can be integrated with `evil-mode' by adding the function
;; `key-leap-create-evil-motion' to your Emacs config.  This creates
;; an evil motion called `key-leap-evil-motion'.  It works the same
;; way as `key-leap-start-matching', but it will also work with evil
;; features like operators, visual state and the jump list.  Please
;; see the documentation for `key-leap-create-evil-motion' for more
;; information.

;;; Code:

(require 'linum)
(require 'cl-lib)

(defgroup key-leap nil
  "Leap to any visible line by typing a keyword."
  :group 'convenience)

(defcustom key-leap-upcase-active t
  "If set to t, `key-leap-mode' will make active characters of the keys upper-cased when waiting for the key input."
  :group 'key-leap
  :type 'boolean
  :version "0.1.0")

(defface key-leap-inactive
  '((t :inherit (linum default)))
  "Face to use for the inactive parts of the keys."
  :group 'key-leap
  :version "0.1.0")

(defface key-leap-active
  '((t :inherit (linum default) :foreground "#FF0000"))
  "Face to use for the parts of the keys that are still being matched."
  :group 'key-leap
  :version "0.1.0")

(defcustom key-leap-key-strings '("hjkl;" "gfdsa" "hjkl;")
  "A list of strings from which the key-leap keys are constructed.  The first list specifies the characters to use for the first position of every key and so on."
  :group 'key-leap
  :type '(repeat string)
  :version "0.1.0")

(defvar key-leap--key-chars)

(defcustom key-leap-after-leap-hook nil
  "Hook that runs after `key-leap-start-matching' has jumped to a new line."
  :type 'hook
  :group 'key-leap
  :version "0.2.0")

(defcustom key-leap-before-leap-hook nil
  "Hook that runs just before `key-leap-start-matching' jumps to a new line."
  :type 'hook
  :group 'key-leap
  :version "0.4.0")

;;;###autoload
(define-minor-mode key-leap-mode
  "Leap between visible lines by typing short keywords."
  :lighter nil
  :keymap (let ((key-map (make-sparse-keymap)))
            (define-key key-map (kbd "C-c #") 'key-leap-start-matching)
            key-map)
  (if key-leap-mode
      (progn
        (key-leap--cache-keys)
        (add-hook 'after-change-functions 'key-leap--after-change nil t)
        (add-hook 'window-scroll-functions 'key-leap--window-scrolled nil t)
        (add-hook 'change-major-mode-hook 'key-leap--clean-current-buffer nil t)
        (add-hook 'window-configuration-change-hook 'key-leap--update-current-buffer nil t)
        (add-hook 'post-command-hook 'key-leap--update-current-buffer nil t)
        (key-leap--update-current-buffer))
    (progn
      (remove-hook 'after-change-functions 'key-leap--after-change t)
      (remove-hook 'window-scroll-functions 'key-leap--window-scrolled t)
      (remove-hook 'change-major-mode-hook 'key-leap--clean-current-buffer t)
      (remove-hook 'window-configuration-change-hook 'key-leap--update-current-buffer t)
      (remove-hook 'post-command-hook 'key-leap--update-current-buffer t)
      (key-leap--clean-current-buffer))))

(defun key-leap--tree-size (level)
  "Return how many nodes are below one node at LEVEL."
  (cl-reduce '* (cl-mapcar 'length key-leap--key-chars) :start level))

(defun key-leap--tree-sizes ()
  "Return a list of numbers, specifying how many nodes are below each level in the tree."
  (cl-mapcar 'key-leap--tree-size (number-sequence 1 (length key-leap--key-chars))))

(defun key-leap--coords-from-index (index)
  "Convert INDEX to a list of indices into the strings in `key-leap-key-strings'."
  (cl-mapcar (lambda (level)
            (/ (mod index (key-leap--tree-size (- level 1))) (key-leap--tree-size level)))
          (number-sequence 1 (length key-leap--key-chars))))

(defun key-leap--index-from-key-string (key-string)
  "Convert KEY-STRING to its corresponding index in the list `key-leap--all-keys'."
  (let* ((char-list (string-to-list key-string))
         (coordinates (cl-mapcar 'cl-position char-list key-leap--key-chars)))
    (cl-reduce '+ (cl-mapcar '* (key-leap--tree-sizes) coordinates))))

(defun key-leap--coords-to-string (coords)
  "Convert the list COORDS into a key string."
  (apply 'string (cl-mapcar 'nth coords key-leap--key-chars)))

(defvar key-leap--all-keys nil)
(defvar key-leap--num-keys nil)

(defun key-leap--listify-string (string)
  "Convert STRING into a list of chars."
  (let ((len (length string)))
    (mapcar
     (lambda (n)
       (string-to-char (substring string n len)))
     (number-sequence 0 (- len 1)))))

(defun key-leap--cache-keys ()
  "Convert `key-leap-key-strings' into a vector of keys and store it."
  (setq key-leap--key-chars (mapcar 'key-leap--listify-string key-leap-key-strings))
  (setq key-leap--all-keys (apply 'vector (mapcar (lambda (n)
                                             (key-leap--coords-to-string (key-leap--coords-from-index n)))
                                           (number-sequence 0 (- (key-leap--tree-size 0) 1)))))
  (setq key-leap--num-keys (length key-leap--all-keys)))

(defvar-local key-leap--current-key "*")

(defun key-leap--leap-to-current-key ()
  "Move the point to the line given by the current entered key."
  (run-hooks 'key-leap-before-leap-hook)
  (goto-char (window-start))
  (forward-visible-line (key-leap--index-from-key-string key-leap--current-key))
  (run-hooks 'key-leap-after-leap-hook))

(defun key-leap--color-substring (str use-active-face)
  "Add properties to key string STR.  When USE-ACTIVE-FACE is t, apply the activation face to it."
  (if (and use-active-face
           (string-match (concat "\\(^" key-leap--current-key "\\)\\(.*\\)") str))
      (concat
       (propertize (match-string 1 str) 'face 'key-leap-inactive)
       (let* ((active-str (match-string 2 str))
              (cased-str (if key-leap-upcase-active (upcase active-str) active-str)))
         (propertize cased-str 'face 'key-leap-active)))
    (propertize str 'face 'key-leap-inactive)))

(defvar-local key-leap--buffer-overlays nil "List of key-leap overlays visible in the current buffer.")
(defvar-local key-leap--available-buffer-overlays nil "List of key-leap overlays that can be reused in the current buffer.")

(defun key-leap--get-overlay (beg end)
  "Return an overlay from BEG to END.  Reuse available when possible."
  (if key-leap--available-buffer-overlays
      (let ((overlay-to-move (pop key-leap--available-buffer-overlays)))
        (move-overlay overlay-to-move beg end)
        (push overlay-to-move key-leap--buffer-overlays)
        overlay-to-move)
    (let ((new-overlay (make-overlay beg end)))
      (push new-overlay key-leap--buffer-overlays)
      new-overlay)))

(defun key-leap--place-overlay (win key-index)
  "Place an overlay in WIN for showing a margin text for the key with KEY-INDEX."
  (let* ((ol (key-leap--get-overlay (point) (+ 1 (point))))
         (str (elt key-leap--all-keys key-index))
         (colored-string (key-leap--color-substring str (eq (selected-window) win))))
    (overlay-put ol 'window win)
    (overlay-put ol 'before-string
                 (propertize " " 'display`((margin left-margin) ,colored-string)))))

(defun key-leap--delete-overlays ()
  "Delete all visible key-leap overlays in the current buffer."
  (dolist (ol key-leap--buffer-overlays)
    (delete-overlay ol)
    (push ol key-leap--available-buffer-overlays)
    (pop key-leap--buffer-overlays)))

(defun key-leap--update-margin-keys (win)
  "Populate WIN with new overlays for showing keys in the margin."
  (set-window-margins win (length key-leap--key-chars))
  (let ((start (line-number-at-pos (window-start win)))
        (limit (- key-leap--num-keys 1))
        (continue t))
    (save-excursion
      (goto-char (window-start win))
      (unless (bolp) (forward-visible-line 1))
      (let ((line (line-number-at-pos)))
        (while (and continue (<= (- line start) limit))
          (when (or (not (eobp))
                    (and (eobp) (= (point) (line-beginning-position))))
            (key-leap--place-overlay win (- line start)))
          (setq line (+ 1 line))
          (when (eobp) (setq continue nil))
          (forward-visible-line 1))))))

(defun key-leap--after-change (beg end len)
  "Run when buffer is changed from BEG to END with length LEN."
  (when (or (= beg end)
            (string-match "\n" (buffer-substring-no-properties beg end)))
    (key-leap--update-current-buffer)))

(defun key-leap--clean-current-buffer ()
  "Delete all key-leap overlays from the current buffer and reset the margins of all windows showing it."
  (dolist (ol key-leap--buffer-overlays)
    (delete-overlay ol))
  (setq key-leap--buffer-overlays nil)
  (dolist (win (get-buffer-window-list (current-buffer) nil t))
    (set-window-margins win 0 (cdr (window-margins win)))))

(defun key-leap--update-buffer (buffer)
  "Refresh the key-leap overlays in BUFFER."
  (with-current-buffer buffer
    (when key-leap-mode
      (key-leap--delete-overlays)
      (dolist (win (get-buffer-window-list buffer nil t))
        (key-leap--update-margin-keys win)))))

(defun key-leap--window-scrolled (win beg)
  "Run when WIN is scrolled with its top aligned with BEG."
  (key-leap--update-buffer (window-buffer win)))

(defun key-leap--update-current-buffer ()
  "Update the overlays of the current buffer."
  (key-leap--update-buffer (current-buffer)))

(defun key-leap--reset-match-state ()
  "Clear the key input and update the overlays in the current buffer."
  (setq key-leap--current-key "*")
  (key-leap--update-current-buffer))

(defun key-leap--append-char (valid-chars char-source-function)
  "Read next char from input.  Signal error if the char is not in VALID-CHARS.  Use CHAR-SOURCE-FUNCTION for reading input."
  (let ((input-char (funcall char-source-function (concat (propertize "Enter key: " 'face 'minibuffer-prompt) key-leap--current-key))))
    (if (member input-char valid-chars)
        (setq key-leap--current-key (concat key-leap--current-key (char-to-string input-char)))
      (progn
        (key-leap--reset-match-state)
        (error "Key-leap matching stopped - no such key available")))))

(defun key-leap--read-keys (char-source-function)
  "Repeatedly wait for char input from CHAR-SOURCE-FUNCTION until a complete key has been typed."
  (setq key-leap--current-key "")
  (dolist (position-chars key-leap--key-chars)
    (key-leap--update-current-buffer)
    (key-leap--append-char position-chars char-source-function)))

(defun key-leap-start-matching ()
  "Wait for the user to type the characters of a key in the margin, and then jump to the corresponding line."
  (interactive)
  (let ((inhibit-quit t)
        (echo-keystrokes 0))
    (if key-leap-mode
        (progn
          (message "")
          (with-local-quit
            (condition-case nil
                (progn
                  (key-leap--read-keys (lambda (msg) (read-char msg)))
                  (key-leap--leap-to-current-key))
              (error (key-leap--reset-match-state))))
          (key-leap--reset-match-state))
      (error "Key-leap-mode not enabled in this buffer"))))

(defun key-leap-create-evil-motion (&optional key)
  "Use key-leap as an evil motion, bound to KEY.
This function defines a new evil motion called
`key-leap-evil-motion' that allows you to use key-leap together
with evil features like operators, visual state and the jump
list.  May only be called after key-leap and evil have been
loaded.  When KEY is omitted, only the motion will be defined and
no key binding will be created."
  (defvar key-leap-evil-motion)
  (defvar evil-motion-state-map)
  (evil-define-motion key-leap-evil-motion ()
    "Motion for moving between lines, similar to `key-leap-start-matching'."
      :type line
      :jump t
      (key-leap-start-matching))
  (when key
    (define-key evil-motion-state-map key 'key-leap-evil-motion)))

(provide 'key-leap)

;;; key-leap.el ends here
