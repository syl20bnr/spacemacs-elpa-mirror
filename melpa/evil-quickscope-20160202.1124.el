;;; evil-quickscope.el --- Highlight unique characters in words for f,F,t,T navigation

;; Copyright (C) 2016 Michael Chen

;; Author: Michael Chen <blorbx@gmail.com>
;; Maintainer: Michael Chen <blorbx@gmail.com>
;; Created: 12 Aug 2015
;; Version: 0.1.4
;; Package-Version: 20160202.1124

;; Homepage: http://github.com/blorbx/evil-quickscope
;; Keywords: faces, emulation, vim, evil
;; Package-Requires: ((evil "0"))

;; This file is not part of GNU Emacs.

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

;;; Commentary:

;; This package emulates quick_scope.vim by Brian Le
;; (https://github.com/unblevable/quick-scope). It highlights targets for
;; evil-mode's f,F,t,T keys, allowing for quick navigation within a line with no
;; additional mappings.
;;
;; The functionality is wrapped into two different minor modes. Only one can be
;; activated at a time.

;; evil-quickscope-always-mode provides targets at all times and directly
;; emulates quick_scope.vim. It can be activated by adding the following to
;; ~/.emacs:
;;
;;     (require 'evil-quickscope)
;;     (global-evil-quickscope-always-mode 1)
;;
;; Alternatively, you can enable evil-quickscope-always-mode in certain modes by
;; adding 'turn-on-evil-quickscope-always-mode' to the mode hook. For example:
;;
;;     (add-hook 'prog-mode-hook 'turn-on-evil-quickscope-always-mode)
;;
;; evil-quickscope-mode provides targets only after one of the f,F,t,T keys are
;; pressed. It can be activated by adding the following to ~/.emacs:
;;
;;     (require 'evil-quickscope)
;;     (global-evil-quickscope-mode 1)
;;
;; Or, you can use 'turn-on-evil-quickscope-mode' as a mode hook:
;;
;;     (add-hook 'prog-mode-hook 'turn-on-evil-quickscope-mode)
;;
;; This program requires EVIL (http://bitbucket.org/lyro/evil/wiki/Home)

;;; Code:

(require 'evil)

(defgroup evil-quickscope nil
  "Target highlighting for evil-mode's f,F,t,T keys."
  :group 'evil)

;;; Faces
(defface evil-quickscope-first-face
  '((t (:inherit font-lock-constant-face :underline t)))
  "Face for first unique character."
  :group 'evil-quickscope)

(defface evil-quickscope-second-face
  '((t (:inherit font-lock-keyword-face :underline t)))
  "Face for second unique character."
  :group 'evil-quickscope)

;;; Settings
(defcustom evil-quickscope-bidirectional nil
  "Determines whether overlay only shows in direction of F/T (nil) or both directions (t)."
  :group 'evil-quickscope)

(defcustom evil-quickscope-cross-lines nil
  "Whether to cross lines for targets.
Use in conjunction with the evil-cross-lines variable."
  :group 'evil-quickscope)

(defcustom evil-quickscope-disable-in-comments nil
  "If enabled (t), disables quickscope-always-mode overlays when in a comment."
  :group 'evil-quickscope)

(defcustom evil-quickscope-accepted-chars
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  "String containing which characters are acceptable to highlight."
  :group 'evil-quickscope)

(defcustom evil-quickscope-word-separator " "
  "String which contains all word separating characters."
  :group 'evil-quickscope)

(defcustom evil-quickscope-search-max 1000
  "Specifies maximum number of characters to search. nil to disable."
  :group 'evil-quickscope)

(defcustom evil-quickscope-always-mode-delay 0.1
  "Seconds to wait before displaying overlays in always-mode.
Usually should be longer than the keyboard repeat rate to prevent excessive
updating when holding a key to scroll. Set to 0 to disable."
  :group 'evil-quickscope)

;;; Internal variables
(defvar evil-quickscope-always-mode-timer nil
  "Timer for delaying always-mode.")
(make-variable-buffer-local 'evil-quickscope-always-mode-timer)

(defvar evil-quickscope-mode-map
  (let ((map (make-sparse-keymap)))
    (evil-define-key 'motion map "f" 'evil-quickscope-find-char)
    (evil-define-key 'motion map "F" 'evil-quickscope-find-char-backward)
    (evil-define-key 'motion map "t" 'evil-quickscope-find-char-to)
    (evil-define-key 'motion map "T" 'evil-quickscope-find-char-to-backward)
    map)
  "Keymap for `evil-quickscope-mode'.")

;;; Utility functions
(defun evil-quickscope-create-char-plist (chars)
  "Creates initialized plist with accepted characters."
  (let ((plist ()))
    (mapc (lambda (c) (setq plist (plist-put plist c 0))) chars)
    plist))

(defun evil-quickscope-increment-plist-char (char-plist char)
  "Add count to corresponding char in plist."
  (plist-put char-plist char
             (1+ (plist-get char-plist char))))

(defun evil-quickscope-is-separator-p (char)
  "Determine if character is a separator."
  (let ((is-separator-list
         (mapcar (lambda (c) (eq char c)) evil-quickscope-word-separator)))
    (if (member t is-separator-list)
        t nil)))

;;; Character Finding Functions
(defun evil-quickscope-get-highlighted-chars (start end)
  "Gets highlighted chars and returns a list of first chars and second chars."
  (defun update-hl-chars (pos)
    "Checks if char at pos is separator/invalid, if not update seen-chars list."
    (let ((char (char-after pos)))
      (if (is-separator-or-invalid-char-p char)
          (add-to-hl-chars)
        (update-seen-chars))))

  (defun is-separator-or-invalid-char-p (char)
    "Determine if char is a separator or invalid."
    (or (evil-quickscope-is-separator-p char)
        (not (plist-get seen-chars char))))

  (defun add-to-hl-chars ()
    "Adds current hl-char pair to hl-chars list."
    (when (not first-word)
      (setq hl-chars (cons word-hl-chars hl-chars)))
    (setq word-hl-chars (list 0 0))
    (setq first-word nil))

  (defun update-seen-chars ()
    "Increments current char in seen-chars list and updates hl-char pair."
    (setq seen-chars (evil-quickscope-increment-plist-char seen-chars char))
    (let ((occurences (plist-get seen-chars char))
          (hl-p (car word-hl-chars))
          (hl-s (cadr word-hl-chars)))
      (cond
       ((and (= occurences 1) (= hl-p 0))
        (setcar word-hl-chars pos))
       ((and (= occurences 2) (= hl-s 0))
        (setcar (cdr word-hl-chars) pos)))))

  (let ((hl-chars ())
        (first-word t)
        (word-hl-chars '(0 0))
        (seen-chars (evil-quickscope-create-char-plist
                     evil-quickscope-accepted-chars))
        (direction (if (> end start) 1 -1))
        (pos start)
        (num-searches 0))
    (while (and (/= pos end)
                (or (eq evil-quickscope-search-max nil)
                    (< num-searches evil-quickscope-search-max)))
      (update-hl-chars pos)
      (setq pos (+ pos direction))
      (setq num-searches (1+ num-searches)))
    (add-to-hl-chars)
    hl-chars))

;;; Overlays
(defun evil-quickscope-apply-overlays-forward ()
  "Gets highlighted characters and apply overlays forward."
  (let* ((search-end (if evil-quickscope-cross-lines
                  (point-max) (line-end-position)))
         (hl-positions (evil-quickscope-get-highlighted-chars
                       (1+ (point)) search-end)))
    (evil-quickscope-apply-overlays hl-positions)))

(defun evil-quickscope-apply-overlays-backward ()
  "Gets highlighted characters and apply overlays backward."
  (let* ((search-end (if evil-quickscope-cross-lines
                        (point-min) (line-beginning-position)))
        (hl-positions (evil-quickscope-get-highlighted-chars
                       (1- (point)) search-end)))
    (evil-quickscope-apply-overlays hl-positions)))

(defun evil-quickscope-apply-overlays (hl-positions)
  "Applies quickscope overlays at specified positions."
  (dolist (hl-pair hl-positions)
    (cond
     ((> (car hl-pair) 0) ; First occurence of letter
      (evil-quickscope-set-overlay 'evil-quickscope-first-face (car hl-pair)))
     ((> (cadr hl-pair) 0) ; Second occurence of letter
      (evil-quickscope-set-overlay 'evil-quickscope-second-face (cadr hl-pair))))))

(defun evil-quickscope-set-overlay (face pos)
  "Sets face overlay at position."
  (overlay-put (make-overlay pos (1+ pos)) 'face face))

(defun evil-quickscope-remove-overlays ()
  "Remove all quickscope overlays from buffer."
    (dolist (face '(evil-quickscope-first-face
                    evil-quickscope-second-face))
      (remove-overlays nil nil 'face face)))

;;; Display updates
(defun evil-quickscope-update-overlays-bidirectional ()
  "Update overlays in both directions from point."
  (evil-quickscope-remove-overlays)
  (unless (and evil-quickscope-disable-in-comments
               (nth 4 (syntax-ppss)))
    (evil-quickscope-apply-overlays-forward)
    (evil-quickscope-apply-overlays-backward)))

(defun evil-quickscope-update-overlays-directional (is-forward)
  "Update overlay forward from point. If arg is nil, update backward."
  (evil-quickscope-remove-overlays)
  (if is-forward
      (evil-quickscope-apply-overlays-forward)
      (evil-quickscope-apply-overlays-backward)))

(defun evil-quickscope-update-overlays (is-forward)
  "Update overlays bidirectionally or directionally."
    (if evil-quickscope-bidirectional
        (evil-quickscope-update-overlays-bidirectional)
      (evil-quickscope-update-overlays-directional is-forward)))

(defun evil-quickscope-call-find (find-function)
  "Calls function and undo overlays if cancelled out."
  (unwind-protect
      (call-interactively find-function)
    (evil-quickscope-remove-overlays)))

(defun evil-quickscope-update-overlays-bidirectional-delayed ()
  "Update overlays bidirectionally with a delay."
  (when evil-quickscope-always-mode-timer
    (cancel-timer evil-quickscope-always-mode-timer))
  (setq evil-quickscope-always-mode-timer
        (run-at-time evil-quickscope-always-mode-delay nil
                     #'evil-quickscope-update-overlays-bidirectional)))

;;; Replacement evil-find-char* commands
;;;###autoload
(defun evil-quickscope-find-char ()
  "Move to the next COUNT'th occurence of CHAR.
Highlight first or second unique letter of each word."
  (interactive)
  (evil-quickscope-update-overlays t)
  (evil-quickscope-call-find 'evil-find-char))

;;;###autoload
(defun evil-quickscope-find-char-backward ()
  "Move to the previous COUNT'th occurence of CHAR.
Highlight first or second unique letter of each word."
  (interactive)
  (evil-quickscope-update-overlays nil)
  (evil-quickscope-call-find 'evil-find-char-backward))

;;;###autoload
(defun evil-quickscope-find-char-to ()
  "Move before the next COUNT'th occurence of CHAR.
Highlight first or second unique letter of each word."
  (interactive)
  (evil-quickscope-update-overlays t)
  (evil-quickscope-call-find 'evil-find-char-to))

;;;###autoload
(defun evil-quickscope-find-char-to-backward ()
  "Move before the previous COUNT'th occurence of CHAR.
Highlight first or second unique letter of each word."
  (interactive)
  (evil-quickscope-update-overlays nil)
  (evil-quickscope-call-find 'evil-find-char-to-backward))

;; Set evil properties of replacement commands
(evil-set-command-properties 'evil-quickscope-find-char
                             :type 'inclusive :jump t :keep-visual t :repeat 'motion)
(evil-set-command-properties 'evil-quickscope-find-char-backward
                             :type 'exclusive :jump t :keep-visual t :repeat 'motion)
(evil-set-command-properties 'evil-quickscope-find-char-to
                             :type 'inclusive :jump t :keep-visual t :repeat 'motion)
(evil-set-command-properties 'evil-quickscope-find-char-to-backward
                             :type 'exclusive :jump t :keep-visual t :repeat 'motion)

;;; Minor modes
;;;###autoload
(define-minor-mode evil-quickscope-always-mode
  "Quickscope mode for evil. Highlights per-word targets for f,F,t,T vim
movement commands. Target highglights always on."
  :init-value nil
  :lighter ""
  :keymap nil
  :global nil
  :group 'evil-quickscope

  (evil-quickscope-remove-overlays)
  (remove-hook 'post-command-hook 'evil-quickscope-update-overlays-bidirectional-delayed t)

  (when evil-quickscope-always-mode
    ;; Turn off quickscope-mode if on
    (when evil-quickscope-mode
      (evil-quickscope-mode 0))

    (add-hook 'post-command-hook 'evil-quickscope-update-overlays-bidirectional-delayed nil t)))

;;;###autoload
(define-globalized-minor-mode global-evil-quickscope-always-mode
  evil-quickscope-always-mode turn-on-evil-quickscope-always-mode
  "Global minor mode for evil-quickscope-always-mode.")

;;;###autoload
(defun turn-on-evil-quickscope-always-mode ()
  "Enable `evil-quickscope-mode'."
  (interactive)
  (evil-quickscope-always-mode 1))

;;;###autoload
(defun turn-off-evil-quickscope-always-mode ()
  (interactive)
  "Disable `evil-quickscope-mode'."
  (evil-quickscope-always-mode 0))

;;;###autoload
(define-minor-mode evil-quickscope-mode
  "Quickscope mode for evil. Highlights per-word targets for f,F,t,T vim
movement commands. Target highlights activate when f,F,t,T pressed."
  :init-value nil
  :lighter ""
  :keymap evil-quickscope-mode-map
  :global nil
  :group 'evil-quickscope

  (evil-quickscope-remove-overlays)
  (evil-normalize-keymaps)

  (when evil-quickscope-mode
    ;; Turn off quickscope-always-mode if on
    (when evil-quickscope-always-mode
      (evil-quickscope-always-mode 0))))

;;;###autoload
(define-globalized-minor-mode global-evil-quickscope-mode
  evil-quickscope-mode turn-on-evil-quickscope-mode
  "Global minor mode for evil-quickscope-mode.")

;;;###autoload
(defun turn-on-evil-quickscope-mode ()
  "Enable `evil-quickscope-mode'."
  (interactive)
  (evil-quickscope-mode 1))

;;;###autoload
(defun turn-off-evil-quickscope-mode ()
  "Disable `evil-quickscope-mode'."
  (interactive)
  (evil-quickscope-mode 0))

(provide 'evil-quickscope)

;;; evil-quickscope.el ends here
