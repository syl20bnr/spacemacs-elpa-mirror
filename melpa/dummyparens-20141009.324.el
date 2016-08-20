;;; dummyparens.el --- parenthesis auto-pairing and wrapping

;; Copyright (C) 2014 Sergei Nosov

;; Author: Sergei Nosov <sergei.nosov [at] gmail.com>
;; Version: 1.1
;; Package-Version: 20141009.324
;; Keywords: dummyparens auto-pair wrapping
;; URL: https://github.com/snosov1/dummyparens

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Dummyparens is a simple utility providing parentheses auto-pairing and
;; wrapping.

;; When you press an opening parenthesis key it inserts the closing one
;; for you. If a region is selected when you press the key, the region
;; will be wrapped. If you wrap with { it will also indent the wrapped
;; region (convenient for C and the like).

;; This is similar to electric-pair minor mode, but with wrapping.

;; It's intended to be minimalistic and non-invasive as you would expect
;; such simple functionality to be.

;; For more sophisticated features, like HTML tags or LaTeX environments
;; handling, consider https://github.com/Fuco1/smartparens

;;; Code:

(defvar dp-wrap nil
  "This variable contains 2 points at which paired symbols must
  be put to provide wrapping. If no wrapping is needed it's equal
  to nil.")

(defun dp-brace-post-handler ()
  "Indents after insertion"
  (when dp-wrap
    (indent-region (car dp-wrap) (cdr dp-wrap)))
  (indent-for-tab-command))

(defcustom dp-pairs '(
                      ("(" ")" nil)
                      ("[" "]" nil)
                      ("{" "}" dp-brace-post-handler)
                      ("\"" "\"" nil)
                      )
  "Parenthesis to be paired"
  :group 'dummyparens)

(defcustom dp-ignore-modes-list '(
                                  )
  "Modes where dummyparens mode is inactive if allowed globally."
  :type '(repeat symbol)
  :group 'dummyparens)

(defun dp-self-insert-command (arg)
  "This function should be binded to opening pair"
  (interactive "p")
  (if (not (region-active-p))
      ;; we're not wrapping
      (setq dp-wrap nil)
    ;; save point and mark position
    (setq dp-wrap (cons (region-beginning)
                        (1+ (region-end)))) ;; 1+ since we call self-insert next
    (goto-char (car dp-wrap)))

  ;; which key was pressed
  (let ((key (single-key-description last-command-event)))
    ;; call the command that would've been called without dummyparens
    (let ((dummyparens-mode nil))
      (funcall (key-binding key t) arg))
    (save-excursion
      ;; for each pair
      (dolist (pair dp-pairs)
        ;; test if pressed key corresponds to an opening pair
        (when (equal key (car pair))
          ;; goto region end when wrapping
          (when dp-wrap
            (goto-char (cdr dp-wrap)))
          (let ((closing-pair (nth 1 pair))
                (post-handler (nth 2 pair)))
            ;; call the command for the closing pair
            (let* ((dummyparens-mode nil)
                   (command (key-binding closing-pair t))
                   (last-command-event (string-to-char closing-pair))
                   (this-command command))
              (call-interactively command))
            ;; run post-handler
            (when post-handler
              (funcall post-handler))))))))

(defvar dp-keymap (make-sparse-keymap)
  "Keymap used for `dummyparens-mode'.")
(dolist (pair dp-pairs)
  (define-key dp-keymap (car pair) 'dp-self-insert-command))

(define-minor-mode dummyparens-mode
  "Toggle dummyparens mode."
  :keymap dp-keymap)

(defun turn-on-dummyparens-mode ()
  "Turn on `dummyparens-mode'."
  (interactive)
  (unless (or (member major-mode dp-ignore-modes-list)
              (eq (get major-mode 'mode-class) 'special))
    (dummyparens-mode t)))

;;;###autoload
(define-globalized-minor-mode global-dummyparens-mode
  dummyparens-mode
  turn-on-dummyparens-mode)

(provide 'dummyparens)
;;; dummyparens.el ends here
