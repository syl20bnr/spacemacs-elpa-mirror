;;; auto-auto-indent.el --- Indents code as you type
;;; Version: 0.1
;; Package-Version: 20131106.1103
;;; Author: sabof
;;; URL: https://github.com/sabof/auto-auto-indent
;;; Package-Requires: ((es-lib "0.1") (cl-lib "1.0"))

;;; Commentary:

;; The project is hosted at https://github.com/sabof/auto-auto-indent
;; The latest version, and all the relevant information can be found there.

;;; License:

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'cl-lib)
(require 'es-lib)

(defvar aai-indent-function 'aai-indent-line-maybe
  "Indentation function to use call for automatic indentation.")
(defvar aai-indentable-line-p-function (es-constantly t)
  "For mode-specifc cusomizations.")
(defvar aai-after-change-indentation t
  "Whether to reindent after every change.
Useful when you want to keep the keymap and cursor repositioning.")
(defvar aai-indent-limit 30
  "Maximum number of lines for after-change indentation.")
(defvar aai-indented-yank-limit 4000
  "Maximum number of character to indent for `aai-indented-yank'")
(defvar aai-dont-indent-commands
  '(delete-horizontal-space
    quoted-insert
    backward-paragraph
    kill-region
    self-insert-command)
  "Commands after which not to indent.")
(defvar aai-mode-hook nil)
(defvar aai--timer nil)
(defvar aai-timer-delay 0.5
  "Indent after this ammout of second, following a sequence of self-insert commands.
Don't indent when nil")
(defvar aai-debug nil)
(es-define-buffer-local-vars
 aai--change-flag nil)

(defun aai-indent-line-maybe ()
  "\(indent-according-to-mode\) when `aai-indentable-line-p-function' returns non-nil.
All indentation happends through this function."
  (when (and aai-mode
             (not (memq indent-line-function
                        '(insert-tab indent-relative)))
             (funcall aai-indentable-line-p-function))
    (ignore-errors
      (indent-according-to-mode))))

(defun aai-indent-forward ()
  "Indent current line, and \(1- `aai-indent-limit'\) lines afterwards."
  (save-excursion
    (cl-loop repeat aai-indent-limit do
             (aai-indent-line-maybe)
             (forward-line))))

(cl-defun aai--indent-region (start end)
  "Indent region lines where `aai-indentable-line-p-function' returns non-nil."
  (save-excursion
    (let (( end-line (line-number-at-pos end)))
      (goto-char start)
      (while (<= (line-number-at-pos) end-line)
        (aai-indent-line-maybe)
        (when (cl-plusp (forward-line))
          (cl-return-from aai--indent-region))))))

(defun aai-indent-defun ()
  "Indent current defun, if it is smaller than `aai-indent-limit'.
Otherwise call `aai-indent-forward'."
  (let (init-pos
        end-pos
        line-end-distance)
    (condition-case nil
        (save-excursion
          (end-of-defun)
          (beginning-of-defun)
          (setq init-pos (point))
          (end-of-defun)
          (when (> (1+ (- (line-number-at-pos)
                          (line-number-at-pos init-pos)))
                   aai-indent-limit)
            (error "defun too long"))
          (setq end-pos (point))
          (aai--indent-region init-pos end-pos))
      (error (aai-indent-forward)))))

(cl-defun aai-indented-yank (&optional dont-indent)
  (interactive)
  (es-silence-messages
    (when (use-region-p)
      (delete-region (point) (mark))
      (deactivate-mark))
    (let (( starting-point (point))
          end-distance
          line)
      (yank)
      (setq end-distance (- (line-end-position) (point))
            line (line-number-at-pos))
      (unless (or dont-indent
                  (> (- (point) starting-point)
                     aai-indented-yank-limit))
        (aai--indent-region starting-point (point)))
      ;; ;; Necessary for web-mode. Possibly others
      ;; (when (and (bound-and-true-p font-lock-mode)
      ;;            (memq major-mode '(web-mode)))
      ;;   (font-lock-fontify-region starting-point (point)))
      (es-goto-line-prog line)
      (goto-char (max (es-indentation-end-pos)
                      (- (line-end-position) end-distance)))
      (when (derived-mode-p 'comint-mode)
        (let (( point (point)))
          (skip-chars-backward " \t\n" starting-point)
          (delete-region (point) point)))
      (set-marker (mark-marker) starting-point (current-buffer)))))

(defun aai-mouse-yank (event &optional dont-indent)
  (interactive "e")
  (if (use-region-p)
      (let (( reg-beg (region-beginning))
            ( reg-end (region-end)))
        (mouse-set-point event)
        (when (and (<= reg-beg (point))
                   (<= (point) reg-end))
          (delete-region reg-beg reg-end)
          (goto-char reg-beg)))
      (progn
        (mouse-set-point event)
        (deactivate-mark)))
  (aai-indented-yank dont-indent))

(defun aai-mouse-yank-dont-indent (event)
  (interactive "e")
  (aai-mouse-yank event t))

(defun aai-delete-char (&optional from-backspace)
  "Like `delete-char', but deletes indentation, if point is at it, or before it."
  (interactive)
  (if (use-region-p)
      (delete-region (point) (mark))
      (if (>= (point) (es-visible-end-of-line))
          (progn
            (delete-region (point) (1+ (line-end-position)))
            (when (and (es-fixup-whitespace)
                       (not from-backspace))
              (backward-char)))
          (delete-char 1))
      ;; (aai-indent-line-maybe)
      ))

(defun aai-backspace ()
  "Like `backward-delete-char', but removes the resulting gap when point is at EOL."
  (interactive)
  (cond ( (use-region-p)
          (delete-region (point) (mark)))
        ( (es-point-between-pairs-p)
          (delete-char 1)
          (delete-char -1))
        ( (<= (current-column)
              (current-indentation))
          (forward-line -1)
          (goto-char (line-end-position))
          (aai-delete-char t))
        ( (bound-and-true-p paredit-mode)
          (paredit-backward-delete))
        ( t (backward-delete-char 1))))

(defun aai-open-line ()
  "Open line, and indent the following."
  (interactive)
  (save-excursion
    (newline))
  (save-excursion
    (forward-char)
    (aai-indent-line-maybe))
  (aai-indent-line-maybe))

(cl-defun aai-newline-and-indent ()
  ;; This function won't run when cua--region-map is active
  (interactive)
  ;; For c-like languages
  (when (and (not (use-region-p))
             (member (char-before) '( ?{ ?\( ?\[ ))
             (member (char-after) '( ?} ?\) ?\] )))
    (newline)
    (save-excursion
      (newline))
    (aai-indent-line-maybe)
    (save-excursion
      (forward-char)
      (aai-indent-line-maybe))
    (cl-return-from aai-newline-and-indent))
  (when (use-region-p)
    (delete-region (point) (mark))
    (deactivate-mark))
  (newline)
  (aai-indent-line-maybe)
  (when (memq major-mode '(nxml-mode web-mode))
    (save-excursion
      (forward-line -1)
      (aai-indent-line-maybe))))

(defun aai-correct-position-this ()
  "Go back to indentation if point is before indentation."
  (let (( indentation-beginning (es-indentation-end-pos)))
    (when (< (point) indentation-beginning)
      (goto-char indentation-beginning))))

(defun aai-before-change-function (&rest ignore)
  "Change tracking."
  (when aai-mode
    (setq aai--change-flag t)))

(defun aai-on-timer (marker)
  (when (buffer-modified-p)
    (with-no-warnings
      (save-excursion
        (set-buffer (marker-buffer marker))
        (goto-char (marker-position marker))
        (funcall aai-indent-function))))
  (setq aai--timer))

(cl-defun aai-post-command-hook ()
  "Correct the cursor, and possibly indent."
  (condition-case error
      (progn
       (when (or (not aai-mode) (bound-and-true-p cua--rectangle))
         (cl-return-from aai-post-command-hook))
       (let* (( last-input-structural
                (member last-input-event
                        (mapcar 'string-to-char
                                (list "(" ")" "[" "]" "{" "}" "," ";" " "))))
              ( first-keystroke
                (and (eq this-command 'self-insert-command)
                     (or last-input-structural
                         (not (eq last-command 'self-insert-command)))))
              ( dont-indent-commands
                (append '(save-buffer
                          undo
                          undo-tree-undo
                          undo-tree-redo)
                        aai-dont-indent-commands)))
         ;; Correct position
         (when (or (not (region-active-p))
                   deactivate-mark
                   ;; (= (region-beginning)
                   ;;    (region-end))
                   )
           (when (and (es-neither (bound-and-true-p cua--rectangle)
                                  (bound-and-true-p multiple-cursors-mode))
                      (> (es-indentation-end-pos) (point)))
             (cond ( (memq this-command '(backward-char left-char))
                     (forward-line -1)
                     (goto-char (line-end-position)))
                   ( (memq this-command
                           '(forward-char
                             right-char
                             previous-line
                             next-line))
                     (back-to-indentation))))
           ;; It won't indent if corrected
           (cond ( (and aai-after-change-indentation
                        aai--change-flag
                        (buffer-modified-p) ; Don't indent in unmodified buffers
                        (or first-keystroke
                            (not (memq this-command dont-indent-commands))))
                   (funcall aai-indent-function)
                   (aai-correct-position-this))
                 ( (and aai-after-change-indentation
                        aai--change-flag
                        (not (memq this-command
                                   (remove
                                    'self-insert-command
                                    dont-indent-commands))))
                   (when aai--timer
                     (cancel-timer aai--timer))
                   (when aai-timer-delay
                     (setq aai--timer
                           (run-with-idle-timer
                            aai-timer-delay nil
                            `(lambda () (aai-on-timer ,(point-marker))))))
                   )))
         (setq aai--change-flag)))
    (error (when aai-debug
             (debug nil error)))))

(defun aai--major-mode-setup ()
  "Optimizations for speicfic modes"
  (when (memq major-mode
              '(lisp-interaction-mode
                common-lisp-mode
                emacs-lisp-mode))
    (set (make-local-variable 'aai-indent-function)
         'aai-indent-defun)))

(defun aai--minor-mode-setup ()
  "Change interacting minor modes."
  (eval-after-load 'multiple-cursors-core
    '(pushnew 'aai-mode mc/unsupported-minor-modes))
  (eval-after-load 'paredit
    '(es-define-keys auto-auto-indent-mode-map
       [remap paredit-forward-delete] 'aai-delete-char
       [remap paredit-backward-delete] 'aai-backspace))
  (eval-after-load 'cua-base
    '(define-key cua--region-keymap [remap delete-char]
       (lambda ()
         (interactive)
         (if aai-mode
             (aai-delete-char)
             (cua-delete-region)))))
  (eval-after-load 'eldoc
    '(eldoc-add-command 'aai-indented-yank)))

(defun aai--init ()
  (run-hooks 'aai-mode-hook)
  (add-hook 'post-command-hook 'aai-post-command-hook t t)
  (pushnew 'aai-before-change-function before-change-functions)
  (when (eq (key-binding (kbd "C-v")) 'cua-paste)
    (es-define-keys auto-auto-indent-mode-map
      (kbd "C-v") 'aai-indented-yank))
  (es-define-keys auto-auto-indent-mode-map
    [mouse-2] 'aai-mouse-yank
    [remap yank] 'aai-indented-yank
    [remap cua-paste] 'aai-indented-yank
    [remap newline] 'aai-newline-and-indent
    [remap open-line] 'aai-open-line
    [remap delete-char] 'aai-delete-char
    [remap forward-delete] 'aai-delete-char
    [remap backward-delete-char-untabify] 'aai-backspace
    [remap autopair-backspace] 'aai-backspace
    [remap backward-delete-char] 'aai-backspace
    [remap delete-backward-char] 'aai-backspace)
  (aai--minor-mode-setup)
  (aai--major-mode-setup))

;;;###autoload
(define-minor-mode auto-auto-indent-mode
    "Automatic automatic indentation.
Works pretty well for lisp out of the box.
Other modes might need some tweaking to set up:
If you trust the mode's automatic indentation completely, you can add to it's
init hook:

\(set \(make-local-variable 'aai-indent-function\)
     'aai-indent-defun\)

or

\(set \(make-local-variable 'aai-indent-function\)
     'aai-indent-forward\)

depending on whether the language has small and clearly
identifiable functions, that `beginning-of-defun' and
`end-of-defun' can find.

If on the other hand you don't trust the mode at all, but like
the cursor correction and delete-char behaviour,

you can add

\(set \(make-local-variable
      'aai-after-change-indentation\) nil\)

if the mode indents well in all but a few cases, you can change the
`aai-indentable-line-p-function'. This is what I have in my php mode setup:

\(set \(make-local-variable
      'aai-indentable-line-p-function\)
     \(lambda \(\)
       \(not \(or \(es-line-matches-p \"EOD\"\)
                \(es-line-matches-p \"EOT\"\)\)\)\)\)"
  nil " aai" (make-sparse-keymap)
  (if aai-mode
      (aai--init)))

(defalias 'aai-mode 'auto-auto-indent-mode)
(defvaralias 'aai-mode 'auto-auto-indent-mode)

(provide 'auto-auto-indent)
;;; auto-auto-indent.el ends here
