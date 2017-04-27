;;; jonprl-mode.el --- A major mode for editing JonPRL files  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  David Raymond Christiansen

;; Author: David Raymond Christiansen <david@davidchristiansen.dk>
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))
;; Version: 0.0.1
;; Package-Version: 20150627.1430
;; Keywords: languages

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

;; This is a major mode for editing JonPRL files. Right now, it's just
;; simple syntax highlighting.

;;; Code:
(require 'compile)
(require 'cl-lib)

(defgroup jonprl nil "Customization options for JonPRL"
  :prefix 'jonprl- :group 'languages)

(defcustom jonprl-path "jonprl" "The path to the jonprl executable."
  :type 'file
  :group 'jonprl)

(defcustom jonprl-mode-hook () "The hook to run when initializing JonPRL mode."
  :type 'hook
  :group 'jonprl)

(defcustom jonprl-pre-check-buffer-hook '(save-buffer)
  "A hook to run prior to checking the buffer."
  :type 'hook
  :group 'jonprl
  :options '(save-buffer))

(defface jonprl-keyword-face '((t (:inherit font-lock-keyword-face)))
  "The face used to highlight JonPRL keywords."
  :group 'jonprl)

(defface jonprl-tactic-face '((t (:inherit font-lock-function-name-face)))
  "The face used to highlight JonPRL tactics."
  :group 'jonprl)

(defface jonprl-name-face '((t (:inherit font-lock-variable-name-face)))
  "The face used to highlight JonPRL names."
  :group 'jonprl)

(defface jonprl-comment-face '((t (:inherit font-lock-comment-face)))
  "The face used to highlight JonPRL comments."
  :group 'jonprl)

(defconst jonprl-keywords '("Theorem" "Tactic" "Operator" "=def=")
  "Keywords for `jonprl-mode'.")

(defconst jonprl-tactics
  '("cum" "auto" "intro" "elim" "mem-cd" "eq-cd"
    "witness" "hypothesis" "subst" "hyp-subst" "lemma"
    "unfold" "refine" "assumption" "symmetry" "trace"
    "ext")
  "A list of the tactics to be highlighted in JonPRL mode.")

(defvar jonprl-mode-path nil
  "Directory containing the `jonprl-mode' package.
This is used to load resource files such as images.  The default
value is automatically computed from the location of the Emacs
Lisp package.")
(setq jonprl-mode-path (file-name-directory load-file-name))


(defun jonprl-font-lock-defaults ()
  "Calculate the font-lock defaults for `jonprl-mode'."
  `('((,(regexp-opt jonprl-keywords 'words) 0 'jonprl-keyword-face)
      (,(regexp-opt jonprl-tactics 'words) 0 'jonprl-tactic-face)
      ("<\\(\\w+\\(\\s-+\\w+\\)*\\)>" 1 'jonprl-name-face)
      ("^\\s-*\\(|||.*\\)$" 1 'jonprl-comment-face))))

(defun jonprl--compilation-buffer-name-function (_mode)
  "Compute a buffer name for the jonprl-mode compilation buffer."
  "*JonPRL*")

(defun jonprl-check-buffer ()
  "Load the current file into JonPRL."
  (interactive)
  (run-hooks 'jonprl-pre-check-buffer-hook)
  (let* ((filename (buffer-file-name))
         (dir (file-name-directory filename))
         (file (file-name-nondirectory filename))
         (command (concat jonprl-path " --check " file))

         ;; Emacs compile config stuff - these are special vars
         (compilation-buffer-name-function
          'jonprl--compilation-buffer-name-function)
         (default-directory dir))
    (compile command)))

(defvar jonprl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") 'jonprl-check-buffer)
    (define-key map [tool-bar sep] '(menu-item "--"))
    (define-key-after map [tool-bar check-buffer]
      `(menu-item "Check" jonprl-check-buffer
                  :enable t
                  :visible t
                  :help "Check in JonPRL"
                  :image ,(create-image (concat jonprl-mode-path "jonprl-icon.png")))
      t)
    map))

(defvar jonprl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?= "w" table)
    (modify-syntax-entry ?' "w" table)
    table))

(defun jonprl-complete-at-point ()
  "Attempt to complete at point for JonPRL keywords and tactics."
  (when (looking-back "\\w+" nil t)
    (let* ((match (match-string-no-properties 0))
           (start (match-beginning 0))
           (end (match-end 0))
           (candidates (cl-remove-if-not (apply-partially #'string-prefix-p match)
                                         (append jonprl-keywords
                                                 jonprl-tactics))))
      (if (null candidates) () (list start end candidates)))))

(defconst jonprl-parse-error-regexp
  "\\(Fail: Parse error at \\)\\([^:]+\\):\\([0-9]+\\)\\.\\([0-9]+\\)-\\([0-9]+\\)\\.\\([0-9]+\\):"
  "Regexp matching JonPRL parse errors.")

(defconst jonprl-tactic-fail-regexp
  "\\[\\(?1:\\(?2:[^:]+\\):\\(?3:[0-9]+\\)\\.\\(?4:[0-9]+\\)-\\(?5:[0-9]+\\)\\.\\(?6:[0-9]+\\)\\)\\]: tactic '\\(?7:.+\\)' failed with goal:"
  "Regexp matching JonPRL tactic failures.")

(easy-menu-define jonprl-mode-menu jonprl-mode-map
  "Menu for JonPRL major mode"
  `("JonPRL"
    ["Check" jonprl-check-buffer t]))

;;;###autoload
(push '("\\.jonprl\\'" . jonprl-mode) auto-mode-alist)

(provide 'jonprl-mode)
;;; jonprl-mode.el ends here
