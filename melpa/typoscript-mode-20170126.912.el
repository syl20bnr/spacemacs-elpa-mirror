;;; typoscript-mode.el --- mode for TypoScript files

;; Copyright (C) 2009  Joachim Mathes
;; Copyright (C) 2016 Johannes Goslar
;; Author: Johannes Goslar
;; Original-Author: Joachim Mathes
;; Created: July 2009
;; Version: 0.2
;; Package-Version: 20170126.912
;; Package-Requires: ((emacs "24.4") (use-package "0"))
;; Keywords: typo3, typoscript
;; URL: https://github.com/ksjogo/typoscript-mode
;; EmacsWiki: TypoScriptMode

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Credits:

;; Initially taken from https://www.emacswiki.org/emacs/ts-mode.el
;; Thanks to Joachim Mathes

;;; Code:

(defgroup typoscript nil
  "Major mode for editing TypoScript files."
  :prefix "typoscript-"
  :group 'languages)

(defcustom typoscript-newline-function 'newline-and-indent
  "Function to be called upon pressing `RET'."
  :type '(choice (const newline)
                 (const newline-and-indent)
                 (const reindent-then-newline-and-indent))
  :group 'typoscript)

(defcustom typoscript-block-indentation 2
  "The indentation relative to a predecessing line which begins a new block.

  In TypoScript blocks start with the left parenthesis `(' or the left brace
  `{'."
  :type 'integer
  :group 'typoscript)

(defcustom typoscript-fold-foreground-color "white"
  "The foreground color used to highlight the folded block.

  The default value is `white'.  For a list of all available colors use `M-x
list-colors-display'"
  :type 'color
  :group 'typoscript)

(defcustom typoscript-fold-background-color "DodgerBlue1"
  "The background color used to highlight the folded block.

  The default value is `DodgerBlue1'.  For a list of all available colors use
`M-x list-colors-display'"
  :type 'color
  :group 'typoscript)

(defface typoscript-classes-face
  '((t :inherit font-lock-keyword-face))
  "Face for TypoScript classes.")

(defface typoscript-path-face
  '((t :inherit font-lock-builtin-face :foreground "DarkTurquoise"))
  "Face for TypoScript paths.")

(defface typoscript-block-face
  '((t :inherit font-lock-builtin-face :foreground "DodgerBlue1"))
  "Face for TypoScript blocks.")

(defface typoscript-conditional-face
  '((t :inherit font-lock-keyword-face))
  "Face for TypoScript conditionals.")

(defface typoscript-html-face
  '((t :inherit font-lock-string-face))
  "Face for TypoScript HTML tags.")

(defvar typoscript-classes-face 'typoscript-classes-face
  "Face for TypoScript classes.")

(defvar typoscript-path-face 'typoscript-path-face
  "Face for TypoScript paths.")

(defvar typoscript-block-face 'typoscript-block-face
  "Face for TypoScript blocks.")

(defvar typoscript-conditional-face 'typoscript-conditional-face
  "Face for TypoScript conditionals.")

(defvar typoscript-html-face 'typoscript-html-face
  "Face for TypoScript HTML tags.")

(defvar typoscript-font-lock-keywords
  (let ((kw1 (mapconcat 'identity
                        ;; Basic TypoScript classes
                        '("CONFIG"   "PAGE"  "TEXT"       "COA"  "COA_INT"
                          "FILE"     "IMAGE" "GIFBUILDER" "CASE" "TEMPLATE"
                          "HMENU"    "GMENU" "CONTENT")
                        "\\|")))
    (list
     ;; Paths
     '("^[ \t]*\\([[:alnum:]-_\\.]+\\)[ \t]*[=<>]" 1 'typoscript-path-face)
     ;; Blocks
     '("^[ \t]*\\([[:alnum:]-_\\.]+\\)[ \t]*[{(]" 1 'typoscript-block-face)
     ;; Periods
     ;;'("^[ \t]*" "\\(\\.\\)" nil nil (1 'default t))
     ;; Classes (keywords)
     (list (concat "\\<\\(" kw1 "\\)\\>") 1 'typoscript-classes-face t)
     ;; Conditional expressions `[...]'
     '("^[ \t]*\\(\\[.+?\\]\\)[ \t]*$" 1 'typoscript-conditional-face)
     ;; Comment lines beginning with hash symbol `#'
     '("^[ \t]*\\(#.*\\)$" 1 'font-lock-comment-face)
     ;; HTML special character encodings on the right side of the operator
     '("\\(=\\|=<\\|>\\|:=\\)" "\\(&[#[:alnum:]]+;\\)" nil nil (0 'typoscript-html-face))
     ;; HTML tags
     '("=<?\\|>\\|:=\\|[ \t]*" "\\(<[^<]+?>\\)" nil nil (0 'typoscript-html-face))
     ;; HTML color definitions
     '("#[[:xdigit:]]\\{6\\}[ \t\n]+" 0 'typoscript-html-face t)))
  "Expressions to highlight in TypoScript mode.")

(defvar typoscript-highlight-overlays [nil nil]
  "A vector of different overlay to do highlighting.
This vector concerns only highlighting of horizontal lines.")

(defvar typoscript-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'typoscript-newline)
    (define-key map "\C-c\C-e" 'typoscript-fold-block)
    (define-key map "\C-c\C-a" 'typoscript-unfold-block)
    (define-key map "\C-c\C-u\C-r" 'typoscript-unfold-region)
    (define-key map "\C-c\C-u\C-b" 'typoscript-unfold-buffer)
    (define-key map "}" 'typoscript-electric-brace)
    (define-key map ")" 'typoscript-electric-brace)
    map)
  "Key map used in TypoScript Mode buffers.")

(defvar typoscript-mode-syntax-table
  (let ((typoscript-mode-syntax-table (make-syntax-table)))
    ;; Parenthesis, brackets and braces
    (modify-syntax-entry ?\( "()" typoscript-mode-syntax-table)
    (modify-syntax-entry ?\) ")(" typoscript-mode-syntax-table)
    (modify-syntax-entry ?\[ "(]" typoscript-mode-syntax-table)
    (modify-syntax-entry ?\] ")[" typoscript-mode-syntax-table)
    (modify-syntax-entry ?\{ "(}" typoscript-mode-syntax-table)
    (modify-syntax-entry ?\} "){" typoscript-mode-syntax-table)
    ;; Comment delimiters
    (modify-syntax-entry ?/ ". 124b" typoscript-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" typoscript-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" typoscript-mode-syntax-table)
    (modify-syntax-entry ?\" "." typoscript-mode-syntax-table)
    (modify-syntax-entry ?. "." typoscript-mode-syntax-table)
    typoscript-mode-syntax-table)
  "Syntax table used in TypoScript Mode buffers.")

(defcustom typoscript-mode-hook nil
  "Hook run when entering TypoScript mode."
  :options '()
  :type 'hook
  :group 'typoscript)

;;;###autoload
(define-derived-mode typoscript-mode prog-mode "TypoScript"
  "Major mode for editing TypoScript files."
  :group 'typoscript
  (setq-local font-lock-defaults '(typoscript-font-lock-keywords))
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "# ")
  (setq-local indent-line-function 'typoscript-indent-line)
  (setq-local defun-prompt-regexp "^[ \t]*\\([[:alnum:]-_\\.]+\\)[ \t]*"))

;;;###autoload
(add-to-list 'auto-mode-alist '("setup.txt" . typoscript-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("constants.txt" . typoscript-mode))

(defun typoscript-newline ()
  "Call the dedicated newline function.

The variable `typoscript-newline-function' decides which newline function to
use."
  (interactive)
  (funcall typoscript-newline-function))

(defun typoscript-indent-line ()
  "Indent current line for TypoScript mode."
  (let ((cp (point))                ; current point
        (cc (current-column))       ; current column
        (ci (current-indentation))  ; current indentation
        (cl (line-number-at-pos))   ; current line
        (counter 0)
        ps                          ; parser state
        psp			    ; parser state position
        save-indent-column)

    ;; Evaluate parser state
    (save-excursion
      (beginning-of-line)
      (setq ps (typoscript-parser-state))

      (cond
       ;; Check if parser state position is:
       ;; -> Inside a comment
       ((nth 8 ps)
        (setq psp (nth 8 ps))
        (goto-char psp)
        (setq save-indent-column (+ (current-column)
                                    1)))
       ;; Check if parser state position is:
       ;; -> Inside a parenthetical grouping
       ((nth 1 ps)
        (setq psp (nth 1 ps))
        (cond
         ;; Check if point is looking at a string and a closing curly brace
         ((looking-at "[ \t[:alnum:]]*[)}]")
          (goto-char psp)
          (back-to-indentation)
          (setq save-indent-column (current-column)))
         (t
          (goto-char psp)
          (back-to-indentation)
          (setq save-indent-column (+ (current-column)
                                      typoscript-block-indentation)))))
       ;; Check if parser state position is:
       ;; -> nil
       (t
       	;; Skip empty lines
       	(forward-line -1)
       	(while (and (looking-at "^[ \t]*\n")
                    (not (bobp)))
       	  (forward-line -1))
       	(back-to-indentation)
        (setq save-indent-column (current-column)))))

    ;; Set indentation value on current line
    (back-to-indentation)
    (backward-delete-char-untabify (current-column))
    (indent-to save-indent-column)
    (if (> cc ci)
        (forward-char (- cc ci)))))

(defun typoscript-parser-state ()
  "Return the parser state at point."
  (save-excursion
    (let ((here (point))
          sps)
      ;; For correct indentation the character position of the start of the
      ;; innermost parenthetical grouping has to be found.
      (goto-char (point-min))
      ;; Now get the parser state, i.e. the depth in parentheses.
      (save-excursion
        (setq sps (parse-partial-sexp (point) here)))
      sps)))

(defun typoscript-block-start ()
  "Return buffer position of the last unclosed enclosing block.

If nesting level is zero, return nil."
  (let ((status (typoscript-parser-state)))
    (if (<= (car status) 0)
        nil
      (car (cdr status)))))

;; Electric characters

(defun typoscript-electric-brace (arg)
  "Insert closing brace.
Argument ARG prefix."
  (interactive "*P")
  ;; Insert closing brace.
  (self-insert-command (prefix-numeric-value arg))

  (when (and (looking-at "[ \t]*$")
             (looking-back "^[ \t]*[})]"))
    (typoscript-indent-line)))

;; Folding

(defun typoscript-fold-block ()
  "Hide the block on which point currently is located."
  (interactive)
  (let ((current-point (point))
        (block-start (typoscript-block-start)))

    (if (not block-start)
        (message "Point is not within a block.")

      ;; Look for block start
      (save-excursion
        (goto-char (typoscript-block-start))
        (beginning-of-line)
        (setq block-start (point)))

      (when block-start
        (let ((block-name
               ;; Save block name
               (save-excursion
                 (goto-char block-start)
                 (beginning-of-line)
                 (looking-at
                  "^[ \t]*\\(.*?\\)[ \t]*{")
                 (match-string 1)))
              (block-end
               ;; Look for block end
               (save-excursion
                 (goto-char block-start)
                 (forward-list)
                 (point)))
              ;; Variable for overlay
              skampi-overlay)

          ;; ------------------------------------------------------------------
          ;; The following local variables are defined up to here:
          ;; [1] block-start: point of block start, at the beginning
          ;;                  of the line; nil otherwise
          ;; [2] block-name : name of block, i.e. the object path
          ;; [3] block-end  : point of block end, at the end of the
          ;;                  line which contains the closing curly brace `}
          ;; ------------------------------------------------------------------

          ;; Check if end of measurement block is beyond point;
          ;; call fold function otherwise
          (if (>= block-end current-point)
              (typoscript-fold block-start block-end block-name)
            (message "Error: No valid block found."))

          ;; Indent overlay
          (goto-char block-start)
          (beginning-of-line)
          (typoscript-indent-line))))))

(defun typoscript-fold (block-start block-end block-name)
  "Fold block.

The block starts at BLOCK-START and ends at BLOCK-END.  Its
BLOCK-NAME is the TypoScript object path."
  (let (typoscript-overlay)
    ;; Check if block-start and block-end are valid values, i.e. not nil
    (if (or (eq block-start nil)
            (eq block-end nil))
        (message "Error: No valid block found.")
      ;; Make an overlay and hide block
      (setq typoscript-overlay (make-overlay block-start block-end
                                     (current-buffer) t nil))
      (overlay-put typoscript-overlay 'category 'typoscript-fold)
      (overlay-put typoscript-overlay 'evaporate t)
      (overlay-put typoscript-overlay 'mouse-face 'highlight)
      (overlay-put typoscript-overlay 'display (concat "["
                                               (propertize block-name
                                                           'face
                                                           nil)
                                               "]"))
      (overlay-put typoscript-overlay 'font-lock-face `(:foreground ,typoscript-fold-foreground-color
                                                            :background ,typoscript-fold-background-color))
      (overlay-put typoscript-overlay 'help-echo (concat
                                          "Folded block: "
                                          block-name)))))

(defun typoscript-unfold-buffer ()
  "Unfold all blocks in the buffer."
  (interactive)
  (typoscript-unfold-region (point-min) (point-max)))

(defun typoscript-unfold-region (start end)
  "Unfold all blocks in the region.

The region delimiters are START and END."
  (interactive "r")
  (let ((typoscript-overlays (overlays-in start end)))
    (typoscript-unfold-overlays typoscript-overlays)))

(defun typoscript-unfold-block ()
  "Unfold block at point."
  (interactive)
  (let ((typoscript-overlays (overlays-at (point))))
    (typoscript-unfold-overlays typoscript-overlays)))

(defun typoscript-unfold-overlays (typoscript-overlays)
  "Unfold all overlays set by typoscript-fold in TYPOSCRIPT-OVERLAYS.

Return non-nil if an unfold happened, nil otherwise."
  (let (found)
    (dolist (overlay typoscript-overlays)
      (when (eq (overlay-get overlay 'category) 'typoscript-fold)
        (delete-overlay overlay)
        (setq found t)))
    found))

(use-package rainbow-identifiers
  :defer t
  :config
  (add-to-list 'rainbow-identifiers-faces-to-override 'typoscript-block-face)
  (add-to-list 'rainbow-identifiers-faces-to-override 'typoscript-path-face))

(provide 'typoscript-mode)

;;; typoscript-mode.el ends here
