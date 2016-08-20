;;; hfst-mode.el --- major mode for editing HFST files

;; Copyright (C) 2010-2016 Kevin Brubeck Unhammer
;; Copyright (C) 2006 Sebastian Nagel (sfst.el used as basis)

;; Author: Kevin Brubeck Unhammer <unhammer@fsfe.org>
;; Version: 0.3.0
;; Package-Version: 20160708.502
;; Package-Requires:
;; Url: http://wiki.apertium.org/wiki/Emacs
;; Keywords: languages

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides syntax highlighting and a "go to lexicon"-function useful
;; for editing Helsinki Finite State Transducer descriptions.

;; Usage:
;; (add-to-list 'load-path "/path/to/hfst-mode-folder")
;; (autoload 'hfst-mode "~/path/to/hfst-mode/hfst.el")
;; ; Change these lines if you name your files something other
;; ; than .twol and .lexc:
;; (add-to-list 'auto-mode-alist '("\\.twol$" . hfst-mode))
;; (add-to-list 'auto-mode-alist '("\\.lexc$" . hfst-mode))

;; For information about Helsinki Finite State Transducer Tools
;; (HFST), see http://hfst.github.io/"

;;; TODO:
;;; - Recognize if we are in a lexc or twolc (or xfst) file,
;;;   and apply specific keywords etc.
;;; - Recognize if we are in < xfst-in-lexc >
;;; - Recognize END keyword in lexc files and comment all that is
;;;   below it
;;; - Compile list of lexicon names for quick go-to?
;;; - A feature like dix.el's dix-xmlise-using-above-elt
;;; - Use define-derived-mode instead of this old-school stuff

;;; Code:

(defconst hfst-mode-version "0.4.0" "Version of hfst-mode.")

;;;============================================================================
;;;
;;; Define the formal stuff for a major mode named hfst.
;;;

(defvar hfst-mode-hook nil
  "Hook run on turning on hfst-mode.")

(defvar hfst-mode-multichars-cache nil
  "Cached value of lexc multichar symbols.")

(defgroup hfst-mode nil
  "Major mode for editing HFST soure files."
  :tag "HFST"
  :group 'languages)

;; TODO: should also have a list of parent lexc's for
;; hfst-mode-goto-lexicon to use when looking up
(defcustom hfst-mode-root-lexc nil
  "If your lexc is split into multiple files, you may want to refer to the root.
This is typically a file-local variable, used by
`hfst-mode-lexc-guess-multichars'."
  :type '(choice string (const nil))
  :group 'hfst-mode
  :safe #'hfst-mode-safe-root-lexc)

(defcustom hfst-mode-idle-delay 2
  "How often to run `hfst-mode-idle-timer'.
The function just creates a regexp from the Multichar_Symbols
section of `hfst-mode-root-lexc', which typically shouldn't take
very long."
  :safe #'integerp
  :group 'hfst-mode
  :type 'integer)

(defun hfst-mode-safe-root-lexc (val)
  "Return t if VAL is safe as a file-local variable."
  (or (stringp val) (null val)))

(defvar hfst-mode-map (make-sparse-keymap)
  "Keymap for hfst minor mode.")

(defvar hfst-mode-syntax-table
  (let ((hfst-mode-syntax-table (make-syntax-table)))
    ; comments start with !
    (modify-syntax-entry ?!  "<" hfst-mode-syntax-table)
    ; and last until end-of-line
    (modify-syntax-entry ?\n ">!" hfst-mode-syntax-table)
    ;; todo: % is the escape character (I think)
    (modify-syntax-entry ?% "\\" hfst-mode-syntax-table)
    ;; dots appear as symbols in flag diacritics:
    (modify-syntax-entry ?. "_" hfst-mode-syntax-table)
    (modify-syntax-entry ?« "_" hfst-mode-syntax-table)
    (modify-syntax-entry ?» "_" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?\" "." hfst-mode-syntax-table)
;;     (modify-syntax-entry ?\\ "\\" hfst-mode-syntax-table)
;;    (modify-syntax-entry ?\" "." hfst-mode-syntax-table)
;;     (modify-syntax-entry ?\' "$" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?< "\"" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?< "." hfst-mode-syntax-table)
;;     (modify-syntax-entry ?> "." hfst-mode-syntax-table)
;;     (modify-syntax-entry ?{ "(}" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?} "){" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?( "()" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?) ")(" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?[ "(]" hfst-mode-syntax-table)
;;     (modify-syntax-entry ?] ")[" hfst-mode-syntax-table)
    hfst-mode-syntax-table)
  "Syntax table for hfst-mode.")

(defface hfst-mode-font-lock-escaped-face
  '((((class color) (min-colors 88) (background light)) (:background "Pink" :weight bold))
    (((class color) (min-colors 88) (background dark)) (:background "Red1" :weight bold))
    (((class color) (min-colors 16) (background light)) (:background "Pink" :weight bold))
    (((class color) (min-colors 16) (background dark)) (:background "Red1" :weight bold))
    (((class color) (min-colors 8)) (:background "red"))
    (t (:inverse-video t :weight bold)))
  "Font Lock mode face used to escaped characters (using background colour since we may have spaces)."
  :group 'font-lock-faces)

(defconst hfst-mode-keywords
  '("Alphabet" "Multichar_Symbols" "Sets" "Rules" "Definitions"
    "Diacritics" "Rule-variables" "where" "in" "matched" "END"
    ;; pmatch:
    "set" "need-separators" "off" "on"
    "Define" "regex" "EndTag" "Punct" "Whitespace" "LC" "RC" "@bin")
  "Set of keywords to give keyword-face in font-lock.")

(defconst hfst-mode-operators ; TODO: check if xfst/lexc/twol only highlight the right ones
  '("<=>" "<=" "=>" "/<=" "_" ";"
    "=" ":" ">"
    "\\" "~" "+" "?" "*" "-" "^")
  "Set of operators to give function-name-face in font-lock.")

(defconst hfst-mode-font-lock-keywords
  `((hfst-mode-next-lexc-multichar-symbol
     (0 'font-lock-variable-name-face nil 'lax))
    ;; keywords TODO: alphabet doesn't match if on first line!
    (,(concat "\\(?:\\Sw\\|^\\)" (regexp-opt hfst-mode-keywords 'group) "\\Sw")
     (1 'font-lock-keyword-face nil 'lax))
    ;; todo: lexicon names always start with a capital letter, but can
    ;; you have eg. Æ? or just A-Z?
    ("\\(LEXICON\\) +\\(\\(\\sw\\|\\s_\\)+\\)" ; Root is special, note it in any way?
     (1 'font-lock-keyword-face t 'lax)
     (2 'font-lock-constant-face t 'lax))
    ;; flag diacritics (these get warning because they should be in multichars):
    ("@\\sw\\.\\(\\(\\sw\\|\\s_\\)+\\)@"
     (1 'font-lock-warning-face nil 'lax))
    ;; End symbol:
    ("\\(^\\|[^%]\\)\\(#\\)"
     (2 'font-lock-warning-face nil 'lax))
    ;; escape symbol:
    ("%." 0 'hfst-mode-font-lock-escaped-face nil 'lax)
    ;; operators:
    (,(regexp-opt hfst-mode-operators)
     (0 'font-lock-function-name-face 'keep 'lax)))
  "Expressions to highlight in hfst-mode.")

(defun hfst-mode-font ()
  "Set font-lock variables for hfst mode."
  (make-local-variable 'font-lock-keywords-case-fold-search) ; For GNU Emacs.
  (setq font-lock-keywords-case-fold-search nil)
  (put major-mode 'font-lock-keywords-case-fold-search nil) ; For XEmacs.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(hfst-mode-font-lock-keywords nil nil)))

;;;###autoload
(defun hfst-mode ()
  "Major mode for editing Helsinki Finite State Transducer files.
Supported formats include .lexc and .twolc.
For more information on Helsinki Finite State Transducer Tools, see
http://hfst.github.io/

HFST-mode provides the following specific keyboard key bindings:

\\{hfst-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'hfst-mode
	mode-name "HFST")
  (use-local-map hfst-mode-map)
  (setq parse-sexp-ignore-comments t)
  (set-syntax-table hfst-mode-syntax-table)
  ;;   (make-local-variable 'indent-line-function)
  ;;   (setq indent-line-function 'hfst-mode-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "! ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "! *")
  (make-local-variable 'completion-ignore-case)
  (setq completion-ignore-case nil)
  (hfst-mode-font)
  (hfst-mode-ensure-idle-timer)
  (run-mode-hooks 'hfst-mode-hook))

;;; Interactive functions -----------------------------------------------------

(defun hfst-mode-lexref-at-point ()
  "The lexicon referenced to by this entry."
  (let ((start (save-excursion
		 (re-search-backward "[^%];\\|LEXICON \\S *")
		 (match-end 0)))
	(end (save-excursion (re-search-forward "[^%];"))))
    (save-excursion
      (goto-char start)
      (re-search-forward "\\s \\(\\S +\\)\\s *;")
      (match-string-no-properties 1))))

(defun hfst-mode-goto-lexicon ()
  "Go to the lexicon defined at point/line.
Call from an entry to go to its pardef.  Mark is pushed so you can
go back with \\[universal-argument] \\[set-mark-command]."
  (interactive)
  (let ((lexname (hfst-mode-lexref-at-point))
	pos)
    (if (save-excursion
	  (goto-char (point-min))
	  (if (re-search-forward (concat "^\\s *LEXICON " lexname "\\s *\\($\\|!\\)")
                                 nil 'noerror)
	      (setq pos (match-beginning 0))))
	(progn (push-mark)
	       (goto-char pos))
      (message "Couldn't find LEXICON %s" lexname))))

(defun hfst-mode-next-lexc-multichar-symbol (&optional bound)
  "Go to next multichar symbol.
BOUND is as in `re-search-forward'."
  ;; TODO: if X3 is in Multichar_Symbols, LEXICON ENDLEX3 matches here :/
  (interactive)
  (let ((syms (hfst-mode-lexc-multichars)))
    (re-search-forward syms bound 'noerror)))

(defun hfst-mode-next-xfst-in-lexc-multichar-symbol (&optional bound)
  "Go to next xfst-in-lexc multichar symbol.
BOUND is as in `re-search-forward'."
  ;; This function TODO! Needs to recognize that we are inside a "string" inside <brackets>
  ;; maybe use match-anchored?
  (interactive)
  (let ((syms (hfst-mode-lexc-multichars)))
    (re-search-forward syms bound 'noerror)))

(defun hfst-mode-lexc-multichars (&optional update)
  "Return the multichar symbols of this lexc file.
Should just return the cached value, but if UPDATE, force an
update of the cache."
  (when update
    ;; TODO: set the local cache only in the root-lexc, so we can
    ;; re-use in all children?
    (let ((new (regexp-opt (hfst-mode-lexc-guess-multichars))))
      (when (not (equal new hfst-mode-multichars-cache))
        (make-local-variable 'hfst-mode-multichars-cache)
        (setq hfst-mode-multichars-cache new)
        (jit-lock-refontify))))
  hfst-mode-multichars-cache)

(defmacro hfst-mode-with-root-lexc (&rest body)
  "Run BODY within the buffer of `hfst-mode-root-lexc'.
If it's unset, just use the current buffer."
  (declare (indent 0) (debug t))
  `(if hfst-mode-root-lexc
       (with-current-buffer (find-file-noselect hfst-mode-root-lexc)
         (let (hfst-mode-root-lexc
               (hfst-mode-disable-timers t))
           ,@body))
     ,@body))

(defun hfst-mode-lexc-guess-multichars ()
  "Return a list of the multichar symbols of this lexc file."
  (hfst-mode-with-root-lexc
   (save-excursion
     (goto-char (point-min))
     (let* ((beg (and (re-search-forward "^\\s *Multichar_Symbols\\s *" nil 'noerror)
                      (match-end 0)))
            (end (and beg
                      (re-search-forward "^\\s *LEXICON\\s ")
                      (match-beginning 0)))
            (raw (and end
                      (string-to-list (buffer-substring-no-properties beg end))))
            inquot
            incomt
            sym
            syms)
       ;; Could just split-string raw, but whitespace can actually be quoted!
       (dolist (c raw)
         (cond (incomt (when (eq c ?\n) (setq incomt nil)))
               (inquot (setq inquot nil) (push c sym))
               ((eq c ?!) (setq incomt t))
               ((eq c ?%) (setq inquot t) (push c sym))
               ;; split-string-default-separators
               ((memq c '(32 12 9 10 13 11)) (when sym
                                               (push (concat (reverse sym)) syms)
                                               (setq sym nil)))
               (t (push c sym))))
       syms))))

(defvar hfst-mode-idle-timer nil
  "Timer which keeps `hfst-mode-multichars-cache' up-to-date.
See `hfst-mode-ensure-idle-timer'.")

(defun hfst-mode-ensure-idle-timer ()
  "Start `hfst-mode-idle-timer' if not running already."
  (unless hfst-mode-idle-timer
    ;; run once first, in an idle-timer since we have to let
    ;; file-local variables get set:
    (run-with-idle-timer 1 nil #'hfst-mode-idle-timer)
    (make-local-variable 'hfst-mode-idle-timer)
    (setq hfst-mode-idle-timer
          (run-with-idle-timer hfst-mode-idle-delay 'repeat #'hfst-mode-idle-timer))))

(defvar hfst-mode-disable-timers nil
  "Set to t to disable all timers.")

(defun hfst-mode-idle-timer (&optional buffer)
  "Cache multichar symbols.
Use current buffer or BUFFER."
  (with-current-buffer
      (or buffer (current-buffer))
    (when (and (eq major-mode 'hfst-mode)
               (not hfst-mode-disable-timers))
      (hfst-mode-lexc-multichars 'update))))

;;; Keybindings --------------------------------------------------------------
(define-key hfst-mode-map (kbd "M-.") #'hfst-mode-goto-lexicon)
(define-key hfst-mode-map (kbd "M-,") #'pop-to-mark-command)

;;; Run hooks -----------------------------------------------------------------
(run-hooks 'hfst-mode-load-hook)

(provide 'hfst-mode)

;;;============================================================================

;;; hfst-mode.el ends here
