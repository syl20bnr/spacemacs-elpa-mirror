This program is derived from comint-mode and provides the following features.

 * file name completion in string
 * incremental history search


Put this file in your Emacs lisp path (e.g. ~/.emacs.d/site-lisp)
and add the following line to your .emacs:

   (require 'php-boris)

Type M-x php-boris to run boris php REPL.
See also `comint-mode' to check key bindings.


(require 'cc-mode)
(require 'comint)
(require 'ansi-color)

(defgroup php-boris nil
  "Run boris REPL and communicate the process."
  :group 'processes)

(defconst php-boris-version "0.0.1"
  "php-boris mode Version.")

(defcustom php-boris-command "boris"
  "boris command used in `php-boris-mode'."
  :group 'php-boris
  :type 'string)

(defcustom php-boris-prompt "\\[\\d+\\] boris> "
  "boris prompt used in `php-boris-mode'."
  :group 'php-boris
  :type 'string)

(defvar php-boris-process-name "boris-repl"
  "process name of boris REPL.")

(defvar php-boris-temp-buffer-name "*php-boris-command-output*")

(defvar php-boris-mode-syntax-table
  (let ((st (make-syntax-table)))
    (c-populate-syntax-table st)
    (modify-syntax-entry ?$ "_" st)
    st))

(defvar php-boris-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'comint-dynamic-complete)
    (define-key map (kbd "C-c C-c") 'php-boris-quit-or-cancel)
    map))

process.stdout.columns should be set.
but process.stdout.columns in Emacs is infinity because Emacs returns 0 as winsize.ws_col.
The completion candidates won't be displayed if process.stdout.columns is infinity.
see also `handleGroup` function in readline.js
(defvar php-boris-code "")


(defvar php-boris-input-ignoredups t
  "If non-nil, don't add input matching the last on the input ring.

See also `comint-input-ignoredups'")

(defvar php-boris-process-echoes t
  "If non-nil, boris does not echo any input.

See also `comint-process-echoes'")

(defvar php-boris-extra-espace-sequence-re "\\(\x1b\\[[0-9]+[GJK]\\)")
(defvar php-boris-ansi-color-sequence-re "\\(\x1b\\[[0-9]+m\\)")
if send string like "a; Ma\t", return a; Math\x1b[1G> a; Math\x1b[0K\x1b[10G
(defvar php-boris-prompt-re-format
  (concat
   "\x1b\\[1G"
   "\\("
   "\x1b\\[0J%s.*\x1b\\[[0-9]+G"  ; for Node.js 0.8
   "\\|"
   "%s.*\x1b\\[0K\x1b\\[[0-9]+G"  ; for Node.js 0.4 or 0.6
   "\\)"
   "$"))
(defvar php-boris-prompt-re
  (format php-boris-prompt-re-format php-boris-prompt php-boris-prompt))
not support Unicode characters
(defvar php-boris-require-re
  (concat
   "\\(?:^\\|\\s-\\|[-+*/%&|><!;{}()[]\\|\\]\\)"  ; delimiter
   "require\\s-*(\\s-*"
   "\\("
   "\"[^\"\\]*\\(?:\\\\.[^\"\\]*\\)*"             ; double quote
   "\\|"
   "'[^'\\]*\\(?:\\\\.[^'\\]*\\)*"                ; single quote
   "\\)"
   "$"))

(defvar php-boris-cache-token "")
(defvar php-boris-cache-candidates ())


--------------------------
Private functions
--------------------------
(defun php-boris--in-string-p (&optional pos)
  "Return non-nil if point is inside string"
  (nth 3 (syntax-ppss pos)))

(defun php-boris--extract-require-argument (string)
  (if (string-match php-boris-require-re string)
      (match-string 1 string)))

(defun php-boris--get-last-token (string)
  "Return the last token in the string."
  (if (string-match "\\([._$]\\|\\w\\)+$" string)
      (match-string 0 string)))
