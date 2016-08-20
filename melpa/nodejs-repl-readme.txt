This program is derived from comint-mode and provides the following features.

 * TAB completion same as Node.js REPL
 * file name completion in string
 * incremental history search


Put this file in your Emacs lisp path (e.g. ~/.emacs.d/site-lisp)
and add the following line to your .emacs:

   (require 'nodejs-repl)

Type M-x nodejs-repl to run Node.js REPL.
See also `comint-mode' to check key bindings.


(require 'cc-mode)
(require 'comint)
(require 'ansi-color)

(defgroup nodejs-repl nil
  "Run Node.js REPL and communicate the process."
  :group 'processes)

(defconst nodejs-repl-version "0.1.0"
  "Node.js mode Version.")

(defcustom nodejs-repl-command "node"
  "Node.js command used in `nodejs-repl-mode'."
  :group 'nodejs-repl
  :type 'string)

(defcustom nodejs-repl-arguments '()
  "Command line parameters forwarded to `nodejs-repl-command'."
  :group 'nodejs-repl
  :type '(repeat string))

(defcustom nodejs-repl-prompt "> "
  "Node.js prompt used in `nodejs-repl-mode'."
  :group 'nodejs-repl
  :type 'string)


(defcustom nodejs-repl-input-ignoredups t
  "If non-nil, don't add input matching the last on the input ring.

See also `comint-input-ignoredups'"
  :group 'nodejs-repl
  :type 'boolean)

(defcustom nodejs-repl-process-echoes t
  "If non-nil, Node.js does not echo any input.

See also `comint-process-echoes'"
  :group 'nodejs-repl
  :type 'boolean)

(defvar nodejs-repl-mode-hook nil
  "Functions runafter `nodejs-repl' is started.")

(defvar nodejs-repl-process-name "nodejs"
  "process name of Node.js REPL.")

(defvar nodejs-repl-temp-buffer-name "*nodejs-repl-command-output*")

(defvar nodejs-repl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (c-populate-syntax-table st)
    (modify-syntax-entry ?$ "_" st)
    st))

(defvar nodejs-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'comint-dynamic-complete)
    (define-key map (kbd "C-c C-c") 'nodejs-repl-quit-or-cancel)
    map))

process.stdout.columns should be set.
Node.js 0.8 and 0.10 uses this value as the maximum number of columns,
but process.stdout.columns in Emacs is infinity because Emacs returns 0 as winsize.ws_col.
The completion candidates won't be displayed if process.stdout.columns is infinity.
see also `handleGroup` function in readline.js
(defvar nodejs-repl-code-format
  (concat
   "process.stdout.columns = %d;"
   "require('repl').start('%s', null, null, true, false, "
   "require('repl')['REPL_MODE_' + '%s'.toUpperCase()])"))

(defvar nodejs-repl-extra-espace-sequence-re "\\(\x1b\\[[0-9]+[GJK]\\)")

(defvar nodejs-repl-ansi-color-sequence-re "\\(\x1b\\[[0-9]+m\\)")

if send string like "a; Ma\t", return a; Math\x1b[1G> a; Math\x1b[0K\x1b[10G
(defvar nodejs-repl-prompt-re-format
  (concat
   "\x1b\\[1G"
   "\\("
   "\x1b\\[0J%s.*\x1b\\[[0-9]+G.*"  ; for Node.js 0.8
   "\\|"
   "%s.*\x1b\\[0K\x1b\\[[0-9]+G.*"  ; for Node.js 0.4 or 0.6
   "\\)"
   "$"))
(defvar nodejs-repl-prompt-re
  (format nodejs-repl-prompt-re-format nodejs-repl-prompt nodejs-repl-prompt))
not support Unicode characters
(defvar nodejs-repl-require-re
  (concat
   "\\(?:^\\|\\s-\\|[-+*/%&|><!;{}()[]\\|\\]\\)"  ; delimiter
   "require\\s-*(\\s-*"
   "\\("
   "\"[^\"\\]*\\(?:\\\\.[^\"\\]*\\)*"             ; double quote
   "\\|"
   "'[^'\\]*\\(?:\\\\.[^'\\]*\\)*"                ; single quote
   "\\)"
   "$"))

(defvar nodejs-repl-cache-token "")
(defvar nodejs-repl-cache-candidates ())


--------------------------
Private functions
--------------------------
(defun nodejs-repl--in-string-p (&optional pos)
  "Return non-nil if point is inside string"
  (nth 3 (syntax-ppss pos)))

(defun nodejs-repl--extract-require-argument (string)
  (if (string-match nodejs-repl-require-re string)
      (match-string 1 string)))

(defun nodejs-repl--get-last-token (string)
  "Return the last token in the string."
  (if (string-match "\\([._$]\\|\\w\\)+$" string)
      (match-string 0 string)))
