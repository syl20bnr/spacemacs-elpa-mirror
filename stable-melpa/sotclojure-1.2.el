;;; sotclojure.el --- Write clojure at the speed of thought.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; Keywords: convenience, clojure
;; Package-Version: 1.2
;; Package-Requires: ((emacs "24.1") (clojure-mode "4.0.0") (cider "0.8") (sotlisp "1.3"))
;; Version: 1.2
;; URL: https://github.com/Malabarba/speed-of-thought-clojure

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
;;
;;   This defines a new local minor-mode `sotclojure-mode', which is
;;   activated by the global `speed-of-thought-mode' on any clojure
;;   buffers.
;;
;;   The mode is quite simple, and is composed of two parts:
;;
;; 1.1 Abbrevs
;; ───────────
;;
;;   A large number of abbrevs which expand function initials to their
;;   name. A few examples:
;;
;;   • wl -> when-let [|]
;;   • n -> not
;;   • wo -> with-open
;;   • np -> number? (the p stands for predicate)
;;   • ck -> :keys [|] (the c stands for colon)
;;
;;   Note that, in order to avoid frustration, the 1-letter abbrevs will
;;   only expand after a `(' or after a `/', so you can still use 1-letter
;;   local variables like `a' and `n'.
;;
;; 1.2 Commands
;; ────────────
;;
;;   It also defines 4 commands, which really fit into this "follow the
;;   thought-flow" way of writing. The bindings are as follows:
;;   `M-RET': Break line, and insert `()' with point in the middle.
;;   `C-RET': Do `forward-up-list', then do M-RET.
;;
;;   Hitting RET followed by a `(' was one of the most common key sequences
;;   for me while writing elisp, so giving it a quick-to-hit key was a
;;   significant improvement.
;;
;;   `C-c f': Find function under point. If it is not defined, create a
;;   definition for it below the current function and leave point inside.
;;
;;   With these commands, you just write your code as you think of it. Once
;;   you hit a “stop-point” of sorts in your tought flow, you hit `C-c f/v'
;;   on any undefined functions/variables, write their definitions, and hit
;;   `C-u C-SPC' to go back to the main function.
;;
;; 1.3 Small Example
;; ─────────────────
;;
;;   With the above (assuming you use something like paredit or
;;   electric-pair-mode), if you write:
;;
;;   ┌────
;;   │ (wl SPC {ck SPC x C-f C-RET (a SPC (np SPC y C-f SPC f SPC y
;;   └────
;;
;;   You get
;;
;;   ┌────
;;   │ (when-let [{:keys [x]}
;;   │            (and (number? y) (first y))])
;;   └────
;;
;;; Code:

(require 'sotlisp)
(require 'clojure-mode)
(require 'cider)

(defun sotclojure--function-p ()
  "Non-nil if point is at reasonable place for a function name.
In clojure, that is basically anywhere.  Still, for the sake of
usability, return nil if word at point has a single letter and is
not after a `('."
  (save-excursion
    (ignore-errors
      (and (not (string-match (rx (syntax symbol)) (string last-command-event)))
           (sotlisp--code-p)
           (not (let ((orig (point)))
                  (save-excursion
                    (beginning-of-defun)
                    (nth 3 (parse-partial-sexp (point) orig)))))
           (string-match (rx alpha) (string (char-before)))
           (or (and (< (skip-chars-backward (rx word)) -1)
                    (= ?\s (char-before)))
               (and (string-match (rx (not (syntax symbol)))
                                  (string (char-before)))
                    (not (sotlisp--whitespace-char-p (char-before)))))))))

(defvar sotclojure--function-table (make-hash-table :test #'equal)
  "Table where function abbrev expansions are stored.")


;;; Abbrev definitions
(defconst sotclojure--default-function-abbrevs
  '(
    ("a"   . "and ")
    ("am"  . "alter-meta! ")
    ("amb" . "alter-meta! ")
    ("ai"  . "assoc-in ")
    ("as"  . "assoc ")
    ("ap"  . "associative? ")
    ("b"   . "binding [$]")
    ("bl"  . "butlast ")
    ("c"   . "count ")
    ("ca"  . ":as ")
    ("ck"  . ":keys [$] ")
    ("co"  . ":or {$}")
    ("con" . ":only [$]")
    ("cp"  . "coll? ")
    ("cr"  . ":refer [$]")
    ("cs"  . ":strs [$]")
    ("cy"  . ":syms [$]")
    ("d"  . "def ")
    ("di"  . "dissoc ")
    ("df"  . "defn $ []\n  ")
    ("dm"  . "defmacro $\n  \"\"\n  []\n  ")
    ("dv"  . "def $ nil\n  \"\"")
    ("ds"  . "doseq [it $]")
    ("dt"  . "deftest ")
    ("ep"  . "empty? ")
    ("f"   . "first ")
    ("fi"  . "filter ")
    ("fp"  . "fn? ")
    ("fn"  . "fn [$]")
    ("g"   . "get ")
    ("gb"  . "group-by ")
    ("gi"  . "get-in ")
    ("i"   . "into ")
    ("ip"  . "instance? ")
    ("idp" . "identical? ")
    ("il"  . "if-let [$]")
    ("kp"  . "keyword? ")
    ("l"   . "let [$]")
    ("let" . "let [$]")
    ("loop" . "loop [$]")
    ("lp"  . "list? ")
    ("m"   . "map ")
    ("mc"  . "mapcat ")
    ("mp"  . "map? ")
    ("n"   . "not ")
    ("np"  . "number? ")
    ("pl"  . "println ")
    ("pn"  . "println ")
    ("r"   . "reduce ")
    ("rb"  . "reset! ")
    ("re"  . "remove ")
    ("rf"  . "re-find #\"$\"")
    ("rs"  . "re-seq #\"$\"")
    ("s"   . "str ")
    ("sb"  . "swap! ")
    ("sk"  . "select-keys ")
    ("sp"  . "seq? ")
    ("stp" . "string? ")
    ("syp" . "symbol? ")
    ("t"   . "throw (Exception. \"$\")")
    ("tt"  . "testing \"$\"")
    ("u"  . "update ")
    ("ui"  . "update-in $ []")
    ("vm"  . "vary-meta ")
    ("vp"  . "vector? ")
    ("w"   . "when ")
    ("wn"  . "when-not ")
    ("wl"  . "when-let [$]")
    ("wb"  . "with-bindings ")
    ("wm"  . "with-meta ")
    ("wo"  . "with-open [$]")
    ("wr"  . "with-redefs [$]")
    ("wis" . "with-in-str ")
    ("wos" . "with-out-str ")
    )
  "Alist of (ABBREV . EXPANSION) used by `sotclojure'.")

(defun sotclojure-define-function-abbrev (name expansion)
  "Define a function abbrev expanding NAME to EXPANSION.
This abbrev will only be expanded in places where a function name is
sensible.  Roughly, this is right after a `(' or a `#''.

If EXPANSION is any string, it doesn't have to be the just the
name of a function.  In particular:
  - if it contains a `$', this char will not be inserted and
    point will be moved to its position after expansion."
  (define-abbrev clojure-mode-abbrev-table
    name t #'sotclojure--expand-function
    ;; Don't override user abbrevs
    :system t
    ;; Only expand in function places.
    :enable-function #'sotclojure--function-p)
  (puthash name expansion sotclojure--function-table))

(defun sotclojure--expand-function ()
  "Expand the function abbrev before point.
See `sotclojure-define-function-abbrev'."
  (let ((r (point)))
    (skip-chars-backward (rx alnum))
    (let* ((name (buffer-substring (point) r))
           (expansion (gethash name sotclojure--function-table nil)))
      (if (not expansion)
          (progn (goto-char r) nil)
        (delete-region (point) r)
        (insert expansion)
        (when (string-match "\\$" expansion)
          (setq sotlisp--needs-moving t))
        ;; Must be last.
        (sotlisp--post-expansion-cleanup)))))

(put 'sotclojure--expand-function 'no-self-insert t)

(defun sotclojure-erase-all-abbrevs ()
  "Undefine all abbrevs defined by `sotclojure'."
  (interactive)
  (maphash (lambda (x _) (define-abbrev clojure-mode-abbrev-table x nil))
           sotclojure--function-table))

(defun sotclojure-define-all-abbrevs ()
  "Define all abbrevs in `sotclojure--default-function-abbrevs'."
  (interactive)
  (mapc (lambda (x) (sotclojure-define-function-abbrev (car x) (cdr x)))
        sotclojure--default-function-abbrevs))

(defun sotclojure-find-or-define-function (&optional prefix)
  "If symbol under point is a defined function, go to it, otherwise define it.
Essentially `find-function' on steroids.

If you write in your code the name of a function you haven't
defined yet, just place point on its name and hit \\[sotclojure-find-or-define-function]
and a defun will be inserted with point inside it.  After that,
you can just hit `pop-mark' to go back to where you were.
With a PREFIX argument, creates a `defmacro' instead.

If the function under point is already defined this just calls
`find-function', with one exception:
    if there's a defun (or equivalent) for this function in the
    current buffer, we go to that even if it's not where the
    global definition comes from (this is useful if you're
    writing an Emacs package that also happens to be installed
    through package.el).

With a prefix argument, defines a `defmacro' instead of a `defun'."
  (interactive "P")
  (let ((name (cider-symbol-at-point)))
    (unless (and name (sotlisp--find-in-buffer "(def[^ ]* " name))
      (let ((dict (cider-var-info name)))
        (if (and dict (> (length dict) 10))
            (cider--find-var name)
          (sotlisp--beginning-of-defun)
          (insert "(def" (if prefix "macro" "n"))
          (save-excursion
            (insert " " name "\n  \"\"\n  [])\n\n")))))))


;;; Mode definition
;;;###autoload
(define-minor-mode sotclojure-mode
  nil nil " SoT"
  `(([M-return] . sotlisp-newline-and-parentheses)
    ([C-return] . sotlisp-downlist-newline-and-parentheses)
    (,(kbd "C-M-;") . sotlisp-comment-or-uncomment-sexp)
    ("\C-cf"    . sotclojure-find-or-define-function))
  (if sotclojure-mode
      (abbrev-mode 1)
    (kill-local-variable 'abbrev-mode)))

;;;###autoload
(defun sotclojure-turn-on-everywhere ()
  "Call-once function to turn on sotclojure everywhere.
Calls `sotclojure-mode' on all `clojure-mode' buffers, and sets
up a hook and abbrevs."
  (add-hook 'clojure-mode-hook #'sotclojure-mode)
  (sotclojure-define-all-abbrevs)
  (mapc (lambda (b)
          (with-current-buffer b
            (when (derived-mode-p 'clojure-mode)
              (sotclojure-mode 1))))
        (buffer-list)))

(defun sotclojure-turn-off-everywhere ()
  "Call-once function to turn off sotclojure everywhere.
Removes `sotclojure-mode' from all `clojure-mode' buffers, and
removes hooks and abbrevs."
  (remove-hook 'clojure-mode-hook #'sotclojure-mode)
  (sotclojure-erase-all-abbrevs)
  (mapc (lambda (b)
          (with-current-buffer b
            (when (derived-mode-p 'clojure-mode)
              (sotclojure-mode -1))))
        (buffer-list)))

;;;###autoload
(eval-after-load 'sotlisp
  '(speed-of-thought-hook-in #'sotclojure-turn-on-everywhere #'sotclojure-turn-off-everywhere))

(provide 'sotclojure)
;;; sotclojure.el ends here

