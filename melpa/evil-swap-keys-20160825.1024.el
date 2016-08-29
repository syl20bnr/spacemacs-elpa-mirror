;;; evil-swap-keys.el --- intelligently swap keys on text input with evil -*- lexical-binding: t; -*-

;; Author: Wouter Bolsterlee <wouter@bolsterl.ee>
;; Version: 1.0.0
;; Package-Version: 20160825.1024
;; Package-Requires: ((emacs "24") (evil "1.2.12"))
;; Keywords: evil key swap numbers symbols
;; URL: https://github.com/wbolster/evil-swap-keys
;;
;; This file is not part of GNU Emacs.

;;; License:

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; Minor mode to intelligently swap keys when entering text.
;; See the README for more details.

;;; Code:

(require 'evil)

(defgroup evil-swap-keys nil
  "Intelligently swap keys when entering text"
  :prefix "evil-swap-keys-"
  :group 'evil)

(defcustom evil-swap-keys-number-row-keys
  '(("1" . "!")
    ("2" . "@")
    ("3" . "#")
    ("4" . "$")
    ("5" . "%")
    ("6" . "^")
    ("7" . "&")
    ("8" . "*")
    ("9" . "(")
    ("0" . ")"))
  "The numbers and symbols on the keyboard's number row.

This should match the actual keyboard layout."
  :group 'evil-swap-keys
  :type '(alist
          :key-type string
          :value-type string))

(defcustom evil-swap-keys-text-input-states
  '(emacs
    insert
    replace)
  "Evil states in which key presses will be treated as text input."
  :group 'evil-swap-keys
  :type '(repeat symbol))

(defcustom evil-swap-keys-text-input-commands
  '(evil-find-char
    evil-find-char-backward
    evil-find-char-to
    evil-find-char-to-backward
    evil-replace
    evil-snipe-f
    evil-snipe-F
    evil-snipe-s
    evil-snipe-S
    evil-snipe-t
    evil-snipe-T
    evil-snipe-x
    evil-snipe-X)
  "Commands that read keys which should be treated as text input."
  :group 'evil-swap-keys
  :type '(repeat function))

(defvar evil-swap-keys--mappings nil
  "Active mappings for this buffer.")
(make-variable-buffer-local 'evil-swap-keys--mappings)

(defun evil-swap-keys--text-input-p ()
  "Determine whether the current input should treated as text input."
  ;; NOTE: The evil-this-type check is a hack that seems to work well
  ;; for motions in operator mode. This variable is non-nil while
  ;; reading motions themselves, but not while entering a (optional)
  ;; count prefix for those motions. This makes things like d2t@
  ;; (delete until the second @ sign) work without using the shift key
  ;; at all: the first 2 is a count and will not be translated, and
  ;; the second 2 will be translated into a @ since the 't' motion
  ;; reads text input.
  (or
   (and (eq evil-state 'operator) evil-this-type)
   isearch-mode
   (minibufferp)
   (memq evil-state evil-swap-keys-text-input-states)
   (memq this-command evil-swap-keys-text-input-commands)))

(defun evil-swap-keys--maybe-translate (&optional prompt)
  "Maybe translate the current input.

The PROMPT argument is ignored; it's only there for compatibility with
the 'key-translation-map callback signature."
  ;; This callback uses the local configuration to decide whether the
  ;; key should be translated, and if so, determine the replacement.
  ;; A nil return value implies no key translation takes place.
  (let* ((key (string last-input-event))
         (buffer (if (minibufferp)
                     (window-buffer (minibuffer-selected-window))
                   (current-buffer)))
         (mappings (buffer-local-value 'evil-swap-keys--mappings buffer))
         (should-translate (and (buffer-local-value 'evil-swap-keys-mode buffer)
                                (buffer-local-value 'evil-local-mode buffer)
                                (evil-swap-keys--text-input-p)))
         (replacement (cdr (assoc key mappings))))
    (when should-translate replacement)))

(defun evil-swap-keys--add-bindings ()
  "Add bindings to the global 'key-translation-map'."
  ;; Note: key-translation-map is global. Enabling key swapping in a
  ;; buffer only adds bindings to this global map. Other buffers (with
  ;; possibly different configurations) may have added these bindings
  ;; already, which is not a problem because define-key is idempotent.
  (dolist (mapping evil-swap-keys--mappings)
    (define-key key-translation-map
      (car mapping)
      #'evil-swap-keys--maybe-translate)))

(defun evil-swap-keys--remove-bindings ()
  "Remove bindings from the global 'key-translation-map'."
  (dolist (key (where-is-internal #'evil-swap-keys--maybe-translate
                                  key-translation-map))
    (define-key key-translation-map key nil)))

;;;###autoload
(define-minor-mode evil-swap-keys-mode
  "Minor mode to intelligently swap keyboard keys during text input."
  :group 'evil-swap-keys
  :lighter " !1"
  (when evil-swap-keys-mode
      (evil-swap-keys--add-bindings)))

;;;###autoload
(define-globalized-minor-mode global-evil-swap-keys-mode
  evil-swap-keys-mode
  (lambda () (evil-swap-keys-mode t))
  "Global minor mode to intelligently swap keyboard keys during text input.")

;;;###autoload
(defun evil-swap-keys-add-mapping (from to)
  "Add a one-way mapping from key FROM to key TO."
  (add-to-list 'evil-swap-keys--mappings (cons from to))
  (evil-swap-keys-mode t))

;;;###autoload
(defun evil-swap-keys-add-pair (FROM TO)
  "Add a two-way mapping to swap keys FROM and TO."
  (evil-swap-keys-add-mapping FROM TO)
  (evil-swap-keys-add-mapping TO FROM))

;;;###autoload
(defun evil-swap-keys-swap-number-row ()
  "Swap the keys on the number row."
  (dolist (pair evil-swap-keys-number-row-keys)
    (evil-swap-keys-add-pair (car pair) (cdr pair))))

;;;###autoload
(defun evil-swap-keys-swap-underscore-dash ()
  "Swap the underscore and the dash."
  (evil-swap-keys-add-pair "_" "-"))

;;;###autoload
(defun evil-swap-keys-swap-colon-semicolon ()
  "Swap the colon and semicolon."
  (evil-swap-keys-add-pair ":" ";"))

;;;###autoload
(defun evil-swap-keys-swap-tilde-backtick ()
  "Swap the backtick and tilde."
  (evil-swap-keys-add-pair "~" "`"))

;;;###autoload
(defun evil-swap-keys-swap-double-single-quotes ()
  "Swap the double and single quotes."
  (evil-swap-keys-add-pair "\"" "'"))

;;;###autoload
(defun evil-swap-keys-swap-square-curly-brackets ()
  "Swap the square and curly brackets."
  (evil-swap-keys-add-pair "[" "{")
  (evil-swap-keys-add-pair "]" "}"))

;;;###autoload
(defun evil-swap-keys-swap-pipe-backslash ()
  "Swap the pipe and backslash."
  (evil-swap-keys-add-pair "|" "\\"))

;;;###autoload
(defun evil-swap-keys-swap-question-mark-slash ()
  "Swap the question mark and slash."
  (evil-swap-keys-add-pair "/" "?"))

(provide 'evil-swap-keys)
;;; evil-swap-keys.el ends here
