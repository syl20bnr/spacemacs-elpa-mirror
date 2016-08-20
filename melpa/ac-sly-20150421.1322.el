;;; ac-sly.el --- An auto-complete source using sly completions
;;
;; Author: Damian T. Dobroczy\'nski <qoocku@gmail.com>
;; URL: https://github.com/qoocku/ac-sly
;; Package-Version: 20150421.1322
;; Version: DEV
;; Package-Requires: ((sly "1.0.0-alpha") (auto-complete "1.4") (cl-lib "0.5"))
;;
;; Commentary:
;; This is direct translation of ac-slime module replacing
;; "slime" with "sly". That's it.
;; Original Author - Steve Purcell <steve@sanityinc.com>
;; URL - https://github.com/purcell/ac-slime
;; Version - DEV
;;
;; Usage:
;;     (require 'ac-sly)
;;     (add-hook 'sly-mode-hook 'set-up-sly-ac)
;;     (add-hook 'sly-repl-mode-hook 'set-up-sly-ac)
;;     (eval-after-load "auto-complete"
;;       '(add-to-list 'ac-modes 'sly-repl-mode))
;;; Code:

(require 'cl-lib)
(require 'sly)
(require 'auto-complete)

(defgroup ac-sly nil
  "Sly auto-complete customizations"
  :prefix "ac-sly-"
  :group 'sly)

(defcustom ac-sly-show-flags t
  "When non-nil, show completion result flags during fuzzy completion."
  :group 'ac-sly)

(defun ac-source-sly-fuzzy-candidates ()
  "Return a possibly-empty list of fuzzy completions for the symbol at point."
  (when (sly-connected-p)
    (let ((sly-fuzzy-completion-limit 50))
      (mapcar (lambda (result)
                (let ((sym (car result))
                      (flags (car (last result))))
                  (if ac-sly-show-flags
                      (propertize sym 'summary flags)
                    sym)))
              (car (sly-fuzzy-completions (substring-no-properties ac-prefix)))))))

(defun ac-source-sly-simple-candidates ()
  "Return a possibly-empty list of completions for the symbol at point."
  (when (sly-connected-p)
    (car (sly-simple-completions (substring-no-properties ac-prefix)))))

(defun ac-source-sly-case-correcting-completions (name collection)
  (mapcar #'(lambda (completion)
              ;; FIXME
              (cl-replace completion name))
          (all-completions (downcase name) collection)))

(defvar ac-sly-current-doc nil "Holds slime docstring for current symbol.")
(defun ac-sly-documentation (symbol-name)
  "Return a documentation string for SYMBOL-NAME."
  (let ((symbol-name (substring-no-properties symbol-name)))
    (sly-eval `(swank:documentation-symbol ,symbol-name))))

(defun ac-sly-init ()
  "Called when completion source is initialized."
  (setq ac-sly-current-doc nil))

;;;###autoload
(defface ac-sly-menu-face
  '((t (:inherit ac-candidate-face)))
  "Face for slime candidate menu."
  :group 'auto-complete)

;;;###autoload
(defface ac-sly-selection-face
  '((t (:inherit ac-selection-face)))
  "Face for the slime selected candidate."
  :group 'auto-complete)

;;;###autoload
(defvar ac-source-sly-fuzzy
  '((init . ac-sly-init)
    (candidates . ac-source-sly-fuzzy-candidates)
    (candidate-face . ac-sly-menu-face)
    (selection-face . ac-sly-selection-face)
    (prefix . sly-symbol-start-pos)
    (symbol . "l")
    (match . (lambda (prefix candidates) candidates))
    (document . ac-sly-documentation))
  "Source for fuzzy slime completion.")

;;;###autoload
(defvar ac-source-sly-simple
  '((init . ac-sly-init)
    (candidates . ac-source-sly-simple-candidates)
    (candidate-face . ac-sly-menu-face)
    (selection-face . ac-sly-selection-face)
    (prefix . sly-symbol-start-pos)
    (symbol . "l")
    (document . ac-sly-documentation)
    (match . ac-source-sly-case-correcting-completions))
  "Source for slime completion.")


;;;###autoload
(defun set-up-sly-ac (&optional fuzzy)
  "Add an optionally-fuzzy slime completion source to `ac-sources'."
  (interactive)
  (add-to-list 'ac-sources
               (if fuzzy
                   'ac-source-sly-fuzzy
                 'ac-source-sly-simple)))


(provide 'ac-sly)

;;; ac-sly.el ends here
