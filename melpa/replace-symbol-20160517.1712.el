;;; replace-symbol.el --- Rename symbols in expressions or buffers

;; Author: Brian Mastenbrook <brian@mastenbrook.net>
;; Version: 1.1
;; Package-Version: 20160517.1712
;; URL: https://github.com/bmastenbrook/replace-symbol-el

;;; Commentary:

;; This file implements a set of functions for replacing symbols
;; within a sexp or a buffer. A symbol for this purpose is anything
;; for which (progn (forward-sexp) (backward-sexp)) is idempotent and
;; for which (progn (down-list) (backward-up-list)) is *not*
;; idempotent. It is case sensitive at the moment, but that may change
;; in the future.

;; I have used this to rename variables, functions, types, etc. in
;; both Lisp and C. Your mileage may of course vary.

;; This file was written by Brian Mastenbrook (brian AT mastenbrook
;; DOT net) and is placed in the public domain.

;; M-x replace-symbol-in-sexp from to
;; M-x replace-symbol-in-buffer from to

;;; Code:

(defun replace-symbol-in-sexps-until-error (from to)
  "Replace FROM with TO in all the sexps inside of a list.
Returns when a scan-error is caught."
  (condition-case c
      (while t
        (replace-symbol-in-sexp from to t)
        (forward-sexp))
    (scan-error nil)))

(defun replace-symbol--beginning-of-list-p ()
  "Return true when the point is at the beginning of a list."
  (save-excursion
    (condition-case c
        (progn
          (forward-sexp)
          (backward-sexp)
          (let ((point-1 (point)))
           (down-list)
           (backward-up-list)
           (forward-sexp)
           (backward-sexp)
           (eq (point) point-1)))
      (scan-error nil))))

;; Dynamic variable used for recursive calls to replace-symbol-in-sexp
(defvar replace-symbol--replaced-in-sexp nil)

(defun replace-symbol--read-args ()
  "Query from/to replace strings, defaulting to symbol at point."
  (let* ((sap (symbol-at-point))
         (def (when (and sap (symbolp sap))
                (symbol-name sap)))
         (from (read-string (if def
                                (format "Replace symbol (default %s): " def)
                              "Replace symbol: ")
                            nil query-replace-from-history-variable def))
         (to (read-string (format "Replace symbol %s with: " from)
                          nil query-replace-to-history-variable nil)))
    (list from to)))

;;;###autoload
(defun replace-symbol-in-sexp (from to &optional recursive)
  "Replace the symbol FROM with TO in the sexp following the point.
If RECURSIVE is true, do not announce the number of replacements."
  (interactive (replace-symbol--read-args))
  (let ((do-replace
         (lambda ()
           (save-excursion
             (forward-sexp)
             (backward-sexp)
             ;; the combination of those two puts us on the first character
             ;; of the sexp
             (cond
              ((replace-symbol--beginning-of-list-p)
               ;; we're sitting on the beginning of a list
               (down-list)   ; travel into the sexp and replace inside
               (replace-symbol-in-sexps-until-error from to))
              (t (let ((beginning-of-sexp (point))
                       (end-of-sexp 0))
                   (forward-sexp)
                   (setq end-of-sexp (point))
                   (backward-sexp)
                   ;; we get these values for the beginning and end of the
                   ;; sexp so we can compare and delete it
                   (if (string-equal (buffer-substring beginning-of-sexp
                                                       end-of-sexp) from)
                       (progn
                         (delete-region beginning-of-sexp end-of-sexp)
                         (insert to)
                         (setq replace-symbol--replaced-in-sexp (1+ replace-symbol--replaced-in-sexp)))
                     nil))))))))
    (if recursive
        (funcall do-replace)
      (let ((replace-symbol--replaced-in-sexp 0))
        (funcall do-replace)
        (unless recursive
          (message "Replaced %s occurrence%s"
                   replace-symbol--replaced-in-sexp
                   (if (eq replace-symbol--replaced-in-sexp 1) "" "s")))))))

;;;###autoload
(defun replace-symbol-in-buffer (from to)
  "Replace the symbol FROM with TO in the entire buffer."
  (interactive (replace-symbol--read-args))
  (let ((replace-symbol--replaced-in-sexp 0))
    (save-excursion
      (goto-char (point-min))
      (condition-case c
          (while (not (eq (point-max) (point)))
            (replace-symbol-in-sexp from to t)
            (forward-sexp))
        (scan-error nil)))
    (message "Replaced %s occurrence%s"
             replace-symbol--replaced-in-sexp
             (if (eq replace-symbol--replaced-in-sexp 1) "" "s"))))

(provide 'replace-symbol)

;;; replace-symbol.el ends here
