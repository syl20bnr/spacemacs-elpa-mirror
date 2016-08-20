;;; hlint-refactor.el --- Apply HLint suggestions

;; Copyright (C) 2015 Matthew Pickering

;; Author Matthew Pickering
;; Keywords: haskell, refactor
;; Package-Version: 20151124.1241
;; Version: 0.0.1
;; URL: https://github.com/mpickering/hlint-refactor-mode

;;; Commentary:

;; This package provides a minor mode to apply the suggestions from
;; hlint.
;;
;; To activate it use (add-hook 'haskell-mode-hook 'hlint-refactor-mode).

;;; Code:

(defun hlint-refactor-call-process-region-checked (start end program &optional args)
  "Send text from START to END to PROGRAM with ARGS.
This is a wrapper around `call-process-region' that doesn't replace
the region with the output of PROGRAM if it returned a non-zero
exit code."
  (let ((exit (apply 'call-process-region
                     start end
                     program            ; name of program
                     t                  ; delete region
                     t                  ; send output to buffer
                     nil                ; no redisplay during output
                     args
                     )))
    (unless (eq exit 0) (primitive-undo 1 buffer-undo-list))))


(defun hlint-refactor-call-process-region-preserve-point (start end program &optional args)
  "Send text from START to END to PROGRAM with ARGS preserving the point.
This uses `call-process-region-checked' internally."
  (let ((line (line-number-at-pos))
        (column (current-column)))
    (hlint-refactor-call-process-region-checked start end program args)
    (goto-line line)
    (move-to-column column)))

;;;###autoload
(defun hlint-refactor-refactor-buffer (&optional args)
  "Apply all hlint suggestions in the current buffer.
ARGS specifies additional arguments that are passed to hlint."
  (interactive)
  (hlint-refactor-call-process-region-preserve-point
   (point-min)
   (point-max)
   "hlint"
   (append '("--refactor"
             "-")
           args)))

;;;###autoload
(defun hlint-refactor-refactor-at-point ()
  "Apply the hlint suggestion at point."
  (interactive)
  (let ((col (number-to-string (+ 1 (current-column))))
        (line (number-to-string (line-number-at-pos))))
    (hlint-refactor-refactor-buffer
     (list (concat "--refactor-options=--pos " line "," col)))))

;;;###autoload
(define-minor-mode hlint-refactor-mode
  "Automatically apply hlint suggestions"
  :lighter " hlint-refactor"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c , b") 'hlint-refactor-refactor-buffer)
            (define-key map (kbd "C-c , r") 'hlint-refactor-refactor-at-point)
            map))

(provide 'hlint-refactor)

;;; hlint-refactor-mode.el ends here

;;; hlint-refactor.el ends here
