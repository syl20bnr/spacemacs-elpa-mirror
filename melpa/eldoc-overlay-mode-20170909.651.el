;;; eldoc-overlay-mode.el --- Display eldoc with contextual documentation overlay.

;; Author: stardiviner <numbchild@gmail.com>
;; Keywords: eldoc overlay
;; Package-Version: 20170909.651
;; URL: https://github.com/stardiviner/eldoc-overlay-mode
;; Created: 14th Jan 2017
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (inline-docs "1.0.1") (quick-peek "1.0"))

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------

(defvar eldoc-overlay-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(defcustom eldoc-overlay-library #'quick-peek
  "Specify the library for displaying eldoc.
Two libraries currently supported: `inline-docs', and `quick-peek'.")

(defvar eldoc-overlay-function (pcase eldoc-overlay-library
                                 (`inline-docs 'inline-docs)
                                 (`quick-peek 'eldoc-overlay-quick-peek))
  "Specify the function for displaying eldoc.
Two functions currently supported: `inline-docs', and `quick-peek'.")

(defun eldoc-overlay-disable-in-org-mode ()
  (setq-local eldoc-message-function #'eldoc-minibuffer-message))

(defun eldoc-overlay-quick-peek (format-string &rest args)
  (when format-string
    (quick-peek-show
     (apply 'format format-string args)
     (point)
     1)))

;;;###autoload
(define-minor-mode eldoc-overlay-mode
  "Minor mode for displaying eldoc with contextual documentation overlay."
  :init-value t
  :lighter " ElDoc overlay"
  :keymap eldoc-overlay-mode-map
  :global t
  (if eldoc-overlay-mode
      (progn
        (setq eldoc-message-function eldoc-overlay-function)
        (add-hook 'post-command-hook 'quick-peek-hide)
        (add-hook 'org-mode-hook #'eldoc-overlay-disable-in-org-mode))
    (setq eldoc-message-function #'eldoc-minibuffer-message)
    )
  )

;;; ----------------------------------------------------------------------------

(provide 'eldoc-overlay-mode)

;;; eldoc-overlay-mode.el ends here
