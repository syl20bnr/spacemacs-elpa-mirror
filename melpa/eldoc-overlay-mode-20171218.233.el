;;; eldoc-overlay-mode.el --- Display eldoc with contextual documentation overlay.

;; Author: stardiviner <numbchild@gmail.com>
;; Keywords: eldoc overlay
;; Package-Version: 20171218.233
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

(defgroup eldoc-overlay nil
  "eldoc-over-mode."
  :prefix "eldoc-overlay-"
  :group 'eldoc)

(defcustom eldoc-overlay-library #'quick-peek
  "Specify the library for displaying eldoc.
Two libraries currently supported: `inline-docs', and `quick-peek'."
  :type 'function
  :group 'eldoc-overlay)

(defvar eldoc-overlay-function (pcase eldoc-overlay-library
                                 (`inline-docs 'inline-docs)
                                 (`quick-peek 'eldoc-overlay-quick-peek))
  "Specify the function for displaying eldoc.
Two functions currently supported: `inline-docs', and `quick-peek'.")

(defcustom eldoc-overlay-disable-in-minibuffer nil
  "Disable eldoc-overlay-mode in minibuffer."
  :type 'boolean
  :group 'eldoc-overlay)

(defun eldoc-overlay-disable ()
  "Disable `eldoc-overlay-mode' in some modes."
  (setq-local eldoc-message-function #'eldoc-minibuffer-message))

(defun eldoc-overlay-quick-peek (format-string &rest args)
  "The real function to show FORMAT-STRING and ARGS of `eldoc-over-mode'."
  (if (and eldoc-overlay-disable-in-minibuffer
           (minibufferp))
      (eldoc-overlay-disable)
    ;; (eldoc-minibuffer-message format-string args)
    (when format-string
      (quick-peek-show
       (apply 'format format-string args)
       (point)
       1))))

;;;###autoload
(define-minor-mode eldoc-overlay-mode
  "Minor mode for displaying eldoc with contextual documentation overlay."
  :init-value t
  :lighter " ElDoc overlay"
  :keymap eldoc-overlay-mode-map
  :global t
  :require 'eldoc-overlay-mode
  (if eldoc-overlay-mode
      (progn
        (setq eldoc-message-function eldoc-overlay-function)
        (add-hook 'post-command-hook 'quick-peek-hide)
        (add-hook 'org-mode-hook #'eldoc-overlay-disable))
    (setq eldoc-message-function #'eldoc-minibuffer-message)
    )
  )

;;; ----------------------------------------------------------------------------

(provide 'eldoc-overlay-mode)

;;; eldoc-overlay-mode.el ends here
