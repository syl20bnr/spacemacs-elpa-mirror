;;; eldoc-overlay.el --- Display eldoc with contextual documentation overlay.

;; Author: stardiviner <numbchild@gmail.com>
;; Author: Robert Weiner <rsw@gnu.org>
;; Maintainer: stardiviner <numbchild@gmail.com>
;; Keywords: documentation, eldoc, overlay
;; Package-Version: 20171219.940
;; URL: https://github.com/stardiviner/eldoc-overlay
;; Created:  14th Jan 2017
;; Modified: 18th Dec 2017
;; Version: 0.1.1
;; Package-Requires: ((emacs "24.3") (inline-docs "1.0.1") (quick-peek "1.0"))

;;; Commentary:
;;
;; Eldoc displays the function signature of the closest function call
;; around point either in the minibuffer or in the modeline.
;;
;; This package modifies Eldoc to display this documentation inline
;; using a buffer text overlay.
;;
;;  `eldoc-overlay-mode' is a per-buffer minor mode.
;;    A call to `eldoc-overlay-enable' turns it on.
;;    A call to `eldoc-overlay-disable' turns it off
;;
;;    {C-x C-h} interactively calls `eldoc-overlay-toggle' and tells
;;    you the mode's new state.
;;
;;  `global-eldoc-overlay-mode' can be used to toggle this for all buffers.
;;    A call to `global-eldoc-overlay-enable' turns it on.
;;    A call to `global-eldoc-overlay-disable' turns it off
;;
;;    {C-u C-x C-h} interactively calls `global-eldoc-overlay-toggle' and tells
;;    you the mode's new state.
;;
;; By default, the overlay is not used in the minibuffer, eldoc is shown in the modeline
;; in this case.  Set the option `eldoc-overlay-in-minibuffer-flag' non-nil if you want
;; to enable overlay use in the minibuffer.
;;
;; The key used to toggle the mode can be customized by setting the `eldoc-overlay-key'
;; option.
;;
;; Finally, see the documentation for `eldoc-overlay-backend' if you want to try
;; a different overlay display package backend.

;;; Code:
;;; ----------------------------------------------------------------------------

(require 'eldoc)

;; User Options
(defgroup eldoc-overlay nil
  "Display Eldoc function signatures using in-buffer text overlays"
  :prefix "eldoc-overlay-"
  :group 'eldoc)

(defcustom eldoc-overlay-in-minibuffer-flag nil
  "Non-nil (default: nil) means enable `eldoc-overlay-mode' in the minibuffer.
When nil and in the minibuffer, if standard `eldoc-mode' is
enabled, it displays function signatures in the modeline."
  :type 'boolean
  :group 'eldoc-overlay)

(defcustom eldoc-overlay-key (kbd "C-x C-h")
  "Key to toggle eldoc overlay mode."
  :type 'kbd
  :group 'eldoc-overlay
  :set (lambda (option value)
         (set option value)
         (global-set-key value #'eldoc-overlay-toggle)))


;; Variables
(defvar eldoc-overlay-backend 'quick-peek
  "The backend library that displays eldoc overlays.
Two backends are supported: `inline-docs' and `quick-peek'.")

;; Functions
(defun eldoc-overlay-inline-docs (format-string &rest args)
  "Inline-docs backend function to show FORMAT-STRING and ARGS."
  (inline-docs format-string args))

(defun eldoc-overlay-quick-peek (format-string &rest args)
  "Quick-peek backend function to show FORMAT-STRING and ARGS."
  (when format-string
    (quick-peek-show
     (apply 'format format-string args)
     (point)
     1)))

(defun eldoc-overlay-display (format-string &rest args)
  "Display eldoc for the minibuffer when there or call the function indexed by `eldoc-overlay-backend'."
  (if (and (minibufferp) (not eldoc-overlay-in-minibuffer-flag))
      (apply #'eldoc-minibuffer-message format-string args)
    (funcall
     (pcase eldoc-overlay-backend
	     (`inline-docs 'eldoc-overlay-inline-docs)
       (`quick-peek 'eldoc-overlay-quick-peek))
	   (funcall eldoc-documentation-function))))

;; Commands
(defun eldoc-overlay-enable ()
  "Enable `eldoc-overlay-mode' minor mode."
  (interactive)
  (eldoc-overlay-mode 1))

(defun eldoc-overlay-disable ()
  "Disable `eldoc-overlay-mode' minor mode."
  (interactive)
  (eldoc-overlay-mode 0))

(defun global-eldoc-overlay-enable ()
  "Globally enable `eldoc-overlay-mode' minor mode."
  (interactive)
  (global-eldoc-overlay-mode 1))

(defun global-eldoc-overlay-disable ()
  "Globally disable `eldoc-overlay-mode' minor mode."
  (interactive)
  (global-eldoc-overlay-mode 0))

;;;###autoload
(defun global-eldoc-overlay-toggle ()
  "Globally toggle display of eldoc-overlay."
  (if global-eldoc-overlay-mode
      (progn (global-eldoc-overlay-disable)
             (message "Globally disabled eldoc-overlay minor mode"))
    (message "Globally enabled eldoc-overlay minor mode")
    (global-eldoc-overlay-enable)
    (eldoc-message (funcall eldoc-documentation-function))))

;;;###autoload
(defun eldoc-overlay-toggle (global-flag)
  "Toggle display of eldoc-overlay in this buffer or with prefix arg GLOBAL-FLAG, globally."
  (interactive "P")
  (cond
   (global-flag
    (global-eldoc-overlay-toggle))
   (eldoc-overlay-mode
    (eldoc-overlay-disable)
    (message "Disabled eldoc-overlay minor mode in the current buffer"))
   (t (message "Enabled eldoc-overlay minor mode in the current buffer")
      (eldoc-overlay-enable)
      (eldoc-message (funcall eldoc-documentation-function)))))

;;;###autoload
(define-minor-mode eldoc-overlay-mode
  "Minor mode for displaying eldoc contextual documentation using a text overlay."
  :require 'eldoc-overlay-mode
  :group 'eldoc-overlay
  :init-value t
  :lighter " ElDocOver"
  (if eldoc-overlay-mode
      (progn
        (when (eq eldoc-overlay-backend 'quick-peek)
          (add-hook 'post-command-hook #'quick-peek-hide))
        (add-hook 'org-mode-hook #'eldoc-overlay-disable)
        (setq eldoc-message-function #'eldoc-overlay-display)
        (eldoc-mode 1))
    (when (eq eldoc-overlay-backend 'quick-peek)
      (quick-peek-hide)
      ;; Remove hook when no buffers have any peek overlays
      (unless (delq nil (mapcar (lambda (buf) (buffer-local-value 'quick-peek--overlays buf)) (buffer-list)))
        (remove-hook 'post-command-hook #'quick-peek-hide)))
    (eldoc-mode 0)
    (setq eldoc-message-function #'eldoc-minibuffer-message)))

;;;###autoload
(define-globalized-minor-mode global-eldoc-overlay-mode eldoc-overlay-mode eldoc-overlay-enable
  :group 'eldoc-overlay
  :init-value t)


;;; ----------------------------------------------------------------------------

(provide 'eldoc-overlay)

;;; eldoc-overlay.el ends here
