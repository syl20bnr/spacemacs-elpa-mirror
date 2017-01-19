;;; eldoc-overlay-mode.el --- Display eldoc with contextual documentation overlay.

;; Author: stardiviner <numbchild@gmail.com>
;; Keywords: eldoc overlay
;; Package-Version: 20170118.2329
;; URL: https://github.com/stardiviner/eldoc-overlay-mode
;; Created: 14th Jan 2017
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------

(defvar eldoc-overlay--overlay nil)

(defface eldoc-overlay-face
  '((t (:inherit italic)))
  "Face for eldoc-overlay-mode."
  :group 'eldoc)

(defun eldoc-overlay--clear-overlay ()
  "Clear eldoc-overlay overlays."
  (when (overlayp eldoc-overlay--overlay)
    (delete-overlay eldoc-overlay--overlay))
  (remove-hook 'post-command-hook 'eldoc-overlay--clear-overlay))

(defun eldoc-overlay--string-display-next-line (string)
  "Overwrite contents of next line with STRING until next command."
  (let* ((indent-spaces (make-string (- (current-indentation) 1) ?\s))
         (str (concat indent-spaces
                      (propertize "➜ " 'face 'font-lock-doc-face)
                      (copy-sequence string) ; original eldoc string with format
                      "\n"))
         start-pos end-pos)
    (unwind-protect
        (save-excursion
          (eldoc-overlay--clear-overlay)
          (forward-line)
          (setq start-pos (point))
          (end-of-line)
          (setq end-pos (point))
          (setq eldoc-overlay--overlay (make-overlay start-pos end-pos (current-buffer)))
          ;; Change the face
          ;; (overlay-put eldoc-overlay--overlay 'face 'eldoc-overlay-face)
          ;; Hide full line
          ;; (overlay-put eldoc-overlay--overlay 'display "")
          ;; (overlay-put eldoc-overlay--overlay 'display :height 120)
          ;; pre-pend indentation spaces
          ;; (overlay-put eldoc-overlay--overlay 'line-prefix indent-spaces)
          ;; auto delete overlay with property 'evaporate
          (overlay-put eldoc-overlay--overlay 'evaporate t)
          ;; Display message
          (overlay-put eldoc-overlay--overlay 'before-string str))
      (add-hook 'post-command-hook 'eldoc-overlay--clear-overlay))))

(defun eldoc-overlay-display-message-momentary (format-string &rest args)
  "Display eldoc message FORMAT-STRING near point with extra ARGS."
  (when format-string
    (eldoc-overlay--string-display-next-line
     (apply 'format format-string args))))

(defvar eldoc-overlay-mode-map
  (let ((map (make-sparse-keymap)))
    map))

;;;###autoload
(define-minor-mode eldoc-overlay-mode
  "Minor mode for displaying eldoc with contextual documentation overlay."
  :init-value t
  :lighter " ElDoc overlay"
  :keymap eldoc-overlay-mode-map
  :global t
  (if eldoc-overlay-mode
      (setq eldoc-message-function #'eldoc-overlay-display-message-momentary)
    (setq eldoc-message-function #'eldoc-minibuffer-message)
    )
  )

;;; ----------------------------------------------------------------------------

(provide 'eldoc-overlay-mode)

;;; eldoc-overlay-mode.el ends here
