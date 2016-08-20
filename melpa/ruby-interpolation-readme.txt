   A little minor mode providing electric keys for #{} interpolation
 in ruby

Installation:

   Drop into your `vendor` directory and `(require 'ruby-interpolation)`

Code

(defvar ruby-interpolation-key "#"
  "The key to invoke ruby string interpolation via #{}")

(defvar ruby-interpolation-mode-map
  (let ((map (make-sparse-keymap))
        (key (read-kbd-macro ruby-interpolation-key)))
    (define-key map key 'ruby-interpolation-insert)
    map)
  "Keymap for `ruby-interpolation-mode`.")

(defun ruby-interpolation-string-at-point-p()
  (cond ((and ruby-interpolation-mode
	      (consp (memq 'font-lock-string-face (text-properties-at (point)))))
	 (save-excursion
	   (search-backward-regexp "\"\\|'" nil t)
	   (string= "\"" (string (char-after (point))))
	   ))))

(defun ruby-interpolation-insert ()
  "Called when interpolation key is pressed"
  (interactive)
  (if (ruby-interpolation-string-at-point-p)
      (progn (save-excursion (insert "#{}"))
             (forward-char 2))
    (insert "#")))

###autoload
(define-minor-mode ruby-interpolation-mode
  "Automatic insertion of ruby string interpolation."
  :init-value nil
  :lighter " #{}"
  :keymap ruby-interpolation-mode-map)

(add-hook 'ruby-mode-hook 'ruby-interpolation-mode)

(provide 'ruby-interpolation)

ruby-interpolation.el ends here
