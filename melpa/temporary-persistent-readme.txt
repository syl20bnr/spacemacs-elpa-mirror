temporary-persistent.el -  easy way to switch temp buffers and keep them
persistent. It provides `temporary-persistent-switch-buffer' function
to create temporary buffers named *temp*, *temp-1* and so on, witch is
associated to files and will be saved any time you run `kill-buffer' or
`kill-emacs'.
Furtermore, you can save them manually any time via `save-buffer' function.
See README.md for more information.

(require 's)
(require 'dash)
(require 'names)

(defgroup temporary-persistent nil
  "Keep temp notes buffers persistent."
  :group 'convenience)

(define-namespace temporary-persistent-

(defcustom default-submodes (list 'linum-mode
                                  'auto-fill-mode
                                  'auto-complete-mode)
  "List of submodes enabled in new temp buffer.")

(defcustom store-folder "~/temp"
  "Directory to keep files with temporary buffers content."
  :type 'string
  :group 'temporary-persistent)

(defcustom buffer-name-template "temp"
  "Template for temporary buffers names."
  :type 'string
  :group 'temporary-persistent)

(defun save-and-kill-buffer ()
  "Save buffer contents and kill buffer."
  (save-buffer)
  (set (make-local-variable 'kill-buffer-query-functions) nil)
  (kill-buffer (current-buffer)))

(defun save-all-related-buffers ()
  "Save all buffers corresponding to `buffer-name-template'."
  (-map
   (lambda (buf)
     (if (string-match
          (concat "^\\*" buffer-name-template "\\(-[0-9]+\\)?" "\\*$" )
          (buffer-name buf))
         (save-buffer buf)))
   (buffer-list)))

:autoload
(defun switch-buffer (&optional num)
  "Switch to temp buffer."
  (interactive "P")
  (let* ((temp-file-name (if (and num (numberp num))
                             (concat buffer-name-template
                                     "-"
                                     (int-to-string num))
                           buffer-name-template))
         (temp-file-path (progn
                           (unless (file-exists-p store-folder)
                             (make-directory store-folder t))
                           (expand-file-name temp-file-name store-folder)))
         (temp-buffer-name (concat "*" temp-file-name "*")
                           buffer-name-template))
    (if (not (get-buffer temp-buffer-name))
        (progn
          (find-file temp-file-path)
          (rename-buffer temp-buffer-name)
          (-map (lambda (mode)
                  (when (fboundp mode)
                    (funcall mode t)))
                default-submodes))
      (switch-to-buffer temp-buffer-name))
    (set (make-local-variable 'kill-buffer-query-functions)
         'temporary-persistent-save-and-kill-buffer)))
)

(add-hook 'kill-emacs-hook 'temporary-persistent-save-all-related-buffers)

(provide 'temporary-persistent)

temporary-persistent.el ends here
