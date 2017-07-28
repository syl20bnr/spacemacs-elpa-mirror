;;; dfmt.el --- Emacs Interface to D indenting/formatting tool dfmt.

;; Author: Per Nordlöw
;; Contributors: Kirill Babikhin
;; Maintainer: Kirill Babikhin <qsimpleq>
;; Keywords: tools, convenience, languages, Dlang
;; Package-Version: 20170728.323
;; URL: https://github.com/qsimpleq/elisp-dfmt
;; Version: 0.1.0
;; License: GPL v2, or any later version

;;; Commentary:
;; Original version: https://github.com/nordlow/elisp/blob/master/mine/dfmt.el
;; Enable hotkeys
;; (add-hook 'd-mode-hook 'dfmt-setup-keys)

;;; Code:

(defgroup dfmt nil
  "Interface to D dfmt."
  :group 'tools)

(defcustom dfmt-command "dfmt" "D format command"
  :group 'dfmt)

(defcustom dfmt-flags
  '()
  "Flags sent to dfmt."
  :type 'list
  :group 'dfmt)

(defcustom dfmt-command "dfmt" "D format command"
  :group 'dfmt)

(defvar dfmt-buffer-name "*dfmt*"
  "*Name of the temporary dfmt buffer.")

(defvar dfmt-stderr-buffer-name "*dfmt-stderr*"
  "*Name of the temporary dfmt buffer.")

(defvar dfmt-stderr-verbose t "Verbose error message")

(defvar dfmt-stderr-begin-message "Cannot format buffer, please check your code")

;;;###autoload
(defun dfmt-region (beg end)
  "Format D BUFFER's region from START to END using the external
D formatting program dfmt."
  (interactive "r")
  (if (executable-find dfmt-command)
      (let ((outfile (expand-file-name dfmt-buffer-name temporary-file-directory))
            (errfile (expand-file-name dfmt-stderr-buffer-name temporary-file-directory))
            (outbuffer (get-buffer-create dfmt-buffer-name))
            (errbuffer (get-buffer-create dfmt-stderr-buffer-name))
            (errmsg)
            (d-mode-buffer (current-buffer))
            (old-point (point))
            )

        (set-buffer outbuffer)
        (erase-buffer)
        (set-buffer errbuffer)
        (erase-buffer)
        (set-buffer d-mode-buffer)

        (apply #'call-process-region
               (append (list beg end dfmt-command
                             nil
                             (list dfmt-buffer-name errfile)
                             nil
                             )
                       dfmt-flags))

        (if (> (nth 7 (file-attributes errfile))
               0)
            (progn
              (if dfmt-stderr-verbose
                  (setq errmsg (concat dfmt-stderr-begin-message ":\n"
                                       (with-temp-buffer
                                         (insert-file-contents errfile)
                                         (buffer-string))))
                (setq errmsg dfmt-stderr-begin-message))
              (set-buffer errbuffer)
              (insert errmsg)
              (delete-file errfile)
              (message "%s" errmsg)
              nil)
          (progn
            (delete-region beg end)
            (goto-char beg)
            (insert-buffer-substring outbuffer)
            (goto-char old-point)
            (font-lock-fontify-buffer)
            t)))
    (error "Seem dfmt is not installed")))
(defalias 'd-indent-region 'dfmt-region)

;;;###autoload
(defun dfmt-buffer ()
  "Format D Buffer using the external D formatting program dfmt."
  (interactive)
  (dfmt-region (point-min) (point-max)))
(defalias 'd-indent-buffer 'dfmt-buffer)

;;;###autoload
(defun dfmt-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (let ((buf (current-buffer))
        beg end)
    (if (region-active-p)
        (progn
          (setq beg (region-beginning)
                end (region-end))
          (dfmt-region beg end))
      (dfmt-buffer))
    )
  )
(defalias 'd-indent-region-or-buffer 'dfmt-region-or-buffer)

;;;###autoload
(defun dfmt-file (file out-file)
  "Format D Source or Header FILE using the external program dfmt and put result in OUT-FILE."
  (interactive "fFormat source file: \nFOutput file (from format): ")
  (when (executable-find dfmt-command)
    (progn (apply #'call-process
                  (append (list dfmt-command
                                file
                                `(:file ,out-file)
                                t
                                )
                          dfmt-flags))
           (find-file out-file))))
(defalias 'd-indent-file 'dfmt-file)

;;;###autoload
(defun dfmt-save-buffer ()
  "Format D Buffer using the external D formatting program dfmt and
save current buffer in visited file if modified.
You are will can't to save a brokened dlang code."
  (interactive)
  (when (buffer-modified-p)
    (if (dfmt-buffer)
        (save-buffer)
      (error (with-temp-buffer
               (insert-buffer-substring (get-buffer dfmt-stderr-buffer-name))
               (buffer-string))))))

;;;###autoload
(defun dfmt-setup-keys ()
  (local-set-key [(control c) (F) (r)] 'dfmt-region)
  (local-set-key [(control c) (F) (b)] 'dfmt-buffer)
  (local-set-key [(control c) (F) (f)] 'dfmt-file))

(define-key menu-bar-tools-menu [dfmt-buffer]
  '(menu-item "Tidy D Buffer (dfmt)..." dfmt-buffer
              :help "Format a D source buffer using dfmt"))
(define-key menu-bar-tools-menu [dfmt-file]
  '(menu-item "Tidy D File (dfmt)..." dfmt-file
              :help "Format a D source file using dfmt"))

(provide 'dfmt)
;;; dfmt.el ends here
