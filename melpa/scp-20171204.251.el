;;; scp.el --- Use the SCP command to transfer files with the remote server
;;
;; Copyright (C) 2017 zg
;;
;; Author: zg <13853850881@163.com>
;; URL: https://github.com/tszg/emacs-scp
;; Package-Version: 20171204.251
;; Package-X-Original-Version: 0
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))
;; Keywords: convenience, scp
;;
;;; Commentary:
;; (require 'scp)
;; Linux uses the scp command and the sshpass command
;; The pscp command line tool is used under Windows
;;
;;; Code:

(require 'cl-lib)

(define-derived-mode scp-mode special-mode "Scp"
  "Major mode for viewing Scp result."
  (read-only-mode 1))

(defconst scp-tools
  (if (memq system-type '(windows-nt ms-dos))
      "pscp"
    "scp")
  "Set scp tool.")

(defcustom scp-buffer-name "*scp*"
  "Result Buffer name."
  :type 'string)

(defun scp-exist-dir-locals-file()
  "Determine whether the dir-local -file file exists"
  (file-exists-p (concat (locate-dominating-file default-directory dir-locals-file) dir-locals-file)))

(defun scp-show-in-buffer()
  (with-current-buffer (get-buffer-create scp-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (scp-mode)
      (goto-char (point-min)))
    (switch-to-buffer-other-window scp-buffer-name)))

(defun scp-remote-file-path(local-path)
  "String replacement to get the remote file path"
  (replace-regexp-in-string (regexp-quote
			     (locate-dominating-file default-directory dir-locals-file))
			    (concat (scp-get-alist 'remote-path) "/")
			    local-path))

(cl-defun scp-get-alist(key &optional (alist file-local-variables-alist))
  "Gets the value of the Association List"
  (shell-quote-argument (cdr (assoc key alist))))

(defun scp-cmd(status &optional local-path)
  "Stitching scp command"
  (hack-dir-local-variables)
  (let* ((host (scp-get-alist 'host))
	 (user (scp-get-alist 'user))
	 (port (scp-get-alist 'port))
	 (pw (scp-get-alist 'password))
	 (cmd (concat (unless (memq system-type '(windows-nt ms-dos))
			(format "sshpass -p %s " pw))
		      scp-tools  (unless (eq local-path buffer-file-name) " -r")
		      (concat " -P " port " "
			      (when (memq system-type '(windows-nt ms-dos))
				(format "-pw %s " pw)))))
	 (remote-path (concat (scp-remote-file-path local-path)
			      (if (and (not (eq local-path buffer-file-name)) (string-equal status "get"))
				  "*")))
	 (cmd_list (if (string= status "put")
		       (format "%s %s@%s:%s" local-path user host remote-path)
		     (format "%s@%s:%s %s" user host remote-path local-path))))
    (concat cmd cmd_list)))

(cl-defun scp(status &optional (directory buffer-file-name))
  "scp operation"
  ;; (message (concat "scp " status "......"))
  (cond ((not (executable-find scp-tools))
	 (message  (format "Please install the %s" scp-tools)))
	((not (scp-exist-dir-locals-file))
	 (message "Please set the configuration file"))
	((and (not (memq system-type '(windows-nt ms-dos))) (not (executable-find "sshpass")))
	 (message "Please install the sshpass"))
	(t (start-process-shell-command "scp" (scp-show-in-buffer) (scp-cmd status directory)))))

;;;###autoload
(defun scp-get()
  "download"
  (interactive)
  (scp "get"))

;;;###autoload
(defun scp-put()
  "Upload"
  (interactive)
  (scp "put"))

;;;###autoload
(defun scp-get-directory(root)
  "Download the folder"
  (interactive "DLocal directory: ")
  (scp "get" (shell-quote-argument root)))

;;;###autoload
(defun scp-put-directory(root)
  "Upload folder"
  (interactive "DLocal directory: ")
  (scp "put" (shell-quote-argument root)))

(provide 'scp)

;;; scp.el ends here
