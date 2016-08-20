;;; memento.el --- maintaining daily journals when the day ends. 
;;
;; Author         : Ernst de Hart 
;; Maintainer     : Ernst de Hart <ernstdehart@gmail.com>
;; Created        : August 22, 2015
;; Last modified  : 2015-08-22
;; Version        : 0.1
;; Package-Version: 20150823.339
;; Keywords       : journal, log, diary
;;
;; This file is not part of GNU Emacs.
;;
;;                ( O O ) 
;; =============oOO=(_)==OOo===========  
;;
;;; Code:

;; ------------------------------------------------------
;; Initialize custom variables for Memento (22 aug 2015).
;; ------------------------------------------------------       
(defgroup memento nil
  "Customization options for the package Memento"
  :group 'emacs)

(defcustom memento-file "~/daily-logs" 
  "Customizable variable to specify any file, which will be used for Memento." 
  :type 'string
  :group 'memento)

(defcustom memento-date-format "%Y-%m-%d, %A "
  "Format string for date, by default: YYYY-MM-DD, day."
  :type 'string
  :group 'memento)

(defcustom memento-on-exit t
  "Boolean wether Memento will be invoked on Emacs exit, enabled by default."
  :type 'boolean
  :group 'memento)


;; ------------------------------------------------
;; Functions for the Memento package (22 aug 2015).
;; ------------------------------------------------
;;;###autoload
(defun memento ()
  (interactive)
  (if (file-exists-p memento-file)
      ;; Check if there was a log written today. If this is not the case, then check if it's already tonight except the night.
      (if (and (string< (memento-get-modification-date) (format-time-string "%Y-%m-%d")) (string< "19" (format-time-string "%k")))
          ;; Invoke Memento if the user wants to proceed. 
          (if (yes-or-no-p "Do you want to tell how your day was?")
              (progn (call-interactively 'memento-write-log))))
    ;; If the Memento file doesn't exist yet, create a file and proceed with creating a log.
    (write-region "" nil memento-file)
    (progn (call-interactively 'memento-write-log))))

(defun memento-get-modification-date ()
  "Returns the last modified date of the current memento file."
  (format-time-string "%Y-%m-%d"
                      (nth 5 (file-attributes memento-file))))

(defun memento-invoke-on-exit ()
  "Invokes Memento, if memento-on-exit is enabled"
  (if memento-on-exit 
      (memento)))

(defun memento-write-log (string)
  (interactive "sOk, how did your day go? ")
  "Writes the input to the memento-file"
  (with-temp-buffer
    (insert (memento-return-log))
    ;; Insert the date.
    (insert "# " (format-time-string memento-date-format))  
    (newline-and-indent)
    ;; Write the reply from the user in the log.
    (insert string)  
    (newline-and-indent) (newline-and-indent)
    (when (find-file-noselect memento-file)
      ;; Write the contents.
      (write-region (point-min)
                    (point-max)
                    memento-file))))  

(defun memento-return-log ()
  "Read the contents of Memento file and return it as a string."
  (with-temp-buffer
    (insert-file-contents memento-file)
    (buffer-string)))

(defun memento-open-log ()
  "Open Memento file with the contents."
  (interactive)
  (switch-to-buffer (find-file-noselect memento-file)))


;; ---------------------------------------------
;; Initialize the Memento package (22 aug 2015).
;; ---------------------------------------------       
;; Invoke Memento when the Emacs application quit.
(add-hook 'kill-emacs-hook 'memento-invoke-on-exit)

(provide 'memento)

;;; memento.el ends here.
