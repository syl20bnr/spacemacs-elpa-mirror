;;; sourcetalk.el --- SourceTalk (http://sourcetalk.net) plugin for Emacs

;; Copyright (C) 2014 Oleg Kalistratov

;; Author: Oleg Kalistratov <oleg@sourcetalk.net>
;; URL: https://github.com/malroc/sourcetalk_emacs
;; Package-Version: 20140823.739
;; Keywords: sourcetalk code discussion
;; Version: 1.2.0
;; Package-Requires: ((request "0.2.0"))

;; Code goes here

(require 'request)

(defun sourcetalk-get-current-line ()
  "Returns the currently selected line number (starting from 1)"
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun sourcetalk-get-buffer-content ()
  "Returns the text content of the current buffer"
  (substring-no-properties (buffer-string)))

(defun sourcetalk-get-buffer-file-name ()
  "Returns the file name of the current buffer"
  (file-name-nondirectory (buffer-file-name (current-buffer))))

(defun sourcetalk-start-external-conference ()
  "Starts a new SourceTalk conference in a browser window"
  (interactive)
  (request
   "http://app.sourcetalk.net/conferences.json"
   :type "POST"
   :data `(("conference[source_files_attributes][0][name]" .
            ,(sourcetalk-get-buffer-file-name))
           ("conference[source_files_attributes][0][source]" .
            ,(sourcetalk-get-buffer-content))
           ("conference[source_files_attributes][0][scroll_position]" .
            ,(number-to-string
              (sourcetalk-get-current-line))))
   :parser 'json-read
   :success (function*
             (lambda
               (&key data &allow-other-keys)
               (browse-url (concat "http://app.sourcetalk.net/conferences/"
                                   (assoc-default 'slug data)))))))

(provide 'sourcetalk)

;;; sourcetalk.el ends here
