;;; niconama.el --- Tools for Niconico Live Broadcast

;; Copyright (C) 2016 Nobuto Kaitoh
;; Auther: Nobuto Kaitoh <nobutoka@gmail.com>

;; niconama.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; niconama.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with niconama.el.
;; If not, see <http://www.gnu.org/licenses/>.

;; Created: 18 June 2016
;; Version: 2.1
;; Package-Version: 20170910.801
;; URL: https://github.com/NOBUTOKA/niconama.el

;; Keywords: comm
;; Package-Requires: ((emacs "24") (request "20170131.1747") (cl-lib "0.5"))

;;; Commentary:

;; This package provides a comment viewer of Niconico Live Broadcast <http://live.nicovideo.jp/>.
;; To use, require this script and configure your Niconico account like this.

;; (setq niconama-user "your@account.com")

;; And then, type M-x niconama-comment-viewer to activate comment viewer.
;; C-RET in "Write Comment" buffer submit the contents of this buffer to broadcast.
;; If you want to switch 184 attribute of comment, use C-c C-y.

;; To kill the comment viewer, use M-x niconama-kill-comment-viewer and type process number
;; shown in comment viewer buffer name as <process number>: <broadcast title>.

;;; Code:

(require 'request)
(require 'cl-lib)
(require 'xml)

(defvar niconama--loginURL "https://secure.nicovideo.jp/secure/login")
(defvar niconama--apiURL "http://live.nicovideo.jp/api/")
(defvar niconama-user nil)
(defvar niconama-pass nil)

(defvar niconama--commentresponse nil)
(defvar niconama--user-id)

(defvar niconama--comment-viewer-process nil)
(defvar niconama--comment-viewer-buffer-name nil)
(defvar niconama--comment-writer-buffer-name nil)
(defvar	niconama--broadcast-thread nil)
(defvar niconama--broadcast-open-time nil)
(defvar niconama--broadcast-ticket nil)
(defvar niconama--comment-last-comment-number '((1 . 1))) ;these 7 variables forms like ((viewer-number.value) (viewer-number.value)).
(defvar niconama--is-process-exist nil)	;car (nthcdr n is-process-exist) shows the nth process is exist.

(defvar niconama--next-comment-viewer-number 1)
(defvar niconama--comment-viewer-number 0)
(make-variable-buffer-local 'niconama--comment-viewer-number)

(defvar niconama--comment-userid "")
(defvar niconama--comment-is184 nil)

(defvar niconama-submitter-map (make-keymap))

(defvar niconama--kotehan-list nil)
(load (concat user-emacs-directory "kotehan.el") t)

;;;###autoload
(defun niconama-comment-viewer (broadcastNum)
  "Activate Niconama Comment Viewer connected to BROADCASTNUM."
  (interactive "MBroadcast Number with \"lv\": ")
  (let (niconama--root
	(niconama--status nil)
	niconama--broadcast-addr
	niconama--broadcast-port
	niconama--broadcast-title
	)

    (when (null niconama-pass)
      (setq niconama-pass (read-passwd (concat "Password of Niconico account " niconama-user " : "))))
    
    (request niconama--loginURL
	     :type "POST"
	     :params '(("site"."nicolive"))
	     :data (concat "mail=" niconama-user "&" "password=" niconama-pass)
	     :headers '(("Content-Type"."application/x-www-form-urlencoded"))
	     :sync t
	     :error (cl-function
		     (lambda (&key response &allow-other-keys)
		       (message "%s\n" (request-response-error-thrown response))
		       ))
	     )


    (message broadcastNum)

    (request (concat niconama--apiURL "getplayerstatus")
	     :type "GET"
	     :params (list (cons "v" broadcastNum))
	     :parser 'xml-parse-region
	     :sync t
	     :success (cl-function
		       (lambda (&key response &allow-other-keys)
			 (setq niconama--root (request-response-data response))
			 (setq niconama--status (cdr (niconama--pick-node-from-xmllist niconama--root "status")))
			 (if (string= niconama--status "ok")
			     (progn
			       (setq niconama--user-id (cadr (niconama--pick-node-from-xmllist niconama--root "user_id")))
			       (setq niconama--broadcast-addr (cadr (niconama--pick-node-from-xmllist niconama--root "addr")))
			       (setq niconama--broadcast-port (string-to-number (cadr (niconama--pick-node-from-xmllist niconama--root "port"))))
			       (setq niconama--broadcast-title (decode-coding-string
								(cadr (niconama--pick-node-from-xmllist niconama--root "title")) 'utf-8-unix))
			       (setq niconama--broadcast-thread
				     (niconama--replace-broadcast-info
				      niconama--broadcast-thread
				      niconama--next-comment-viewer-number
				      (string-to-number (cadr (niconama--pick-node-from-xmllist niconama--root "thread")))))
			       (setq niconama--broadcast-open-time
				     (niconama--replace-broadcast-info
				      niconama--broadcast-open-time
				      niconama--next-comment-viewer-number
				      (string-to-number (cadr (niconama--pick-node-from-xmllist niconama--root "open_time")))))
			       )
			   nil)))
	     :error (cl-function
		     (lambda (&key response &allow-other-keys)
		       (message "%s" (request-response-url response))
		       ))
	     )

    (while (null niconama--status)
      nil)

    (if (string= niconama--status "ok")
	(progn
	  (setq niconama--comment-viewer-process
		(niconama--replace-broadcast-info
		 niconama--comment-viewer-process
		 niconama--next-comment-viewer-number
		 (make-network-process
		  :name "niconama-comment-viewer"
		  :buffer "*commsystem*"
		  :host niconama--broadcast-addr
		  :service niconama--broadcast-port
		  :filter-multibyte t
		  :nowait nil
		  :filter 'niconama--reflesh-comment-viewer
		  :sentinel (cl-function
			     (lambda (proc msg)
			       (let ((viewer-number (niconama--find-broadcast-number
						     niconama--comment-viewer-process proc)))
				 (setq niconama--is-process-exist
				       (niconama--set-process-existance-flag viewer-number niconama--is-process-exist nil))
				 (let ((n 0))
				   (while (car (nthcdr n niconama--is-process-exist))
				     (setq n (1+ n)))
				   (setq niconama--next-comment-viewer-number n)
				   )
				 (kill-buffer	(niconama--find-broadcast-info
						 niconama--comment-writer-buffer-name niconama--next-comment-viewer-number))
				 (kill-buffer (niconama--find-broadcast-info niconama--comment-viewer-buffer-name
									     niconama--next-comment-viewer-number))
				 (delete-window)))))))
	  (message "%s" niconama--comment-viewer-process)
	  (setq niconama--comment-viewer-buffer-name
		(niconama--replace-broadcast-info
		 niconama--comment-viewer-buffer-name
		 niconama--next-comment-viewer-number
		 (concat (number-to-string niconama--next-comment-viewer-number) ": "  niconama--broadcast-title)))
	  (switch-to-buffer (niconama--find-broadcast-info niconama--comment-viewer-buffer-name
							   niconama--next-comment-viewer-number))
	  (erase-buffer)
	  (setq niconama--comment-viewer-number niconama--next-comment-viewer-number)
	  (setq buffer-read-only t)
	  (process-send-string (niconama--find-broadcast-info niconama--comment-viewer-process niconama--next-comment-viewer-number)
			       (format "<thread thread=\"%d\" version=\"20061206\" res_from=\"-100\"/>\0"
				       (niconama--find-broadcast-info niconama--broadcast-thread niconama--next-comment-viewer-number)))

	  (split-window-vertically)
	  (select-window (next-window))
	  (shrink-window 5)
	  (setq niconama--comment-writer-buffer-name
		(niconama--replace-broadcast-info
		 niconama--comment-writer-buffer-name
		 niconama--next-comment-viewer-number
		 (concat "Write Comment to " niconama--broadcast-title)))
	  (switch-to-buffer (niconama--find-broadcast-info niconama--comment-writer-buffer-name niconama--next-comment-viewer-number))
	  (setq niconama--comment-viewer-number niconama--next-comment-viewer-number)
	  (setq major-mode 'Niconama-Comment-Writer)
	  (setq mode-name "Niconama Comment Writer")
	  (define-key niconama-submitter-map [\C-return] #'niconama-submit-comment)
	  (define-key niconama-submitter-map "\C-c\C-y" #'niconama-switch-is184)
	  (use-local-map niconama-submitter-map)

	  (setq niconama--is-process-exist
		(niconama--set-process-existance-flag niconama--next-comment-viewer-number
						      niconama--is-process-exist
						      t))
	  
	  (let ((n 0))
	    (while (car (nthcdr n niconama--is-process-exist))
	      (setq n(1+ n)))
	    (setq niconama--next-comment-viewer-number n)
	    )
	  )
      (message "Broadcast does not exist."))
    )
  )

;;;###autoload
(defun niconama-submit-comment ()
  "Submit comment to broadcast."
  (interactive)
  (if (string-match "Write Comment to" (buffer-name (current-buffer)))
      (let* (vpos postkey mail
		  (viewer-number niconama--comment-viewer-number)
		  (open-time (niconama--find-broadcast-info niconama--broadcast-open-time viewer-number))
		  (thread (niconama--find-broadcast-info niconama--broadcast-thread viewer-number))
		  (last-comment-number (niconama--find-broadcast-info niconama--comment-last-comment-number viewer-number))
		  (ticket (niconama--find-broadcast-info niconama--broadcast-ticket viewer-number))
		  (process (niconama--find-broadcast-info niconama--comment-viewer-process viewer-number)))
	(setq vpos (* (- (+ (* (car (current-time)) (expt 2 16)) (cadr (current-time))) open-time) 100))
	(request "http://live.nicovideo.jp/api/getpostkey"
		 :type "GET"
		 :params (list (cons "thread" (number-to-string thread)) (cons "block_no" (number-to-string (/ (+ last-comment-number 1) 100))))
		 :parser 'buffer-string
		 :sync t
		 :success (cl-function (lambda (&key response &allow-other-keys)
					 (setq postkey (cadr (split-string (request-response-data response) "postkey="))))
				       ))
	(if niconama--comment-is184
	    (setq mail "184")
	  (setq mail ""))
	(process-send-string
	 process
	 (format "<chat thread=\"%s\" vpos=\"%s\" mail=\"%s\" user_id=\"%s\" ticket=\"%s\" postkey=\"%s\" premium=\"0\">%s</chat>\0"
		 thread vpos mail niconama--user-id ticket postkey (buffer-string)))
	(erase-buffer)
	)
    (message "In Wrong Buffer.")
    )
  )

;;;###autoload
(defun niconama-switch-is184 ()
  "Switch whether submitting comment as 184 or not."
  (interactive)
  (setq niconama--comment-is184 (null niconama--comment-is184))
  (if niconama--comment-is184
      (message "184 enabled.")
    (message "184 disabled."))
  )

;;;###autoload
(defun niconama-kill-comment-viewer (num)
  "Kill comment-viewer process of NUM."
  (interactive (list (read-number "Process Number: " 0)))
  (delete-process (niconama--find-broadcast-info
		   niconama--comment-viewer-process num))
  )

(defun niconama--pick-string-from-list (target)
  "Pick the first atom as string from given list.
The list must consist of only list or string.
TARGET:target list"
  (if (listp target)
      (if (car target)
	  (if (stringp (car target))
	      (car target)
	    (niconama--pick-string-from-list(car target)))
	(if (cdr target)
	    (niconama--pick-string-from-list(cdr target))
	  nil))
    target))

(defun niconama--pick-node-from-xmllist (xmllist nodename)
  "Pick up node in XMLLIST named NODENAME.
XMLLIST is xml formated to S-expression."
  (niconama--delete-nillist-from-list (niconama--mapcar-imp #'(lambda (list)
								(cond ((null list) nil)
								      ((listp list) (let ((result (niconama--pick-node-from-xmllist list nodename)))
										      (if result
											  (if (and (atom (car list)) (string= (car list) nodename))
											      list
											    result)
											nil)))
								      ((string= list nodename) list)))
							    xmllist))
  )

(defun niconama--delete-nillist-from-list (targetlist)
  "Remove all nil from TARGETLIST."
  (niconama--restore-to-list (niconama--remove-nil (niconama--mapcar-imp #'(lambda (list)
									     (cond ((niconama--include-listp list) (niconama--delete-nillist-from-list list))
										   ((listp list) (niconama--remove-nil list))
										   (list list))
									     )
									 targetlist)))
  )

(defun niconama--restore-to-list (targetlist)
  "Format TARGETLIST like (((\"foo\" \"bar\"))) as (\"foo\" \"bar\")."
  (if (niconama--include-listp targetlist)
      (niconama--restore-to-list (car targetlist))
    targetlist))

(defun niconama--remove-nil (targetlist)
  "Remove nil from first level of TARGETLIST."
  (if targetlist (if (car targetlist)
		     (cons (car targetlist) (if (listp (cdr targetlist))
						(niconama--remove-nil (cdr targetlist))
					      (cdr targetlist)))
		   (niconama--remove-nil (cdr targetlist)))
    nil)
  )

(defun niconama--include-listp (targetlist)
  "Judge whether TARGETLIST includes inner list or not."
  (niconama--mapor (niconama--mapcar-imp #'listp targetlist)))

(defun niconama--mapor (mlist)
  "Return the first non-nil atom of MLIST.
If all of MLIST is nil, return nil."
  (cond ((null mlist) nil)
	((null (listp mlist)) mlist)
	(t (or (car mlist)
	       (niconama--mapor (cdr mlist))))))

(defun niconama--mapcar-imp (fn mlist)
  "This is mapcar function which can apply to impurity list.
FN: function to apply
MLIST: list to be applied"
  (cond ((null mlist) nil)
	((null (listp mlist)) (funcall fn mlist))
	(t (cons (funcall fn (car mlist))
		 (niconama--mapcar-imp fn (cdr mlist))))))

(defun niconama--find-broadcast-info (list num)
  "Find broadcast infrormation from LIST by the key of NUM."
  (cond ((null list) nil)
	((= (caar list) num) (cdar list))
	(t (niconama--find-broadcast-info (cdr list) num))))

(defun niconama--find-broadcast-number (list info)
  "Find broadcast number from LIST that has INFO."
  (cond ((null list) nil)
	((equal (cdar list) info) (caar list))
	(t (niconama--find-broadcast-number (cdr list) info))))

(defun niconama--replace-broadcast-info (list num info)
  "Return the list made from LIST by adding (NUM.INFO).
If (NUM.someinfo) is exist in the list, replace this."
  (let (newlist)
    (if (null (cond ((null list) nil)
		    ((= (caar list) num) (progn (setcar list (cons num info))
						list))
		    ((setq newlist (niconama--replace-broadcast-info (cdr list) num info))
		     (progn (setcdr list newlist)
			    list))))
	(cons (cons num info) list)
      list)
    )
  )

(defun niconama--set-process-existance-flag (num list bool)
  "Set NUMth process existance flag in LIST to BOOL."
  (if (= num 0)
      (cons bool (cdr list))
    (cons (car list) (niconama--set-process-existance-flag (- num 1) (cdr list) bool))))

(defun niconama--reflesh-comment-viewer (proc string)
  "Write in the comment-viewer buffer of PROC the comment of STRING."
  (setq niconama--commentresponse
	(concat niconama--commentresponse string))
  (when (string= (substring niconama--commentresponse (- (length niconama--commentresponse) 1)) "\u0000")
    (let (commentlist
	  (viewer-number (niconama--find-broadcast-number
			  niconama--comment-viewer-process proc))
	  )
      (set-buffer (niconama--find-broadcast-info niconama--comment-viewer-buffer-name viewer-number))
      (let* (comment (buffer-read-only nil))
	(with-temp-buffer
	  (insert "<comment>")
	  (insert niconama--commentresponse)
	  (goto-char (point-min))
	  (while (re-search-forward "\u0000" nil t)
	    (replace-match ""))
	  (insert "</comment>")
	  (setq commentlist (xml-parse-region)))
	(setq niconama--commentresponse nil)
	(mapcar #'
	 (lambda (comment)
	   (cond ((string= (niconama--pick-string-from-list comment) "chat")
		  (let (commentuserid username)
		    (goto-char (point-max))
		    (setq commentuserid (cdr (niconama--pick-node-from-xmllist comment "user_id")))
		    (setq username (car (niconama--delete-nillist-from-list
					 (mapcar #'(lambda (userlist)
						     (if (string= (format "%s" (car userlist)) commentuserid)
							 (cdr userlist)
						       nil))
						 niconama--kotehan-list))))
		    (cond ((string-match "[@\uff20]" (cl-caddr comment))
			   (niconama--add-kotehan-to-list
			    (cons commentuserid (setq username (cdr (split-string (cl-caddr comment) "[@\uff20]"))))
			    ))
			  (username (goto-char (point-min))
				    (while (re-search-forward (format "%s" commentuserid) nil t)
				      (replace-match (format "%s" username)))
				    (goto-char (point-max))
				    )
			  ((> (string-to-number commentuserid) 10000)
			   (progn
			     (setq niconama--comment-userid (string-to-number commentuserid))
			     (niconama--get-nickname-from-userid)
			     (setq username commentuserid)))
			  ((not username) (setq username commentuserid)))
		    (insert (format "%d\t%02d:%02d\t%s\t%s\n"
				    (niconama--find-broadcast-info
				     (setq niconama--comment-last-comment-number
					   (niconama--replace-broadcast-info
					    niconama--comment-last-comment-number
					    viewer-number
					    (string-to-number (cdr (niconama--pick-node-from-xmllist comment "no"))))) viewer-number)
				    (/ (string-to-number (cdr (niconama--pick-node-from-xmllist comment "vpos"))) 6000)
				    (% (/ (string-to-number (cdr (niconama--pick-node-from-xmllist comment "vpos"))) 100) 60)
				    username
				    (cl-caddr comment)))
		    (setq other-window-scroll-buffer (get-buffer (process-name proc)))
		    (scroll-other-window)
		    (when (string= (cl-caddr comment) "/disconnect")
		      (delete-process proc))))
		 ((string= (niconama--pick-string-from-list comment) "thread")
		  (setq niconama--broadcast-ticket (niconama--replace-broadcast-info
						    niconama--broadcast-ticket
						    viewer-number
						    (cl-cdadr (cl-cddadr comment)))))))
	 (car commentlist)))))
  )

(defun niconama--add-kotehan-to-list (atmic-kotehan-list)
  "If there are no list equals to ATMIC-KOTEHAN-LIST in kotehan-list, add ATMIC-KOTEHAN-LIST to kotehan-list."
  (if (null (niconama--delete-nillist-from-list (mapcar #'(lambda (list)
							    (equal atmic-kotehan-list list))
							niconama--kotehan-list)))
      (progn (setq niconama--kotehan-list (cons atmic-kotehan-list niconama--kotehan-list))
	     (niconama--save-kotehan-list))
    nil
    ))

(defun niconama--save-kotehan-list ()
  "Save Hundlename list."
  (with-temp-buffer
    (insert (format "(setq niconama--kotehan-list '%S)" niconama--kotehan-list))
    (write-file (concat user-emacs-directory "kotehan.el"))))

(defun niconama--get-nickname-from-userid ()
  "Get Niconico nickname from userid."
  (request (format "http://www.nicovideo.jp/user/%s" niconama--comment-userid)
	   :type "GET"
	   :parser 'buffer-string
	   :success (cl-function (lambda (&key response &allow-other-keys)
				   (let ((data (request-response-data response)) nickname)
				     (string-match "<title>\\(.*\\)\u3055\u3093" (decode-coding-string data 'utf-8-unix))
				     (setq nickname (format "%s" (match-string 1 (decode-coding-string data 'utf-8-unix))))
				     (niconama--add-kotehan-to-list (cons (first (last (split-string (request-response-url response) "/"))) nickname))
				     )))
	   :error (cl-function (lambda (&key response &allow-other-keys)
				 (message "%s" (request-response-status-code response))))
	   )
  
  )

(provide 'niconama)
;;; niconama.el ends here

