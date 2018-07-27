;;; org-linkany.el --- Insert link using anything.el/helm.el on org-mode

;; Copyright (C) 2014  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: org, completion
;; Package-Version: 20160207.411
;; URL: https://github.com/aki2o/org-linkany
;; Version: 0.0.2
;; Package-Requires: ((log4e "0.2.0") (yaxception "0.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; This extension provides completion to insert link using anything.el/helm.el on org-mode.

;;; Dependency:
;; 
;; - yaxception.el ( see <https://github.com/aki2o/yaxception> )
;; - log4e.el ( see <https://github.com/aki2o/log4e> )

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;; 
;; (require 'org-linkany)

;;; Configuration:
;; 
;; ;; Make config suit for you. About the config item, see Customization or eval the following sexp.
;; ;; (customize-group "org-linkany")

;;; Customization:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "org-linkany/" :docstring t)
;; `org-linkany/preferred-backend'
;; Function of the preferred backend.
;; `org-linkany/url-source-collection'
;; List for the completion of http/https/ftp link.
;; `org-linkany/browse-function'
;; Function for browsing the URL of the candidate in anything/helm buffer.
;; `org-linkany/use-migemo'
;; Whether use migemo to incremental search in anything/helm buffer.
;; `org-linkany/collect-url-number-limit'
;; Number as the limit of the candidate from `org-linkany/source-link-in-org-buffer'/`org-linkany/source-url-in-other-buffer'.
;; `org-linkany/seek-max-buffer-size'
;; Number as the threshold of the buffer size whether seek URL in the buffer.
;; 
;;  *** END auto-documentation

;;; API:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "org-linkany/" :docstring t)
;; `org-linkany/browse-url-from-minibuffer'
;; Browse the URL inputed in minibuffer.
;; `org-linkany/clear-cache'
;; Clear cache.
;; 
;;  *** END auto-documentation
;; [EVAL] (autodoc-document-lisp-buffer :type 'function :prefix "org-linkany/" :docstring t)
;; `org-linkany/get-hatena-bookmark-candidate-url'
;; Return a URL from the CAND of Hatena::Bookmark.
;; `org-linkany/get-candidate-link-value'
;; Return a URL from the CAND of the link of org-mode buffer.
;; `org-linkany/get-candidate-mail-part'
;; Return a mail address from the CAND of the link of org-mode buffer.
;; `org-linkany/get-bbdb-candidate-name'
;; Return a name from the CAND of BBDB record.
;; `org-linkany/get-bbdb-candidate-mail'
;; Return a mail address from the CAND of BBDB record.
;; 
;;  *** END auto-documentation
;; [Note] Functions and variables other than listed above, Those specifications may be changed without notice.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 24.3.1 (i686-pc-linux-gnu, GTK+ Version 3.4.2) of 2013-08-22 on chindi02, modified by Debian
;; - yaxception.el ... Version 0.1
;; - log4e.el ... Version 0.2.0


;; Enjoy!!!


(eval-when-compile (require 'cl))
(require 'org)
(require 'log4e)
(require 'yaxception)
(require 'org-bbdb nil t)
(require 'anything nil t)
(require 'anything-config nil t)
(require 'anything-migemo nil t)
(require 'anything-hatena-bookmark nil t)
(require 'helm nil t)
(require 'helm-config nil t)
(require 'helm-files nil t)
(require 'helm-migemo nil t)
(require 'helm-w3m nil t)
(require 'helm-firefox nil t)
(require 'helm-hatena-bookmark nil t)
(require 'bbdb nil t)
(require 'bbdb-com nil t)

(defgroup org-linkany nil
  "Insert link using anything.el/helm.el on org-mode."
  :group 'org
  :prefix "org-linkany/")

(defcustom org-linkany/preferred-backend 'helm
  "Function of the preferred backend."
  :type 'function
  :group 'org-linkany)

(defcustom org-linkany/url-source-collection
  '((org-linkany/source-link-in-org-buffer . org-linkany/get-candidate-link-value)
    (org-linkany/source-url-in-other-buffer)
    (helm-source-w3m-bookmarks . helm-w3m-bookmarks-get-value)
    (anything-c-source-w3m-bookmarks . anything-c-w3m-bookmarks-get-value)
    (helm-source-firefox-bookmarks . helm-firefox-bookmarks-get-value)
    (anything-c-source-firefox-bookmarks . anything-c-firefox-bookmarks-get-value)
    (helm-c-source-hatena-bookmark . org-linkany/get-hatena-bookmark-candidate-url)
    (anything-c-source-hatena-bookmark . org-linkany/get-hatena-bookmark-candidate-url))
  "List for the completion of http/https/ftp link.

This value is a list of (VARIABLE . FUNCTION).
VARIABLE is a symbol of the variable as the source of anything.el/helm.el handles URL.
If the sources having same name exists, the first one only is used.
FUNCTION is a symbol of the function for picking up URL from the selected candidate.
The function is given the selected candidate and has to return the URL part."
  :type '(repeat (cons variable function))
  :group 'org-linkany)

(defcustom org-linkany/browse-function 'browse-url-firefox
  "Function for browsing the URL of the candidate in anything/helm buffer.

This function is called when you do persistent-action that is bound to C-z."
  :type 'function
  :group 'org-linkany)

(defcustom org-linkany/use-migemo t
  "Whether use migemo to incremental search in anything/helm buffer."
  :type 'boolean
  :group 'org-linkany)

(defcustom org-linkany/collect-url-number-limit 99
  "Number as the limit of the candidate from `org-linkany/source-link-in-org-buffer'/`org-linkany/source-url-in-other-buffer'."
  :type 'integer
  :group 'org-linkany)

(defcustom org-linkany/seek-max-buffer-size 1048576
  "Number as the threshold of the buffer size whether seek URL in the buffer."
  :type 'integer
  :group 'org-linkany)


(log4e:deflogger "org-linkany" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                         (error . "error")
                                                         (warn  . "warn")
                                                         (info  . "info")
                                                         (debug . "debug")
                                                         (trace . "trace")))
(org-linkany--log-set-level 'trace)


(defvar org-linkany--buffer-name " *linkany*")

;;;;;;;;;;;;;
;; Utility

(defun* org-linkany--show-message (msg &rest args)
  (apply 'message (concat "[ORG-LINKANY] " msg) args)
  nil)

(defmacro org-linkany--aif (test then &rest else)
  (declare (indent 2))
  `(let ((it ,test)) (if it ,then ,@else)))

(defmacro org-linkany--awhen (test &rest body)
  (declare (indent 1))
  `(let ((it ,test)) (when it ,@body)))

(defun org-linkany--update-candidate-in-buffer (&optional link-only)
  (org-linkany--trace "start update candidate in buffer : link-only[%s]" link-only)
  (dolist (buff (buffer-list))
    (with-current-buffer buff
      (when (< (buffer-size) org-linkany/seek-max-buffer-size)
        (if (eq major-mode 'org-mode)
            (org-linkany--seek-link-in-org-buffer)
          (when (not link-only)
            (org-linkany--seek-url-in-other-buffer)))))))

(defun org-linkany--detect-backend ()
  (cond ((and org-linkany/preferred-backend
              (fboundp org-linkany/preferred-backend)) org-linkany/preferred-backend)
        ((fboundp 'helm)                               'helm)
        ((fboundp 'anything)                           'anything)
        (t
         (org-linkany--show-message "Not found any available backend method : anything, helm"))))

(defun* org-linkany--gen-source (backend org-source-sym &key action persistent-action persistent-help)
  (yaxception:$
    (yaxception:try
      (org-linkany--trace
       "start gen source from [%s]\naction: %s\npersistent-action: %s\npersistent-help: %s"
       org-source-sym action persistent-action persistent-help)
      (when (and (boundp org-source-sym)
                 (sequencep (symbol-value org-source-sym))
                 (or (not action)
                     (fboundp action)))
        (let ((src (copy-sequence (symbol-value org-source-sym)))
              (candbuff-subfunc (intern (concat (symbol-name backend) "-candidates-in-buffer"))))
          ;; Use migemo
          (when (and org-linkany/use-migemo
                     (not (assq 'migemo src)))
            (push '(migemo) src))
          ;; For migemo, use anything-candidates-in-buffer as substitute for candidates-in-buffer
          (org-linkany--awhen (and (fboundp candbuff-subfunc)
                                   (assq 'candidates-in-buffer src))
            (setq src (delq it src))
            (push `(candidates . ,candbuff-subfunc) src))
          ;; Customize properties
          (loop for e in `((action ,(or action 'identity))
                           (persistent-action ,persistent-action t)
                           (persistent-help ,persistent-help t)
                           (type)
                           (action-transformer))
                do (multiple-value-bind (key newvalue remain) e
                     (org-linkany--awhen (and (or (not remain)
                                                  newvalue)
                                              (assq key src))
                       (setq src (delq it src)))
                     (when newvalue
                       (push `(,key . ,newvalue) src))))
          (org-linkany--debug "generated source from [%s]\n%s" org-source-sym src)
          src)))
    (yaxception:catch 'error e
      (org-linkany--show-message "Failed generate source : %s" (yaxception:get-text e))
      (org-linkany--error "failed gen source : %s\n%s"
                          (yaxception:get-text e)
                          (yaxception:get-stack-trace-string e)))))


;;;;;;;;;;;;;;;;;;;;;;;
;; Fix To Return URL

(defun org-linkany/get-hatena-bookmark-candidate-url (cand)
  "Return a URL from the CAND of Hatena::Bookmark."
  (if (not (string-match "\\[href:\\(.+\\)\\]$" cand))
      (org-linkany--show-message "Not found href in %s" cand)
    (match-string-no-properties 1 cand)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get Link From Org Buffer

(defun org-linkany/get-candidate-link-value (cand)
  "Return a URL from the CAND of the link of org-mode buffer."
  (replace-regexp-in-string " \\[.+\\]\\'" "" cand))

(defvar org-linkany/source-link-in-org-buffer nil)
(defun org-linkany--init-source-link-in-org-buffer ()
  (when (not org-linkany/source-link-in-org-buffer)
    (org-linkany--trace "init org-linkany/source-link-in-org-buffer")
    (setq org-linkany/source-link-in-org-buffer
          `((name . "Link in Org Buffer")
            (candidates . org-linkany--get-links-in-org-buffer)
            (candidate-number-limit . ,org-linkany/collect-url-number-limit)))))

(defvar org-linkany--org-buffer-link-alist nil)
(defun org-linkany--get-links-in-org-buffer ()
  (mapcar (lambda (e)
            (let ((link (car e))
                  (description (cdr e)))
              (if description (format "%s [%s]" link description) link)))
          org-linkany--org-buffer-link-alist))

(defvar org-linkany--regexp-link-format
  (rx-to-string `(and bos "[["
                      (group (+ (not (any "]")))) "]"
                      (? "[" (group (+ (not (any "]")))) "]")
                      "]" eos)))
(defun org-linkany--seek-link-in-org-buffer ()
  (yaxception:$
    (yaxception:try
      (org-linkany--trace "start seek link in org buffer : %s" (buffer-name))
      (save-excursion
        (save-restriction
          (widen)
          (loop initially (goto-char (point-min))
                for nextpt = (text-property-any (point) (point-max) 'face 'org-link)
                while (and nextpt
                           (< (length org-linkany--org-buffer-link-alist)
                              org-linkany/collect-url-number-limit))
                do (let* ((endpt (or (next-single-property-change nextpt 'face)
                                     (point-max)))
                          (linktext (buffer-substring-no-properties nextpt endpt)))
                     (org-linkany--trace "found link text : %s" linktext)
                     (when (string-match org-linkany--regexp-link-format linktext)
                       (let* ((link (match-string-no-properties 1 linktext))
                              (description (ignore-errors (match-string-no-properties 2 linktext))))
                         (org-linkany--trace "got link : %s [%s]" link description)
                         (pushnew `(,link . ,description) org-linkany--org-buffer-link-alist :test 'equal)))
                     (goto-char endpt))))))
    (yaxception:catch 'error e
      (org-linkany--show-message "Failed seek link in org buffer : %s" (yaxception:get-text e))
      (org-linkany--error "failed seek link in org buffer : %s\n%s"
                          (yaxception:get-text e)
                          (yaxception:get-stack-trace-string e)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get URL From Other Buffer

(defvar org-linkany--other-buffer-urls nil)

(defvar org-linkany/source-url-in-other-buffer nil)
(defun org-linkany--init-source-url-in-other-buffer ()
  (when (not org-linkany/source-url-in-other-buffer)
    (org-linkany--trace "init org-linkany/source-url-in-other-buffer")
    (setq org-linkany/source-url-in-other-buffer
          `((name . "URL in Other Buffer")
            (candidates . (lambda () org-linkany--other-buffer-urls))
            (candidate-number-limit . ,org-linkany/collect-url-number-limit)))))

(defvar org-linkany--regexp-url-part
  (rx-to-string `(and bow (or "http" "https" "ftp") "://" (+ (any "a-zA-Z0-9_./:;#~@?&%=+-")))))
(defun org-linkany--seek-url-in-other-buffer ()
  (yaxception:$
    (yaxception:try
      (org-linkany--trace "start seek url in other buffer : %s" (buffer-name))
      (save-excursion
        (loop initially (goto-char (point-min))
              while (re-search-forward org-linkany--regexp-url-part nil t)
              for url = (match-string-no-properties 0)
              if (>= (length org-linkany--other-buffer-urls)
                     org-linkany/collect-url-number-limit)
              return nil
              if (not (member url org-linkany--other-buffer-urls))
              do (progn
                   (org-linkany--trace "found url : %s" url)
                   (push url org-linkany--other-buffer-urls)))))
    (yaxception:catch 'error e
      (org-linkany--show-message "Failed seek url in other buffer : %s" (yaxception:get-text e))
      (org-linkany--error "failed seek url in other buffer : %s\n%s"
                          (yaxception:get-text e)
                          (yaxception:get-stack-trace-string e)))))


;;;;;;;;;;;;;;;;;;
;; Complete URL

(defvar org-linkany/complete-url-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "C-z") 'org-linkany/browse-url-from-minibuffer)
    map))

(defun org-linkany/browse-url-from-minibuffer ()
  "Browse the URL inputed in minibuffer."
  (interactive)
  (yaxception:$
    (yaxception:try
      (org-linkany--trace "start browse url from minibuffer")
      (let ((url (with-selected-window (or (active-minibuffer-window)
                                           (minibuffer-window))
                   (minibuffer-contents))))
        (save-selected-window
          (funcall org-linkany/browse-function url))))
    (yaxception:catch 'error e
      (org-linkany--show-message "Failed browse url : %s" (yaxception:get-text e))
      (org-linkany--error "failed browse url from minibuffer : %s\n%s"
                          (yaxception:get-text e)
                          (yaxception:get-stack-trace-string e)))))

(defvar org-linkany--available-url-sources nil)
(defun org-linkany--get-available-url-sources (backend)
  (org-linkany--init-source-link-in-org-buffer)
  (org-linkany--init-source-url-in-other-buffer)
  (or org-linkany--available-url-sources
      (setq org-linkany--available-url-sources
            (loop initially (org-linkany--trace "start collect available sources : backend[%s]" backend)
                  with found-names = nil
                  for e in org-linkany/url-source-collection
                  for org-source-sym = (car e)
                  for action-sym = (or (cdr e) 'identity)
                  for src = (org-linkany--gen-source
                             backend
                             org-source-sym
                             :action action-sym
                             :persistent-action `(lambda (cand)
                                                   (funcall org-linkany/browse-function
                                                            (,action-sym cand)))
                             :persistent-help "Browse this URL")
                  for srcnm = (when src
                                (assoc-default 'name src))
                  if (and src
                          (not (member srcnm found-names)))
                  collect (progn
                            (org-linkany--info "got source from [%s]/[%s]" org-source-sym action-sym)
                            (push srcnm found-names)
                            src)))))

(defun org-linkany--complete-url (type)
  (yaxception:$
    (yaxception:try
      (org-linkany--trace "start complete url : type[%s]" type)
      (org-linkany--awhen (org-linkany--detect-backend)
        (let ((sources (org-linkany--get-available-url-sources it)))
          (org-linkany--update-candidate-in-buffer)
          (org-linkany--trace "start %s\n%s"
                              it
                              (mapconcat (lambda (x) (format "%s" x)) sources "\n"))
          (read-from-minibuffer "URL: "
                                (funcall it :sources sources :buffer org-linkany--buffer-name)
                                org-linkany/complete-url-map))))
    (yaxception:catch 'error e
      (org-linkany--show-message "Failed complete %s : %s" type (yaxception:get-text e))
      (sleep-for 3)
      (org-linkany--error "failed complete url : %s\n%s"
                          (yaxception:get-text e)
                          (yaxception:get-stack-trace-string e)))))

(defun org-http-complete-link ()
  (org-linkany--complete-url "http"))

(defun org-https-complete-link ()
  (org-linkany--complete-url "https"))

(defun org-ftp-complete-link ()
  (org-linkany--complete-url "ftp"))


;;;;;;;;;;;;;;;;;;;
;; Complete BBDB

(defun org-linkany/get-candidate-mail-part (cand)
  "Return a mail address from the CAND of the link of org-mode buffer."
  (replace-regexp-in-string "\\`mailto:" "" (org-linkany/get-candidate-link-value cand)))

(defvar org-linkany/source-mail-in-org-buffer nil)
(defun org-linkany--get-source-mail-in-org-buffer ()
  (or org-linkany/source-mail-in-org-buffer
      (setq org-linkany/source-mail-in-org-buffer
            (let ((src (progn
                         (org-linkany--init-source-link-in-org-buffer)
                         (copy-sequence org-linkany/source-link-in-org-buffer))))
              (push `(action . org-linkany/get-candidate-mail-part) src)
              src))))

(defvar org-linkany--bbdb-candidates nil)
(defun org-linkany--update-bbdb-candidates ()
  (org-linkany--trace "start update bbdb candidates")
  (setq org-linkany--bbdb-candidates
        (mapcar (lambda (r)
                  (format "%s %s" (bbdb-record-name r) (bbdb-record-mail r)))
                (bbdb-records))))

(defvar org-linkany--regexp-bbdb-candidate
  (rx-to-string `(and bos (group (+ not-newline)) " (" (group (+ (not (any ")")))) ")" eos)))
(defun org-linkany--get-bbdb-candidate-attribute (cand idx)
  (when (string-match org-linkany--regexp-bbdb-candidate cand)
    (match-string-no-properties idx cand)))

(defun org-linkany/get-bbdb-candidate-name (cand)
  "Return a name from the CAND of BBDB record."
  (org-linkany--get-bbdb-candidate-attribute cand 1))

(defun org-linkany/get-bbdb-candidate-mail (cand)
  "Return a mail address from the CAND of BBDB record."
  (let ((mails (split-string (org-linkany--get-bbdb-candidate-attribute cand 2) " +")))
    (if (> (length mails) 1)
        (completing-read "Select Address: " mails nil t)
      (car mails))))

(defun org-linkany--get-bbdb-source (action)
  `((name . "BBDB Record")
    (init . org-linkany--update-bbdb-candidates)
    (candidates . (lambda () org-linkany--bbdb-candidates))
    (candidate-number-limit . 9999)
    (action . ,action)
    (migemo)))

(defun* org-linkany--complete-bbdb (type &rest sources)
  (yaxception:$
    (yaxception:try
      (org-linkany--trace "start complete bbdb")
      (org-linkany--awhen (org-linkany--detect-backend)
        (org-linkany--update-candidate-in-buffer t)
        (org-linkany--trace "start %s\n%s"
                            it
                            (mapconcat (lambda (x) (format "%s" x)) sources "\n"))
        (org-linkany--awhen (funcall it :sources sources :buffer org-linkany--buffer-name)
          (concat type ":" it))))
    (yaxception:catch 'error e
      (org-linkany--show-message "Failed complete %s : %s" type (yaxception:get-text e))
      (sleep-for 3)
      (org-linkany--error "failed complete bbdb : %s\n%s"
                          (yaxception:get-text e)
                          (yaxception:get-stack-trace-string e)))))

(defun org-mailto-complete-link ()
  (org-linkany--complete-bbdb "mailto"
                              (org-linkany--get-bbdb-source 'org-linkany/get-bbdb-candidate-mail)
                              (org-linkany--get-source-mail-in-org-buffer)))

(defun org-bbdb-complete-link ()
  (org-linkany--complete-bbdb "bbdb"
                              (org-linkany--get-bbdb-source 'org-linkany/get-bbdb-candidate-name)))


;;;;;;;;;;;;;;;;;;;
;; Complete File

(defvar org-linkany--available-file-source nil)
(defadvice org-iread-file-name (around org-linkany/replace activate)
  (or (yaxception:$
        (yaxception:try
          (org-linkany--trace "start complete file")
          (org-linkany--awhen (org-linkany--detect-backend)
            (let* ((bknm (symbol-name it))
                   (func (intern-soft (concat bknm "-find-files")))
                   (src  (or org-linkany--available-file-source
                             (setq org-linkany--available-file-source
                                   (org-linkany--gen-source
                                    it
                                    (or (intern-soft (concat bknm "-c-source-find-files"))
                                        (intern-soft (concat bknm "-source-find-files")))))))
                   (anything-c-source-find-files src)
                   (helm-source-find-files src))
              (when (and func src)
                (org-linkany--trace "start %s\n%s" func src)
                (org-linkany--awhen (funcall func nil)
                  (org-linkany--trace "finished %s : %s" func it)
                  (setq ad-return-value it))
                t))))
        (yaxception:catch 'error e
          (org-linkany--show-message "Failed complete file : %s" (yaxception:get-text e))
          (sleep-for 3)
          (org-linkany--error "failed complete file : %s\n%s"
                              (yaxception:get-text e)
                              (yaxception:get-stack-trace-string e))))
      ad-do-it))


;;;;;;;;;;;;;;;;;;;
;; Complete Head

(defvar org-linkany--current-heads nil)
(defun org-head-complete-link ()
  (yaxception:$
    (yaxception:try
      (org-linkany--trace "start complete head")
      (org-linkany--awhen (org-linkany--detect-backend)
        (let ((src `((name . "Current Heading")
                     (init . (lambda ()
                               (setq org-linkany--current-heads
                                     (save-excursion
                                       (loop initially (goto-char (point-min))
                                             while (re-search-forward org-todo-line-regexp nil t)
                                             collect (org-make-org-heading-search-string
                                                      (match-string-no-properties 3)))))))
                     (candidates . (lambda () org-linkany--current-heads))
                     (candidate-number-limit . 999)
                     (action . (lambda (cand) (concat "*" cand)))
                     (migemo))))
          (org-linkany--trace "start %s" it)
          (funcall it :sources src :buffer org-linkany--buffer-name))))
    (yaxception:catch 'error e
      (org-linkany--show-message "Failed complete head : %s" (yaxception:get-text e))
      (sleep-for 3)
      (org-linkany--error "failed complete head : %s\n%s"
                          (yaxception:get-text e)
                          (yaxception:get-stack-trace-string e)))))

(defadvice org-insert-link (around org-linkany/add-head activate)
  (let ((org-link-types org-link-types))
    (pushnew "head" org-link-types :test 'equal)
    ad-do-it))


;;;;;;;;;;;;;;;;;;
;; User Command

;;;###autoload
(defun org-linkany/clear-cache ()
  "Clear cache."
  (interactive)
  (setq org-linkany--available-url-sources nil)
  (setq org-linkany--available-file-source nil)
  (setq org-linkany--org-buffer-link-alist nil)
  (setq org-linkany--other-buffer-urls nil)
  (org-linkany--show-message "Finished clear cache"))


(provide 'org-linkany)
;;; org-linkany.el ends here
