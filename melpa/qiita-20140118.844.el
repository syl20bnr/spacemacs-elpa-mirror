;;; qiita.el --- Qiita API Library for emacs

;; Author: Wataru MIYAGUNI (gonngo _at_ gmail.com)
;; URL: https://github.com/gongo/qiita-el
;; Package-Version: 20140118.844
;; Package-Requires: ((helm "1.5.9") (markdown-mode "2.0"))
;; Keywords: qiita

;; Copyright (c) 2012 Wataru MIYAGUNI
;;
;; MIT License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; This program is a tool for Qiita <http://qiita.com> .

;;; Usage:

;;
;; (require 'qiita-el)
;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'json)
(require 'markdown-mode)
(require 'helm)

(defconst qiita->api-endpoint "https://qiita.com/api/v1"
  "The base URI on Qiita API. see <http://qiita.com/docs>")

(defvar qiita->show-buffer-name "*Qiita show*")

(defvar qiita->username nil)
(defvar qiita->token nil)

;;;
;;;
;;; Qiita API
;;;
;;;

(defun qiita:response-status (response)
  (plist-get response :status))

(defun qiita:response-body (response)
  (plist-get response :json))

(defun qiita:api-exec (method path &optional args)
  (with-temp-buffer
    (when qiita->token
      (add-to-list 'args `("token" . ,qiita->token)))
    (let* ((uri (concat qiita->api-endpoint path))
           (query (json-encode-alist args))
           (ret (call-process "curl" nil (current-buffer) nil
                              "-H" "Content-type: application/json"
                              "-s"
                              "-X" method
                              "-w" "\nhttp_code=%{http_code}"
                              "--data-binary" query uri))
           res body)
      (goto-char (point-min))

      (unless (zerop ret)
        (error (format "Error: Not retrieved %s" uri)))
      (unless (re-search-forward "^http_code=\\(.*\\)$" nil t)
        (error "Error: Can't find status code"))

      ;; ectract status code
      (setq res
            (plist-put res :status
                       (string-to-number (match-string-no-properties 1))))
      (delete-region (match-beginning 0) (match-end 0))

      ;; convert string to json object
      (setq body (replace-regexp-in-string
                  "\n+$" ""
                  (buffer-substring-no-properties (point-min) (point-max))))

      (unless (eq 0 (length body))
        (setq res (plist-put res :json
                             (let ((json-object-type 'plist)
                                   (json-array-type 'list))
                               (json-read-from-string body)))))
      res
      )))

(defun qiita:api-rate-limit ()
  (let ((response (qiita:api-exec "GET" "/rate_limit")))
    (qiita:response-body response)))

(defun qiita:api-auth (username password)
  (let ((response (qiita:api-exec "POST" "/auth" `(("url_name" . ,username)
                                                   ("password" . ,password)))))
    (plist-get (qiita:response-body response) :token)))

(defun qiita:api-user-items (username)
  (let ((response (qiita:api-exec "GET" (format "/users/%s/items" username))))
    (qiita:response-body response)))

(defun qiita:api-user-stocks (username)
  (let ((response (qiita:api-exec "GET" (format "/users/%s/stocks" username))))
    (qiita:response-body response)))

(defun qiita:api-tag-items (tag)
  (let ((response (qiita:api-exec "GET" (format "/tags/%s/items" tag))))
    (qiita:response-body response)))

(defun qiita:api-tags ()
  (let ((response (qiita:api-exec "GET" "/tags" '(("per_page" . 100)))))
    (qiita:response-body response)))

(defun qiita:api-search (q &optional stocked)
  (let ((args `(("q" . ,q)))
        response)
    (when qiita->token
      (add-to-list 'args `("stocked" . ,(if stocked "true" "false"))))
    (setq response (qiita:api-exec "GET" "/search" args))
    (qiita:response-body response)))

(defun qiita:api-items (&optional my)
  (let* ((qiita->token (if my qiita->token nil))
         (response (qiita:api-exec "GET" "/items")))
    (qiita:response-body response)))

(defun qiita:api-stocks ()
  (let ((response (qiita:api-exec "GET" "/stocks")))
    (when (<= 400 (qiita:response-status response))
      (error "Error: Can't get my stocks because %s"
             (plist-get (qiita:response-body response) :error)))
    (qiita:response-body response)))

(defun qiita:api-post-item (title body tags private &optional gist? tweet?)
  (let ((args `(("title"   . ,title)
                ("body"    . ,body)
                ("tags"    . ,tags)
                ("private" . ,private))))
    (when gist?    (add-to-list 'args '("gist"    . "true")))
    (when tweet?   (add-to-list 'args '("tweet"   . "true")))

    (let ((response (qiita:api-exec "POST" "/items" args)))
      (if (eq 201 (qiita:response-status response))
          (message "success")
        (error "Error: Can't create item because %s"
               (plist-get (qiita:response-body response) :error))))))

(defun qiita:api-put-item (uuid &optional title body tags private)
  (let (args '())
    (when title   (add-to-list 'args `("title"   . ,title)))
    (when body    (add-to-list 'args `("body"    . ,body)))
    (when tags    (add-to-list 'args `("tags"    . ,tags)))
    (when private (add-to-list 'args `("private" . ,private)))

    (let ((response (qiita:api-exec "PUT" (concat "/items/" uuid) args)))
      (if (eq 200 (qiita:response-status response))
          (message "success")
        (error "Error: Can't update item because (%s) %s"
               uuid (plist-get (qiita:response-body response) :error))))))

(defun qiita:api-delete-item (uuid)
  (let ((response (qiita:api-exec "DELETE" (format "/items/%s" uuid))))
    (if (eq 204 (qiita:response-status response))
        (message "success")
      (error "Error: Can't Delete item (%s). because %s"
             uuid (plist-get (qiita:response-body response) :error)))))

(defun qiita:api-get-item (uuid)
  (let ((response (qiita:api-exec "GET" (format "/items/%s" uuid))))
    (qiita:response-body response)))

(defun qiita:api-stock-item (uuid)
  (let ((response (qiita:api-exec "PUT" (format "/items/%s/stock" uuid))))
    (if (eq 204 (qiita:response-status response))
        (message "success")
      (error "Error: Can't stock item (%s). because %s"
             uuid (plist-get (qiita:response-body response) :error)))))

(defun qiita:api-unstock-item (uuid)
  (let ((response (qiita:api-exec "DELETE" (format "/items/%s/stock" uuid))))
    (if (eq 204 (qiita:response-status response))
        (message "success")
      (error "Error: Can't unstock item (%s). because %s"
             uuid (plist-get (qiita:response-body response) :error)))))

(defun qiita:api-user-info (username)
  "指定したユーザーの情報を取得します。"
  ;; pending
  )

;;;
;;;
;;; Function for post item
;;;
;;;

(defun qiita:body-cut-title (&optional buffer)
  (when (null buffer) (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (goto-char (point-min))
    (let (title)
      (setq title (cond
                   ((re-search-forward markdown-regex-header-1-atx nil t)
                    (match-string-no-properties 2))
                   ((re-search-forward markdown-regex-header-1-setext nil t)
                    (match-string-no-properties 1))
                   (t (error "Can't find header-1"))))
      (delete-region (match-beginning 0) (match-end 0))
      title)))

(defun qiita:body-cut-tags (&optional buffer)
  (when (null buffer) (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (goto-char (point-min))
    (let (tags)
      (unless (re-search-forward "^<!-- tags \\(.*\\) -->$" nil t)
        (error "Can't find tags"))
      (setq tags (match-string-no-properties 1))
      (delete-region (match-beginning 0) (match-end 0))
      (vconcat (mapcar (lambda (x) `((:name . ,x))) (split-string tags ","))))))

(defun qiita:body-cut-uuid (&optional buffer)
  (when (null buffer) (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (goto-char (point-min))
    (let (uuid)
      (when (re-search-forward "^<!-- uuid \\(.*\\) -->$" nil t)
        (setq uuid (match-string-no-properties 1))
        (delete-region (match-beginning 0) (match-end 0)))
      uuid)))

(defun qiita:show (uuid)
  (with-current-buffer (get-buffer-create qiita->show-buffer-name)
    (view-mode-disable)
    (erase-buffer)

    (let* ((response (qiita:api-get-item uuid))
           (title (plist-get response :title))
           (body  (plist-get response :raw_body))
           (uuid  (plist-get response :uuid))
           (tags  (mapconcat (lambda (tag)
                               (plist-get tag :name))
                             (plist-get response :tags) ",")))

      (insert "# " title "\n\n"
              "<!-- tags " tags " -->\n"
              "<!-- uuid " uuid " -->\n\n"
              body))
    (goto-char (point-min))
    (markdown-mode)
    (view-mode-enable))
  (switch-to-buffer qiita->show-buffer-name))

;;;
;;;
;;; Helm actions
;;;
;;;

(defun qiita:browse-item (uuid)
  (browse-url (concat "http://qiita.com/items/" uuid)))

(defun qiita:browse-tag (tag)
  (browse-url (concat "http://qiita.com/tags/" tag)))

(defun qiita:delete (uuid)
  (when (yes-or-no-p "Delete this item? ")
    (qiita:api-delete-item uuid)))

(defun qiita:tag-items (tag)
  (helm :sources
        `((name . ,(format "Qiita %s items" tag))
          (candidates . ,(lambda () (qiita:api-tag-items tag)))
          (type . qiita-items))))


;;;
;;;
;;; Helm sources
;;;
;;;

(defun qiita:transformer-items (items)
  (mapcar (lambda (item)
            (let ((title (plist-get item :title))
                  (uuid  (plist-get item :uuid))
                  (user  (plist-get (plist-get item :user) :url_name))
                  (tags  (mapconcat (lambda (tag)
                                      (concat "[" (plist-get tag :name) "]"))
                                    (plist-get item :tags) "")))
              (cons (concat tags  "\n"
                            title "\n"
                            "  by " user)
                    uuid)))
          items))


(defvar helm-c-qiita-items-source
  '((name   . "Qiita new activities")
    (type   . qiita-items)
    ))

(defvar helm-c-qiita-my-items-source
  '((name   . "Qiita my activities")
    (type   . qiita-items)
    (candidates . (lambda () (qiita:api-items t)))
    (action . (("Open Browser" . qiita:browse-item)
               ("Open"         . qiita:show)
               ("Stock"        . qiita:api-stock-item)
               ("Unstock"      . qiita:api-unstock-item)
               ("Delete" . qiita:delete)))
    ))

(define-helm-type-attribute 'qiita-items
  `((candidates . qiita:api-items)
    (candidate-number-limit . 100)
    (candidate-transformer qiita:transformer-items)
    (action . (("Open Browser" . qiita:browse-item)
               ("Open"         . qiita:show)
               ("Stock"        . qiita:api-stock-item)
               ("Unstock"      . qiita:api-unstock-item)
               ))
    (multiline)))


;;;
;;;
;;; User functions
;;;
;;;

;;;###autoload
(defun qiita:post (&optional private?)
  (interactive "P")
  (let ((mkdn (with-current-buffer (current-buffer)
                (buffer-substring-no-properties (point-min) (point-max)))))
    (with-temp-buffer
      (insert mkdn)
      (let ((title (qiita:body-cut-title))
            (tags  (qiita:body-cut-tags))
            (uuid  (qiita:body-cut-uuid))
            (body (buffer-substring-no-properties (point-min) (point-max)))
            (private (if (null private?) "true" "false")))
        (if uuid
            (when (yes-or-no-p "Update item? ")
              (qiita:api-put-item uuid title body tags private))
          (qiita:api-post-item title body tags private))))))

;;;###autoload
(defun qiita:stock ()
  (interactive)
  (let ((mkdn (with-current-buffer (current-buffer)
                (buffer-substring-no-properties (point-min) (point-max)))))
    (with-temp-buffer
      (insert mkdn)
      (let ((uuid  (qiita:body-cut-uuid)))
        (unless uuid
          (error "Can't find uuid"))
        (qiita:api-stock-item uuid)))))

;;;###autoload
(defun qiita:unstock ()
  (interactive)
  (let ((mkdn (with-current-buffer (current-buffer)
                (buffer-substring-no-properties (point-min) (point-max)))))
    (with-temp-buffer
      (insert mkdn)
      (let ((uuid  (qiita:body-cut-uuid)))
        (unless uuid
          (error "Can't find uuid"))
        (qiita:api-unstock-item uuid)))))

;;;###autoload
(defun qiita:items (&optional my)
  (interactive "P")
  (helm :sources (if my helm-c-qiita-my-items-source helm-c-qiita-items-source)))

;;;###autoload
(defun qiita:user-items (user)
  (interactive "sWho?: ")
  (helm :sources
        `((name . ,(format "Qiita %s items" user))
          (type . qiita-items)
          (candidates . ,(lambda () (qiita:api-user-items user))))))

;;;###autoload
(defun qiita:tags ()
  (interactive)
  (let (selected-tag)
    (helm :sources
          `((name . "Qiita tags")
            (candidates . ,(lambda ()
                             (mapcar
                              (lambda (tag)
                                (cons (plist-get tag :name)
                                      (plist-get tag :url_name)))
                              (qiita:api-tags))))
            (action . (("Open Browser" .
                        qiita:browse-tag)
                       ("Open tag items" .
                        ,(lambda (tag)
                           (setq selected-tag tag)))))))

    ;; helm の中で helm 起動が上手くいかなかったので妥協
    (when selected-tag
      (qiita:tag-items selected-tag))))

;;;###autoload
(defun qiita:my-stocks ()
  (interactive)
  (helm :sources
        `((name . "Qiita my stocks")
          (type . qiita-items)
          (candidates . qiita:api-stocks))))

;;;###autoload
(defun qiita:user-stocks (user)
  (interactive "sWho?: ")
  (helm :sources
        `((name . ,(format "Qiita %s stocks" user))
          (type . qiita-items)
          (candidates . ,(lambda () (qiita:api-user-stocks user))))))

;;;###autoload
(defun qiita:search (&optional stocked)
  (interactive "P")
  (let* ((keyword (read-from-minibuffer "Search: "))
         (stocked? (if stocked t nil))
         (items (qiita:api-search keyword stocked?)))
    (helm :sources
          `((name . "Qiita search activities")
            (candidates . items)
            (type . qiita-items)
            ))))

(provide 'qiita)

;;; qiita.el ends here
