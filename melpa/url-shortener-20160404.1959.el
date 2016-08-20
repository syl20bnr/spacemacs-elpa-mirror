;;; url-shortener.el --- shorten long url and expand tinyurl
;; Time-stamp: <2013-12-10 17:44:20 Tuesday by Yu Yang>
;;; Author: Yu Yang <yy2012cn@NOSPAM.gmail.com>
;;; URL: https://github.com/yuyang0/url-shortener
;; Package-Version: 20160404.1959
;;; Version: 0.3
;;;
;;; Commentary:
;; Usage:
;; this package provides commands to do url shorten and expand:
;;    M-x goo-url-shorten
;;    M-x goo-url-expand
;;    M-x bitly-url-shorten
;;    M-x bitly-url-expand
;;    M-x dwz-url-shorten
;;    M-x dwz-url-expand
;;    M-x 126am-url-shorten
;;    M-x 126am-url-expand
;;; Code:
(require 'json)
(defgroup bitly nil
  "The bitly URL shortening service."
  :prefix "bitly-"
  :group 'applications)

(defcustom bitly-access-token nil
  "The OAuth access token for bitly.

Get your personal token here: https://bitly.com/a/oauth_apps"
  :type 'string
  :group 'bitly)

(defvar bitly-base-url "https://api-ssl.bitly.com/v3/shorten")
(defvar bitly-shorten-api-url "https://api-ssl.bitly.com/v3/shorten")
(defvar bitly-expand-api-url  "https://api-ssl.bitly.com/v3/expand")

(defvar dwz-shorten-api-url "http://dwz.cn/create.php")
(defvar dwz-expand-api-url "http://dwz.cn/query.php")

(defgroup goo nil
  "The goo.gl URL shortening service."
  :prefix "goo-"
  :group 'applications)

(defcustom goo-api-key nil
  "The Api key for goo.gl.

Get your personal token here:
https://developers.google.com/url-shortener/v1/getting_started#APIKey"
  :type 'string
  :group 'goo)

(defvar goo-shorten-api-url "https://www.googleapis.com/urlshortener/v1/url")

(defgroup 126am nil
  "The 126.am URL shortening service."
  :prefix "126am-"
  :group 'applications)

(defcustom 126am-api-key nil
  "The Api key for 126.am.

Get your personal token here: http://126.am/apiManage.action"
  :type 'string
  :group '126am)

(defvar 126am-shorten-api-url "http://126.am/api!shorten.action")
(defvar 126am-expand-api-url "http://126.am/api!expand.action")

(defgroup tcn nil
  "The 126.am URL shortening service."
  :prefix "tcn-"
  :group 'application)

(defcustom tcn-app-key nil
  "The App key for t.cn.

Get your personal token here: http://open.weibo.com/apps/new"
  :type 'string
  :group 'tcn)

(defvar tcn-shorten-api-url "https://api.weibo.com/2/short_url/shorten.json")

(defun url-equal (url1 url2)
  (if (or (equal url1 url2)
          (equal url1 (concat url2 "/"))
          (equal url2 (concat url1 "/")))
      t
    nil))
(defun smart-insert-url-to-buffer (test-url url-need-insert)
  "If url at current point is equal to `test-url', replace the url at current
point with `url--need-insert', otherwise insert the `url-need-insert' to the buffer"
  (let ((current-point-url (thing-at-point 'url))
        (url-boundaries (bounds-of-thing-at-point 'url)))
    (if url-boundaries
        (if (url-equal test-url current-point-url)
            (progn
              (goto-char (car url-boundaries))
              (delete-region (car url-boundaries) (cdr url-boundaries))
              (insert url-need-insert))
          (progn
            (goto-char (cdr url-boundaries))
            (insert " " url-need-insert)))
      (insert "(" url-need-insert ")"))))

(defun my-url-http-post (url args callback cbargs &optional is-json)
  "Send ARGS to URL as a POST request."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         (if is-json
             '(("Content-Type" . "application/json"))
           '(("Content-Type" . "application/x-www-form-urlencoded"))))
        (url-request-data
         (if is-json
             (json-encode args)
           (mapconcat (lambda (arg)
                        (concat (url-hexify-string (car arg))
                                "="
                                (url-hexify-string (cdr arg))))
                      args
                      "&"))))
    ;; if you want, replace `my-switch-to-url-buffer' with `my-kill-url-buffer'
    (url-retrieve url callback cbargs)))
(defun my-url-http-get (url args callback cbargs)
  (let ((url-request-method "GET")
        (arg-stuff
         (mapconcat (lambda (arg)
                      (concat (url-hexify-string (car arg))
                              "="
                              (url-hexify-string (cdr arg))))
                    args
                    "&")))
    (url-retrieve (concat url "?" arg-stuff)
                  callback cbargs)))

(defun http-callback (status origin-buffer cb-fun &rest cb-fun-args)
  (let* ((json-buffer (current-buffer))
         (resp-json (with-current-buffer json-buffer
                      (goto-char (point-min))
                      (search-forward "\n\n" nil t)
                      (json-read))))
    (switch-to-buffer origin-buffer)
    (apply cb-fun resp-json cb-fun-args)))

(defun bitly-shorten (long-url)
  (defun bitly-shorten-callback (resp-json)
    (let* ((status-code (cdr (assq 'status_code resp-json))))
      (if (equal status-code 200)
          (let ((tinyurl (cdr (assq 'url
                                    (cdr (assq 'data resp-json)))))
                (longurl (cdr (assq 'long_url
                                    (cdr (assq 'data resp-json))))))
            (smart-insert-url-to-buffer longurl tinyurl))
        (error "Error %s calling bitly: %s"
               status-code
               (cdr (assq 'status_txt resp-json)))
        )))

  (my-url-http-get bitly-shorten-api-url `(,(cons "access_token" bitly-access-token)
                                           ,(cons "longUrl" long-url))
                   'http-callback `(,(current-buffer) bitly-shorten-callback)))

(defun bitly-expand (short-url)
  (defun bitly-expand-callback (resp-json)
    (let* ((data (cdr (assq 'data resp-json)))
           (expand-vec (cdr (assq 'expand data)))
           (tinyurl (cdr (assq 'short_url
                                (elt expand-vec 0))))
           (longurl (cdr (assq 'long_url
                               (elt expand-vec 0)))))
      (smart-insert-url-to-buffer tinyurl longurl)))
    (my-url-http-get bitly-expand-api-url `(,(cons "access_token" bitly-access-token)
                                           ,(cons "shortUrl" short-url))
                   'http-callback `(,(current-buffer) bitly-expand-callback)))

(defun tcn-shorten (long-url)
  (defun tcn-callback (resp-json)
    "process the json returned by http"
    (let* ((urls-vector (cdr (assq 'urls resp-json)))
           (i 0))
      (if urls-vector
          (while (< i (length urls-vector))
            (let* ((entry (elt urls-vector i))
                   (tinyurl (cdr (assq 'url_short entry)))
                   (longurl (cdr (assq 'url_long entry))))
              (smart-insert-url-to-buffer longurl tinyurl))
            (setq i (1+ i)))
        (error "Error %s calling t.cn: %s"
               (cdr (assq 'error_code resp-json))
               (cdr (assq 'error resp-json))))))
  (my-url-http-get tcn-shorten-api-url `(,(cons "source" tcn-app-key)
                                         ,(cons "url_long" long-url))
                   'http-callback `(,(current-buffer) tcn-callback)))

(defun dwz-shorten (long-url)
  "Return a shortened URL for LONG-URL."
  (defun dwz-shorten-callback (resp-json)
    (let* ((longurl (cdr (assq 'longurl resp-json)))
           (tinyurl (cdr (assq 'tinyurl resp-json)))
           (status-code (cdr (assq 'status resp-json))))
      (if (equal status-code 0)
          (smart-insert-url-to-buffer longurl tinyurl)
        (error "Error calling dwz.cn: %s"
               (cdr (assq 'err_msg resp-json))))))
  (my-url-http-post dwz-shorten-api-url `(,(cons "url" long-url)) 'http-callback
                    `(,(current-buffer) dwz-shorten-callback)))
(defun dwz-expand (short-url)
  (defun dwz-expand-callback (resp-json short-url)
    (let* ((longurl (cdr (assq 'longurl resp-json)))
           (tinyurl short-url)
           (status-code (cdr (assq 'status resp-json))))
      (if (equal status-code 0)
          (smart-insert-url-to-buffer tinyurl longurl)
        (error "Error calling dwz.cn: %s"
               (cdr (assq 'err_msg resp-json))))))
  (my-url-http-post dwz-expand-api-url `(,(cons "tinyurl" short-url)) 'http-callback
                    `(,(current-buffer) dwz-expand-callback ,short-url)))
(defun 126am-shorten (long-url)
  "Return a shortened URL for LONG-URL."
  (defun 126am-shorten-callback (resp-json)
    (let* ((longurl (cdr (assq 'longUrl resp-json)))
           (tinyurl (cdr (assq 'url resp-json)))
           (status-code (cdr (assq 'status_code resp-json))))
      (if (equal status-code 200)
          (smart-insert-url-to-buffer longurl tinyurl)
        (error "Error %s calling 126.am: %s"
               status-code
               (cdr (assq 'status_txt resp-json))))
      ))
  (my-url-http-post 126am-shorten-api-url
                    `(,(cons "longUrl" long-url) ,(cons "key" 126am-api-key))
                    'http-callback
                    `(,(current-buffer) 126am-shorten-callback)))
(defun 126am-expand (short-url)
  (defun 126am-expand-callback (resp-json short-url)
    (let* ((longurl (cdr (assq 'url resp-json)))
           (tinyurl short-url)
           (status-code (cdr (assq 'status_code resp-json))))
      (if (equal status-code 200)
          (smart-insert-url-to-buffer tinyurl longurl)
        (error "Error %s calling 126.am: %s"
               status-code
               (cdr (assq 'status_txt resp-json))))))
  (my-url-http-post 126am-expand-api-url
                    `(,(cons "shortUrl" short-url) ,(cons "key" 126am-api-key))
                    'http-callback
                    `(,(current-buffer) 126am-expand-callback ,short-url)))

(defun goo-shorten (long-url)
  "Return a shortened URL for LONG-URL."
  (defun goo-callback (resp-json)
    (let ((longurl (cdr (assq 'longUrl resp-json)))
           (tinyurl (cdr (assq 'id resp-json))))
      (smart-insert-url-to-buffer longurl tinyurl)))
  (let ((goo-shorten-api-full-url
         (if goo-api-key
             (format "%s?key=%s" goo-shorten-api-url goo-api-key)
           goo-shorten-api-url)))
    (my-url-http-post goo-shorten-api-full-url
                      `(,(cons "longUrl" long-url))
                      'http-callback
                      `(,(current-buffer) goo-callback) t)))
(defun goo-expand (short-url)
  "Return a expended URL for tiny url."
  (defun goo-callback (resp-json)
    (let ((longurl (cdr (assq 'longUrl resp-json)))
           (tinyurl (cdr (assq 'id resp-json))))
      (smart-insert-url-to-buffer tinyurl longurl)))
  (let ((args (if goo-api-key
                  `(,(cons "shortUrl" short-url)
                       ,(cons "key" goo-api-key))
                `(,(cons "shortUrl" short-url)))))
   (my-url-http-get goo-shorten-api-url
                     args
                     'http-callback
                     `(,(current-buffer) goo-callback))))

(defun  url-shorten-wrapper (shorten-fun)
  (let ((long-url (thing-at-point 'url)))
    (setq long-url (read-string (format "long url(default: %s)" long-url) nil nil long-url))
    (funcall shorten-fun long-url)))

;;;###autoload
(defun dwz-url-shorten ()
  (interactive)
  (url-shorten-wrapper 'dwz-shorten))

;;;###autoload
(defun dwz-url-expand ()
  (interactive)
  (url-shorten-wrapper 'dwz-expand))

;;;###autoload
(defun 126am-url-shorten ()
  (interactive)
  (url-shorten-wrapper '126am-shorten))

;;;###autoload
(defun 126am-url-expand ()
  (interactive)
  (url-shorten-wrapper '126am-expand))

;;;###autoload
(defun tcn-url-shorten ()
  (interactive)
  (url-shorten-wrapper 'tcn-shorten))

;;;###autoload
(defun bitly-url-shorten ()
  (interactive)
  (url-shorten-wrapper 'bitly-shorten))

;;;###autoload
(defun bitly-url-expand ()
  (interactive)
  (url-shorten-wrapper 'bitly-expand))

;;;###autoload
(defun goo-url-shorten ()
  (interactive)
  (url-shorten-wrapper 'goo-shorten))

;;;###autoload
(defun goo-url-expand ()
  (interactive)
  (url-shorten-wrapper 'goo-expand))

(provide 'url-shortener)
;;; url-shortener.el ends here
