;;; helm-google.el --- Emacs Helm Interface for quick Google searches

;; Copyright (C) 2014-2018, Steckerhalter

;; Author: steckerhalter
;; Package-Requires: ((helm "0"))
;; Package-Version: 20180204.232
;; URL: https://github.com/steckerhalter/helm-google
;; Keywords: helm google search browse

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs Helm Interface for quick Google searches

;;; Code:

(require 'helm)
(require 'helm-net)

(defgroup helm-google '()
  "Customization group for `helm-google'."
  :link '(url-link "http://github.com/steckerhalter/helm-google")
  :group 'convenience
  :group 'comm)

(defcustom helm-google-search-function 'helm-google-html-search
  "The function that should be used to get the search results."
  :type 'symbol
  :group 'helm-google)

(defcustom helm-google-url "https://encrypted.google.com/search?ie=UTF-8&oe=UTF-8&q=%s"
  "The Google search url.
`%s' is where the search terms are inserted. `encrypted' is used
to avoid country-specific redirects. For country-specific
searches you will want to use `www.google.TLD'."
  :type 'string
  :group 'helm-google)

(defcustom helm-google-parsing-function 'helm-google--parse-w/regexp
  "Function to use to parse the html."
  :type 'symbol
  :group 'helm-google)

(defcustom helm-google-actions
  '(("Browse URL" . browse-url)
    ("Browse URL with EWW" . (lambda (candidate)
                               (eww-browse-url candidate)))
    ("Copy URL to clipboard" . (lambda (candidate)
                                 (kill-new  candidate))))
  "List of actions for helm-google sources."
  :group 'helm-google
  :type '(alist :key-type string :value-type function))


(defcustom helm-google-idle-delay 0.4
  "Time to wait when idle until query is made."
  :type 'integer
  :group 'helm-google)

(defvar helm-google-input-history nil)
(defvar helm-google-pending-query nil)

(defun helm-google--process-html (html)
  (replace-regexp-in-string
   "\n" ""
   (with-temp-buffer
     (insert html)
     (if (fboundp 'html2text)
         (html2text)
       (shr-render-region (point-min) (point-max)))
     (buffer-substring-no-properties (point-min) (point-max)))))

(defmacro helm-google--with-buffer (buf &rest body)
  (declare (doc-string 3) (indent 2))
  `(with-current-buffer ,buf
     (set-buffer-multibyte t)
     (goto-char url-http-end-of-headers)
     (prog1 ,@body
       (kill-buffer ,buf))))

(defun helm-google--parse-w/regexp (buf)
  (helm-google--with-buffer buf
      (let (results result)
        (while (re-search-forward "class=\"r\"><a href=\"/url\\?q=\\(.*?\\)&amp;sa" nil t)
          (setq result (plist-put result :url (match-string-no-properties 1)))
          (re-search-forward "\">\\(.*?\\)</a></h3>" nil t)
          (setq result (plist-put result :title (helm-google--process-html (match-string-no-properties 1))))
          (re-search-forward "class=\"st\">\\([\0-\377[:nonascii:]]*?\\)</span>" nil t)
          (setq result (plist-put result :content (helm-google--process-html (match-string-no-properties 1))))
          (add-to-list 'results result t)
          (setq result nil))
        results)))

(defun helm-google--parse (buf)
  "Extract the search results from BUF."
    (funcall helm-google-parsing-function buf))

(defun helm-google--response-buffer-from-search (text &optional search-url)
  (let ((url-mime-charset-string "utf-8")
        (url (format (or search-url helm-google-url) (url-hexify-string text))))
    (url-retrieve-synchronously url t)))

(defun helm-google--search (text)
  (let* ((buf (helm-google--response-buffer-from-search text))
         (results (helm-google--parse buf)))
    results))

(defun helm-google-html-search ()
  "Get Google results by scraping the website."
  (let* ((results (helm-google--search helm-pattern)))
    (mapcar (lambda (result)
              (let ((cite (plist-get result :cite)))
                (cons
                 (concat
                  (propertize
                   (plist-get result :title)
                   'face 'font-lock-variable-name-face)
                  "\n"
                  (plist-get result :content)
                  "\n"
                  (when cite
                    (concat
                     (propertize
                      cite
                      'face 'link)
                     "\n"))
                  (propertize
                   (url-unhex-string
                    (plist-get result :url))
                   'face (if cite 'glyphless-char 'link)))
                 (plist-get result :url))))
            results)))

(defun helm-google-search ()
  "Invoke the search function set by `helm-google-search-function'."
  (funcall helm-google-search-function))

(defvar helm-source-google
  `((name . "Google")
    (action . helm-google-actions)
    (candidates . helm-google-search)
    (requires-pattern)
    (nohighlight)
    (multiline)
    (match . identity)
    (volatile)))

;;;###autoload
(defun helm-google ( &optional arg)
  "Preconfigured `helm' : Google search."
  (interactive)
  (let ((region
         (if (not arg)
             (when (use-region-p)
               (buffer-substring-no-properties
                (region-beginning)
                (region-end)))
           arg)))
    (helm :sources 'helm-source-google
          :prompt "Google: "
          :input region
          :input-idle-delay helm-google-idle-delay
          :buffer "*helm google*"
          :history 'helm-google-input-history)))

(add-to-list 'helm-google-suggest-actions
             '("Helm-Google" . (lambda (candidate)
                                 (helm-google candidate))))

(provide 'helm-google)

;;; helm-google.el ends here
