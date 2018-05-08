;;; pinboard-popular.el --- Displays links from the pinboard.in popular page.
;; -*- lexical-binding: t; -*-

;; Adam Simpson <adam@adamsimpson.net>
;; Version: 0.1.0
;; Package-Version: 0.1.0
;; Package-Requires: ((ivy "0.9.0"))
;; Keywords: pinboard, ivy
;; URL: https://github.com/asimpson/ivy-pinboard-popular

;;; Commentary:
;; I'm not great at regexes so this code is designed around that limitation.
;; The resulting plist may seem oddly structured but ivy-read uses the first element of each element as the displayed title.
;; So the strucuture is '(title-for-ivy :title title :url url).  A quick cdr returns a "proper" plist.

;;; Code:
(require 'ivy)
(require 'url)

(defun pinboard-popular--re-capture-between(re-start re-end)
  "Return the string between two regexes."
  (let (start end)
    (setq start (re-search-forward re-start))
    (setq end (re-search-forward re-end))
    (buffer-substring-no-properties start end)))

;;;###autoload
(defun pinboard-popular()
  "Download and parse the pinboard.in/popular page. Yes, I'm bad at regex, but this works...for now."
  (interactive)
  (let ((url "https://pinboard.in/popular/"))
    (url-retrieve url (lambda(_)
                        (let (links link title)
                          (keep-lines "bookmark_title" (point-min) (point-max))
                          (loop-for-each-line (progn
                                                (unless (= (point) (point-max))
                                                  (setq link (substring (pinboard-popular--re-capture-between "href=\"" "\"") 0 -1))
                                                  (setq title (decode-coding-string (substring (pinboard-popular--re-capture-between ">" "<") 0 -1) 'utf-8))
                                                  (push (list title :title title :url link) links))))

                          (ivy-read "Pinboard popular:"
                                    (reverse links)
                                    :action (lambda(x)
                                              (browse-url (plist-get (cdr x) :url)))))))))

(provide 'pinboard-popular)

;;; pinboard-popular.el ends here
