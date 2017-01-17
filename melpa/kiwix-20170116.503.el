;;; kiwix.el --- Kiwix interface and support.
;;; -*- coding: utf-8 -*-

;; Author: stardiviner <numbchild@gmail.com>
;; Maintainer: stardiviner <numbchild@gmail.com>
;; Keywords: kiwix wikipedia
;; Package-Version: 20170116.503
;; URL: https://github.com/stardiviner/kiwix.el
;; Created: 23th July 2016
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))

;;; Commentary:

;;; This currently only works for Linux, not tested for Mac OS X and Windows.

;;; Kiwix installation
;;
;; http://www.kiwix.org

;;; Config:
;;
;; (define-key my-prog-help-document-map (kbd "w") 'kiwix-at-point)
;; (define-key my-prog-help-document-map (kbd "W") 'kiwix-at-point-interactive)
;; (define-key my-prog-help-document-map (kbd "C-w") 'kiwix-launch-server)


;;; Usage:
;;
;; [M-x kiwix-launch-server] to launch Kiwix server.
;; [M-x kiwix-at-point] to search the word under point or the region selected string.

;;; Code:


(require 'cl-lib)
;; load for `org-link-set-parameters'
(require 'org)
(declare-function 'org-link-set-parameters "org")

(defgroup kiwix nil
  "Kiwix customization options."
  :group 'kiwix)

(defcustom kiwix-server-url "http://127.0.0.1:8000/"
  "Specify Kiwix server URL."
  :type 'string
  :group 'kiwix)

(defcustom kiwix-server-command
  (cond
   ((string-equal system-type "gnu/linux")
    "/usr/lib/kiwix/bin/kiwix-serve ")
   ((string-equal system-type "darwin")
    (warn "You need to specify Mac OS X Kiwix path. And send a PR to my repo."))
   ((string-equal system-type "windows-nt")
    (warn "You need to specify Windows Kiwix path. And send a PR to my repo.")))
  "Specify kiwix server command."
  :type 'string
  :group 'kiwix)

(defcustom kiwix-default-data-profile-name
  (car (directory-files
        (concat
         (getenv "HOME") "/.www.kiwix.org/kiwix")
        nil
        ".*\\.default"
        ))
  "Specify the default Kiwix data profile path."
  :type 'string
  :group 'kiwix)

(defcustom kiwix-default-data-path
  (concat
   (getenv "HOME") "/.www.kiwix.org/kiwix/" kiwix-default-data-profile-name)
  "Specify the default Kiwix data path."
  :type 'string
  :group 'kiwix)

(defcustom kiwix-server-port "8000"
  "Specify the default Kiwix server port."
  :type 'string
  :group 'kiwix)

;;;###autoload
(defcustom kiwix-support-org-mode-link t
  "Add support for Org-mode Kiwix link."
  :type 'boolean
  :group 'kiwix)

(defvar kiwix-libraries
  (mapcar #'(lambda (var)
              (replace-regexp-in-string "\.zim" "" var))
          (directory-files
           (concat kiwix-default-data-path "/data/content/") nil ".*\.zim"))
  "A list of Kiwix libraries.")

;; - examples:
;; - "wikipedia_en_all" - "wikipedia_en_all_2016-02"
;; - "wikipedia_zh_all" - "wikipedia_zh_all_2015-17"
;; - "wiktionary_en_all" - "wiktionary_en_all_2015-17"
;; - "wiktionary_zh_all" - "wiktionary_zh_all_2015-17"
;; - "wikipedia_en_medicine" - "wikipedia_en_medicine_2015-17"

(defun kiwix-construct-libraries-abbrev-alist (alist)
  "Construct libraries abbrev alist from `ALIST'."
  (let* ((libraries-name
          (mapcar #'(lambda (library)
                      (string-match "\\(.*\\)_[0-9]\\{4\\}-[0-9]\\{2\\}"  library)
                      (let* ((library-name (match-string 1 library)))
                        library-name))
                  alist))
         (libraries-full-name alist))
    (cl-pairlis libraries-name libraries-full-name)))

(defvar kiwix-libraries-abbrev-alist
  (kiwix-construct-libraries-abbrev-alist kiwix-libraries)
  "Alist of Kiwix libraries with name and full name.")

(defun kiwix-select-library-name ()
  "Select Wikipedia library name abbrev."
  (completing-read "Wikipedia library abbrev: "
                   (map-keys kiwix-libraries-abbrev-alist)))

(defun kiwix-get-library-fullname (abbr)
  "Get Kiwix library full name which is associated with `ABBR'."
  (cdr (assoc abbr kiwix-libraries-abbrev-alist)))

(defcustom kiwix-default-library "wikipedia_en_all"
  "The default kiwix library when library fragment in link not specified."
  :type 'string
  :group 'kiwix)

;; add default key-value pair to libraries alist.
(dolist
    (cons (list
           (cons "default" (kiwix-get-library-fullname kiwix-default-library))
           (cons "en" (kiwix-get-library-fullname kiwix-default-library))
           (cons "zh" (kiwix-get-library-fullname "wikipedia_zh_all"))))
  
  (push cons kiwix-libraries-abbrev-alist)
  )

(defcustom kiwix-your-language-library "zh"
  "Specify the library for your navtive language."
  :type 'string
  :group 'kiwix)

;; test
;; (kiwix-get-library-fullname "wikipedia_en")
;; (kiwix-get-library-fullname "default")
;; (kiwix-get-library-fullname "en")
;; (kiwix-get-library-fullname "zh")

(defcustom kiwix-search-interactively t
  "`kiwix-at-point' search interactively."
  :type 'boolean
  :group 'kiwix)

;; launch Kiwix server
;;;###autoload
(defun kiwix-launch-server ()
  "Launch Kiwix server."
  (interactive)
  
  (let ((library "--library ")
        (port (concat "--port=" kiwix-server-port " "))
        (daemon "--daemon ")
        (library-path (concat kiwix-default-data-path "/data/library/library.xml"))
        )
    (async-shell-command
     (concat kiwix-server-command library port daemon (shell-quote-argument library-path)))))

(defun kiwix-capitalize-first (string)
  "Only capitalize the first word of STRING."
  (concat
   (string (upcase (aref string 0)))
   (substring string 1))
  )

(defun kiwix-query (query &optional library)
  "Search `QUERY' in `LIBRARY' with Kiwix."
  (let* ((kiwix-library (if library
                            library
                          (kiwix-get-library-fullname "default")))
         (url (concat
               kiwix-server-url kiwix-library "/A/"
               ;; query need to be convert to URL encoding: "禅宗" https://zh.wikipedia.org/wiki/%E7%A6%85%E5%AE%97
               (url-encode-url
                ;; convert space to underline: "Beta distribution" "Beta_distribution"
                (replace-regexp-in-string
                 " " "_"
                 ;; only capitalize the first word. like: "meta-circular interpreter" -> "Meta-circular interpreter"
                 (kiwix-capitalize-first query)
                 nil nil))
               ".html")))
    (browse-url url)))

;;;###autoload
(defun kiwix-at-point (&optional interactively)
  "Search for the symbol at point with `kiwix-query'.

Or When prefix argument `INTERACTIVELY' specified, then prompt
for query string and library interactively."
  (interactive "P")
  (let* ((library (if (or kiwix-search-interactively
                          interactively)
                      (kiwix-get-library-fullname (kiwix-select-library-name))
                    (kiwix-get-library-fullname "default")))
         (query (if interactively
                    (read-string "Kiwix Search: "
                                 (if mark-active
                                     (buffer-substring
                                      (region-beginning) (region-end))
                                   (thing-at-point 'symbol)))
                  (progn
                    (if mark-active
                        (buffer-substring
                         (region-beginning) (region-end))
                      (thing-at-point 'symbol))))))
    (message (format "library: %s, query: %s" library query))
    (if (or (null library)
            (string-empty-p library)
            (null query)
            (string-empty-p query))
        (error "Your query is invalid")
      (kiwix-query query library))))


;;;###autoload
(defun kiwix-at-point-interactive ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively 'kiwix-at-point)))

;;; Support Org-mode
;;
;; - [[wikipedia:(library):query]]
;; - [[wikipedia:query]]
;;
;; links:
;; - wikipedia:(zh):%E7%A6%85%E5%AE%97
;; - wikipedia:(en):linux
;; - wikipedia:linux
;;
;; - parameter `link' will be (en):linux" or linux".
;;
;; elisp regexp: "\\(?:(\\(.*\\)):\\)?\\([^] \n\t\r]*\\)"
;; - non capturing group (\(?:...\)) for optional library
;; - group 1: library (en or zh)
;; - group 2: link? (match everything but ], space, tab, carriage return, linefeed by using [^] \n\t\r]*)
;; for open wiki search query with local application database.

(defun kiwix-org-get-library ()
  "Get library from Org-mode link."
  (if (string-match-p "[a-zA-Z\ ]+" (match-string 2 link)) ; validate query is English
      ;; convert between libraries full name and abbrev.
      (kiwix-get-library-fullname (or (match-string 1 link)
                                      "default"))
    ;; validate query is non-English
    (kiwix-get-library-fullname kiwix-your-language-library)
    )
  )

(defun org-wikipedia-link-open (link)
  "Open LINK in external Wikipedia program."
  ;; The regexp: (library):query
  ;; - query : should not exclude space
  (when (string-match "\\(?:(\\(.*\\)):\\)?\\([^]\n\t\r]*\\)"  link) ; (library):query
    (let* ((library (kiwix-org-get-library))
           (query (match-string 2 link))
           (url (concat
                 kiwix-server-url
                 library "/A/"
                 ;; query need to be convert to URL encoding: "禅宗" https://zh.wikipedia.org/wiki/%E7%A6%85%E5%AE%97
                 (url-encode-url
                  ;; convert space to underline: "Beta distribution" "Beta_distribution"
                  (replace-regexp-in-string
                   " " "_"
                   ;; only capitalize the first word. like: "meta-circular interpreter" -> "Meta-circular interpreter"
                   (kiwix-capitalize-first query)
                   nil nil))
                 ".html")))
      ;; (prin1 (format "library: %s, query: %s, url: %s" library query url))
      (browse-url url))))

(defun org-wikipedia-link-export (link description format)
  "Export the Wikipedia LINK with DESCRIPTION for FORMAT from Org files."
  (when (string-match "\\(?:(\\(.*\\)):\\)?\\([^] \n\t\r]*\\)" link)
    (let* ((library (kiwix-org-get-library))
           (query (url-encode-url (or (match-string 2 link) description)))
           ;; "http://en.wikipedia.org/wiki/Linux"
           ;;         --
           ;;          ^- library: en, zh
           (path (concat "http://" library ".wikipedia.org/wiki/" query))
           (desc (or (match-string 2 link) description)))
      (when (stringp path)
        (cond
         ((eq format 'html) (format "<a href=\"%s\">%s</a>" path desc))
         ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
         (t path))))))

(defun org-wikipedia-store-link ()
  "Store a link to a Wikipedia link."
  ;; [C-c o C-l l] `org-store-link'
  ;; remove those interactive functions. use normal function instead.
  (when (eq major-mode 'wiki-mode)
    (let* ((query (read-string "Wikipedia Query with Kiwix: "))
           (library (kiwix-select-library-name))
           (link (concat "wikipedia:" "(" library "):" query)))
      (org-store-link-props
       :type "wikipedia"
       :link link
       :description query))))

;;;###autoload
(with-eval-after-load "org"
  (if kiwix-support-org-mode-link
      (progn
        (org-link-set-parameters "wikipedia"
                                 :follow #'org-wikipedia-link-open
                                 :store #'org-wikipedia-store-link
                                 :export #'org-wikipedia-link-export)
        (add-hook 'org-store-link-functions 'org-wikipedia-store-link t)

        ;; [[Wikipedia_Local:]]
        ;; (if (and
        ;;      (member '("Wikipedia_Local" . "http://127.0.0.1:8000/wikipedia_zh_all_2015-11/A/%s.html") org-link-abbrev-alist)
        ;;      (assoc "Wikipedia_Local" org-link-abbrev-alist))
        ;;
        ;;     (setq org-link-abbrev-alist
        ;;           (cons '("Wikipedia_Local" . "http://127.0.0.1:8000/wikipedia_zh_all_2015-11/A/%s.html") org-link-abbrev-alist))
        ;;   )
        )))



(provide 'kiwix)

;;; kiwix.el ends here
