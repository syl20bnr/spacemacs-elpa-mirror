;;; imgur.el --- imgur client for Emacs

;; Author: myuhe
;; URL: 
;; Package-Version: 20120307.225
;; Version: 0.1
;; Created: 2012-03-07
;; Keywords:  multimedia, convenience
;; Package-Requires: ((anything "1.287"))
;; Copyright (C) 2012 myuhe

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (a your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; imgur.el is imgur client for Emacs, enables image upload to imgur.com.
;; imgur.el depend on Curl, so you should install Curl at first.

;;; Changelog:

(eval-when-compile 
  (require 'cl))
(require 'anything)
(require 'json)
(require 'dired)
(require 'image-dired)

(defgroup imgur nil
  "imgur client , enables image upload to imgur.com"
  :prefix "imgur-"
  :group 'multimedia)

(defcustom imgur-keybinding "\C-c\C-u"
  "keybinding post image to imgur.com."
  :type 'string
  :group 'imgur)

(defvar imgur-url-alist nil)
(defvar imgur-proc-buffer-name " *imgur*")
(defvar imgur-err-msg "imgurCan't get valid callback !!")
(defvar anything-c-source-imgur
           '((name . "imgur")
             (candidates . imgur-anything-alist)
             ;;(multiline)
             (action
              ("Action" . anything-c-imgur-action))))

(defvar imgur-anything-alist
  '("Add original image URL to kill-ring"
    "Add HTML link to kill-ring"
    "Add Org-mode link to kill-ring"
    "Add large thumbnail URL to kill-ring"
    "Add small thumbnail URL to kill-ring"
    "Go to imgur.com in browser"
    "Go to delete page in browser"))

(defvar imgur-api-key "af00b1293fd886a94b1008dd1346b5c8")
(defvar imgur-api-url "http://api.imgur.com/2/upload.json")

(defun anything-c-imgur-action (candidates)
  (cond
   ((string= candidates (nth 0 imgur-anything-alist))
    (kill-new 
     (cdr (assq 'original imgur-url-alist)))
    (message "Added URL to kill-ring"))
   ((string= candidates (nth 1 imgur-anything-alist))
    (kill-new (concat
               "<img src=\""
               (cdr (assq 'original imgur-url-alist))
               "\""))
    (message "Added HTML link to kill-ring"))
   ((string= candidates (nth 2 imgur-anything-alist))
    (kill-new (concat
               "[["
               (cdr (assq 'original imgur-url-alist))
               "]]"))
    (message "Added Org link to kill-ring"))
   ((string= candidates (nth 3 imgur-anything-alist))
    (kill-new 
     (cdr (assq 'large_thumbnail imgur-url-alist)))
    (message "Added URL to kill-ring"))
   ((string= candidates (nth 4 imgur-anything-alist))
    (kill-new 
     (cdr (assq 'small_square imgur-url-alist)))
    (message "Added URL to kill-ring"))
   ((string= candidates (nth 5 imgur-anything-alist))
    (browse-url
     (cdr (assq 'imgur_page imgur-url-alist))))
   ((string= candidates (nth 6 imgur-anything-alist))
    (browse-url
     (cdr (assq 'delete_page imgur-url-alist))))
   (t
    (error imgur-err-msg))))

;;;###autoload
(defun imgur-post-image (&optional filename)
   (let((file (if filename filename
                (expand-file-name
                (read-file-name "Upload file: " nil buffer-file-name t
                         (file-name-nondirectory buffer-file-name))))))
  (set-process-sentinel
   (start-process "imgur" imgur-proc-buffer-name "curl" 
                  "-F" (concat "image=@" file)
                  "-F" (concat "key=" imgur-api-key) 
                  imgur-api-url)
   (lambda (process event) 
     (with-current-buffer imgur-proc-buffer-name
       (goto-char (point-min))
       (if (re-search-forward "{\"upload.*}}}" nil t)
           (setq imgur-url-alist
                 (cdadar
                  (json-read-from-string
                   (buffer-substring
                    (match-beginning 0)
                    (match-end 0)))))
         (error imgur-err-msg))
       (erase-buffer))
     (anything-other-buffer 
      'anything-c-source-imgur "*anything imgur*")))))

;;;###autoload
(defun imgur-post ()
  (interactive)
  (cond
   ((eq major-mode 'dired-mode)
    (imgur-post-image
     (expand-file-name (dired-file-name-at-point))))
   ((eq major-mode 'image-dired-thumbnail-mode)
    (imgur-post-image
     (expand-file-name (image-dired-original-file-name))))
   (t
    (imgur-post-image))))

(define-key image-dired-thumbnail-mode-map imgur-keybinding  'imgur-post)
(define-key dired-mode-map imgur-keybinding  'imgur-post)

(provide 'imgur)

;;; imgur.el ends here
