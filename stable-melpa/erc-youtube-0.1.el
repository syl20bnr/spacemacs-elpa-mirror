;;; erc-youtube.el --- Show info about a YouTube URL in an ERC buffer.

;; Copyright (C) 2014  Raimon Grau Cuscó

;; Author: Raimon Grau Cuscó <raimonster@gmail.com>
;; Version: 0.1
;; Package-Version: 0.1
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
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
;;
;; Show info about a YouTube URL in an ERC buffer.
;; Info is currently just the title.
;; Requires Emacs 24.2.
;;
;; Warning! This version requires the user to define their own
;; erc-youtube-apiv3-key. This won't work without it!
;;
;;; Setup:
;;
;; (require 'erc-youtube)
;; (add-to-list 'erc-modules 'youtube)
;; (erc-update-modules)
;; (setq erc-youtube-apiv3-key "your-own-api-key-your-own-api-key")
;;
;; Or `(require 'erc-youtube)` and  `M-x customize-option erc-modules RET`
;;
;;
;;; Details:
;;
;; This plugin subscribes to hooks `erc-insert-modify-hook' and
;; `erc-send-modify-hook' to download and show youtubes.  In this early
;; version it's doing this synchronously.
;;
;;; Code:


(require 'erc)
(require 'json)
(require 'url-queue)

(defgroup erc-youtube nil
  "Enable youtube."
  :group 'erc)

(defvar erc-youtube-apiv3-key nil
  "An API key must be obtained by creating a Google Account. Use the
Google Developers Console to create your own Client API key.")

(defconst erc-youtube-regex-extract-videoid
  (concat
   ;; Slurp the possible youtube domains and any other parameters
   ;; before the Video ID, to ignore it.
   "^\\(?:https?:\\/\\/\\)?\\(?:www\\.\\)?"
   "\\(?:youtu\\.be\\/\\|youtube\\.com\\/\\(?:embed\\/\\|v\\/\\|watch\\?v=\\|watch\\?.+&v=\\)\\)"
   ;; Match the Video ID which is currently 11 characters of [-_A-Za-z0-9]
   ;; and save it as the first match group.
   "\\(?1:[-_A-Za-z0-9]\\{11\\}\\)"
   ;; Slurp up the rest of the url to ignore it
   "\\(?:[^-_A-Za-z0-9]?.*\\)$"
   )
  "Emacs 24.3 style regexp to extract the Video ID of a Youtube URL.

This regexp is used internally to check and extract the url from
a IRC buffer and to make API request URLs.

A Youtube URL has many patterns, including http://youtu.be/<video:id> and
https://....youtube.com/...?v=<video:id>.

The Video ID is currently defined as a 11 digit string of
alphanumeric characters, hyphens and underscores. The matched
Video ID can be referenced as the first regexp group result.

This regexp is based on the javascript regexp provided by user
eyecatchup from Stackoverflow.com. Greetz and howdy.
http://stackoverflow.com/a/10315969/28524
http://stackoverflow.com/users/624466/eyecatchup")

(defcustom erc-youtube-regex erc-youtube-regex-extract-videoid
  "Regex for matching Youtube videos URLs and extracting the Video ID"
  :group 'erc-youtube
  :type '(regexp :tag "Regex"))

(defun erc-youtube (status marker)
  (interactive)
  (let* ((video-title (erc-youtube--extract-title-from-response)))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (let ((inhibit-read-only t))
          (goto-char (marker-position marker))
          (let ((pt-before (point)))
            (insert-before-markers
             (with-temp-buffer
               (insert "[youtube] -  " video-title "\n")
               (buffer-string)))
            (put-text-property pt-before (point) 'read-only t)))))))

(defun erc-youtube--extract-title-from-response ()
  "While inside an arbitrary `url-retrieve' buffer, extract the video title.
Based on APIv3 specs."
  (let (pt-before json-raw title-packed)
    ;; delete HTTP headers
    (goto-char (point-min))
	(push-mark)
    (search-forward "\n\n")
    (kill-region (mark) (point))
    ;; slurp the HTTP content
    (set-buffer-multibyte t)
    (setq pt-before (point))
    (goto-char (point-max))
    ;; ... as JSON
    (setq json-raw (string-as-multibyte (buffer-substring-no-properties pt-before (point))))
    (setq title-packed (json-read-from-string json-raw))
    ;; convert to Emacs Lisp data and extract title
    (erc-youtube--extract-title-from-packed title-packed)))

(defun erc-youtube--extract-title-from-packed (packed)
  "Retrieve the value of PACKED with key items/snippet/title."
  (cdr (assoc 'title
              (assoc 'snippet
                     (elt (cdr (assoc 'items packed)) 0)))))

(defun erc-youtube-id (url)
  "Extract and return the Youtube Video ID from the string URL."
  (replace-regexp-in-string erc-youtube-regex-extract-videoid "\\1" url))

(defun erc-youtube-make-request-url (input-url)
  "Make request url for YouTube Date API v3 from INPUT-URL.

Returns nil if `erc-youtube-apiv3-key' is nil."
  (let* ((base-url "https://www.googleapis.com/youtube/v3/videos")
         (fields "items%2Fsnippet%2Ftitle")
         (apikey erc-youtube-apiv3-key)
         (id (erc-youtube-id input-url)))
    (when apikey
      (format "%s?part=snippet&fields=%s&key=%s&id=%s"
              base-url fields apikey id))))

(defun erc-youtube-show-info ()
  (interactive)
  (goto-char (point-min))
  (search-forward "http" nil t)
  (let ((url (thing-at-point 'url))
        request-url)
    (when (and url (string-match erc-youtube-regex url))
      (goto-char (point-max))
      (setq request-url (erc-youtube-make-request-url url))
      (if request-url
          (url-queue-retrieve
           request-url 'erc-youtube (list (point-marker)) t)
        (message "Cannot obtain title from YouTube. Please define `erc-youtube-apiv3-key''")))))


;;;###autoload
(eval-after-load 'erc
  '(define-erc-module youtube nil
     "Display inlined info about youtube links in ERC buffer"
     ((add-hook 'erc-insert-modify-hook 'erc-youtube-show-info t)
      (add-hook 'erc-send-modify-hook 'erc-youtube-show-info t))
     ((remove-hook 'erc-insert-modify-hook 'erc-youtube-show-info)
      (remove-hook 'erc-send-modify-hook 'erc-youtube-show-info))
     t))

(provide 'erc-youtube)

;;; erc-youtube.el ends here
