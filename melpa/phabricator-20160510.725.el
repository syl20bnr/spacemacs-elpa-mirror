;;; phabricator.el --- Phabricator/Arcanist helpers for Emacs.

;; Author: Andrew Tulloch
;; URL: https://github.com/ajtulloch/phabricator.el
;; Package-Version: 20160510.725
;; Version: 0.1
;; Created: 2014-09-11
;; Keywords: phabricator, arcanist, diffusion
;; Package-Requires: ((emacs "24.4") (dash "1.0") (projectile "0.13.0") (s "1.10.0") (f "0.17.2"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; A bunch of helpers for Phabricator and arc

;;; License:

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(require 'dash)
(require 'f)
(require 'ido)
(require 'image)
(require 'json)
(require 'projectile)
(require 's)

(defcustom arc-binary "arc"
  "*Location of the arcanist binary."
  :group 'phabricator)

(defcustom phabricator-macro-dir "~/.arcmacros/"
  "*Location for the arcanist macro images to be saved."
  :group 'phabricator)

(defcustom phabricator-macro-list "~/.arcmacros_list"
  "*Location for the list of arcanist macros."
  :group 'phabricator)

(defcustom diffusion-repo-prefix-list '()
  "*Mapping from repository name to path in diffusion."
  :group 'phabricator)

(defun arc--call-conduit (method req)
  "Call conduit METHOD with the parameters REQ."
  (with-temp-buffer
    (let ((tmp-file (make-temp-file "arc-conduit")))
      (with-temp-file tmp-file
        (->> req json-encode insert))
      (call-process
       arc-binary tmp-file `(,(current-buffer) nil) nil "call-conduit" method)
      (->> (buffer-string) json-read-from-string (assoc 'response) cdr))))

;;; ----------------------------------------------------------------------------
;;; arc macro commands.
(defun arc--list-macros ()
  "Load phabricator-macro-list into an ido completion window."
  (with-temp-buffer
    (insert-file-contents phabricator-macro-list)
    (->> (buffer-string) split-string (ido-completing-read "Macro: "))))

(defun arc--write-string-to-file (filename string)
  "Write FILENAME with contents of STRING."
  (write-region string nil filename))

;;;###autoload
(defun arc-refresh-macros ()
  "Refresh phabricator-macro-list from Phabricator with the latest macros."
  (interactive)
  (->> (arc--call-conduit "macro.query" `(:nameLike ""))
       (-map '-first-item)
       (-map 'symbol-name)
       (-sort 'string<)
       (s-join "\n")
       message
       (arc--write-string-to-file phabricator-macro-list)))

;;;###autoload
(defun arc-macro (name)
  "Insert NAME from a list of Phabricator macros."
  (interactive  `(,(arc--list-macros)))
  (->> name insert))

;;;###autoload
(defun arc-paste (start end)
  "Pastes the specified region from START to END (or whole file) with arcanist.
The resulting URL is stored in the kill
ring and messaged in the minibuffer."
  (interactive (if (use-region-p) `(,(region-beginning) ,(region-end))
                 `(,(point-min) ,(point-max))))
  (let* ((extract-uri (lambda (output) (->> output (assoc 'uri) cdr))))
    (->> (arc--call-conduit
          "paste.create"
          `(:title ,(when (buffer-file-name)
                      (file-name-nondirectory (buffer-file-name)))
            :content ,(buffer-substring start end)))
      (funcall extract-uri) message kill-new)))

;;; ----------------------------------------------------------------------------
;;; arc paste commands.

;;;###autoload
(defun arc-insert-macro (name)
  "Insert image NAME from a list of Phabricator macros."
  (interactive  `(,(arc--list-macros)))
  (let ((img (->> name arc-get-macro create-image)))
    (when (image-multi-frame-p img)
      (image-animate img 0 t))
    (insert-image img name)
    (insert "\n\n")))

;;;###autoload
(defun arc-get-macro (macro-name)
  "Retrieve the given MACRO-NAME image and save it to *phabricator-macro-dir*."
  (interactive  `(,(arc--list-macros)))
  (let* ((macro-file-name (format "%s/%s" phabricator-macro-dir macro-name))
         (download-uri
          (lambda (uri)
            (when (not (f-directory? phabricator-macro-dir))
              (f-mkdir phabricator-macro-dir))
            (when (not (f-exists? macro-file-name))
              (url-copy-file uri macro-file-name))
            macro-file-name))
         (extract-uri
          (lambda (output)
            (->> output (assoc (intern macro-name)) cdr (assoc 'uri) cdr))))
    (->> (arc--call-conduit "macro.query" `(:names (,macro-name)))
      (funcall extract-uri) (funcall download-uri) message)))

;;; ----------------------------------------------------------------------------
;;; arc inlines commands

;;;###autoload
(defun arc-inlines ()
  "Display the inlines for the current branch in a compilation buffer."
  (interactive)
  (let ((previous-dir default-directory))
    (unwind-protect
        (save-excursion
          (cd (projectile-project-root))
          (compile (format "%s inlines" arc-binary)))
      (cd previous-dir))))

;;; ----------------------------------------------------------------------------
;;; arc browse commands
(defun arc--repo-prefix ()
  "So repo/, repo_contbuild/ should still map to repo."
  (->> (-first
        (lambda (p) (s-starts-with? (car p) (projectile-project-name)))
        diffusion-repo-prefix-list)
    cdr))

(defun arc--project-relative-name (filename)
  "Find the true relative name of FILENAME to the project root."
  (file-relative-name (file-truename filename)
                      (file-truename (projectile-project-root))))

(defun arc-browse (start end)
  "Paste the specified region from START to END (or current line).
The resulting URL is stored in the kill ring and messaged in the
minibuffer."
  (interactive (if (use-region-p)
                   (mapc 'line-number-at-pos
                         `(,(region-beginning) ,(region-end)))
                 `(,(line-number-at-pos) ,(line-number-at-pos))))
  (when (not (arc--repo-prefix))
    (error "Not in a known Diffusion repository"))
  (let* ((url (format "%s/%s$%s-%s"
                      (arc--repo-prefix)
                      (arc--project-relative-name buffer-file-name)
                      start end)))
    (->> url message kill-new)))

(provide 'phabricator)
;;; phabricator.el ends here
