;;; obfusurl.el --- Obfuscate URLs so they aren't spoilers
;; Copyright 2001-2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 2.0
;; Package-Version: 20170325.802
;; Keywords: convenience, web, text
;; URL: https://github.com/davep/obfusurl.el
;; Package-Requires: ((cl-lib "0.5"))

;; obfusurl.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2. For details see the file COPYING.

;;; Commentary:
;;
;; obfusurl.el provides `obfusurl', a command that will obfuscate an
;; URL under the cursor. This might be useful if you are writing out an URL
;; for someone but the URL itself might spoil the surprise.
;;
;; For example, this:
;;
;; <URL:http://www.davep.org/emacs/>
;;
;; is turned into this:
;;
;; <URL:http://www.davep.org/%65%6d%61%63%73/>
;;
;; The latest obfusurl.el is always available from:
;;
;;   <URL:https://github.com/davep/obfusurl.el>
;;   <URL:https://github.com/%64%61%76%65%70/%6f%62%66%75%73%75%72%6c%2e%65%6c>

;;; THANKS:
;;
;; Andy Sawyer <andys@morebhp.com> for initially pointing out that URLs with
;; percent escapes already in them would get broken.
;;
;; Kevin Rodgers <kevinr@ihs.com> for suggesting a method of fixing the
;; above.
;;
;; Toby Speight <streapadair@gmx.net> for pointing out that I needed to
;; cater for reserved characters.

;;; Code:

;; Things we need:

(eval-when-compile
  (require 'cl-lib))
(require 'thingatpt)

;; Constants.

(defconst obfusurl-reserved-chars '(?\; ?/ ?? ?: ?@ ?& ?= ?+ ?$ ?,)
  "Characters reserved by RFC 2396.")

;; Main code.

(defun obfusurl-hexify-string (string)
  "Return STRING as percent-escaped hex values.

Existing percent-escapes and reserved characters (as defined in RFC 2396) in
the text are preserved."
  (cl-flet ((hexify-string (string)
              (with-output-to-string
                (mapc (lambda (c)
                        (princ (format
                                (if (member c obfusurl-reserved-chars)
                                    "%c"
                                  "%%%02x")
                                c))) string))))
    (let ((case-fold-search t))
      (with-output-to-string
        (cl-loop for i = 0 then (match-end 0)
           while (string-match "%[0-9a-f][0-9a-f]" string i)
           do (princ
               (concat (hexify-string (substring string i (match-beginning 0)))
                       (match-string 0 string)))
           finally (princ (hexify-string (substring string i))))))))

(defun obfusurl-hexify-url (url)
  "Return URL as a percent-escaped URL."
  (let ((trailing-slash (string-match "/$" url))
        (split          (split-string url "/")))
    (with-output-to-string
      (princ (format "%s//%s" (nth 0 split) (nth 2 split)))
      (loop for part in (nthcdr 3 split)
            unless (string= part "")    ; Because of XEmacs' `split-string'.
            do (princ (concat "/" (obfusurl-hexify-string part)))
            finally (when trailing-slash (princ "/"))))))

;;;###autoload
(defun obfusurl ()
  "Obfuscate an URL under `point'.

This might be useful if you're writing out an URL for someone but the URL
itself is a spoiler. The URL will still work but it won't be readable (by
most mortals anyway)."
  (interactive "*")
  (let ((url (thing-at-point 'url)))
    (if url
        (let ((bounds (bounds-of-thing-at-point 'url)))
          (setf (point) (car bounds))
          (delete-region (car bounds) (cdr bounds))
          (insert (obfusurl-hexify-url url)))
      (error "I can't see an URL here"))))

(provide 'obfusurl)

;;; obfusurl.el ends here
