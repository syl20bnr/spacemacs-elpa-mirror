;;; melpa-upstream-visit.el --- A set of kludges to visit a melpa-hosted package's homepage -*- lexical-binding: t -*-

;; Copyright (C) 2013  Alessandro Piras

;; Author: Alessandro Piras <laynor@gmail.com>
;; Keywords: convenience
;; Package-Version: 20130720.333
;; Version: 0.5
;; Package-Requires: ((s "1.6.0"))

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

;; This package provides an interactive command to `browse-url' a
;; melpa-hosted package's homepage.
;;
;; To (try to) visit a package's homepage, just
;;   M-x muv RET package-name RET
;; and the package's homepage (actually, its repo page) will be hopefully
;; opened in your browser.
;;
;; The list of kludges used to guess the package's homepage is stored
;; in `muv:url-kludges', a variable that you can customize with your
;; own functions.
;;
;; These functions are applied to a melpa recipe and return a string
;; containing the URL of the package - note that here applied refers
;; to `apply'.  See `muv::github-kludge' for an example.
;;
;; The kludges in the list are applied in order until one of them
;; returns non-nil. The first non-nil result is then interpreted
;; as the URL to be visited.
;;
;; Customization
;; -------------
;;
;; If `muv:enable-muv-button' is non nil - the default - a
;; Visit Homepage button will be show in the package description.
;; The button can be disabled by customizing `muv:enable-muv-button',
;; and can be customized via the customization variables
;; `muv:button-face' and `muv:button-label'.
;;
;; Thank yous:
;; -- milkypostman for melpa :-)
;; -- dgutov for thing-at-point integration



;;; Code:
(eval-when-compile
  (require 'cl))

(require 's)

(defgroup melpa-upstream-visit nil
  "A set of kludges to visit a melpa-installed package's homepage."
  :prefix "muv:"
  :group 'package)

(defface muv:button-face
  '((((type x w32 ns) (class color))
     :box (:line-width 2 :style released-button)
     :background "lightgrey" :foreground "black"))
  "the face used to fontify the 'Visit Homepage' button"
  :group 'melpa-upstream-visit)

(defcustom muv:button-label "Visit Homepage"
  "The Label of the 'Visit Homepage' button"
  :group 'melpa-upstream-visit
  :type 'string)

(defcustom muv:user-url-kludges nil
  "Recipe-to-homepage url translation functions, applied in order.
These functions will be tried before the default kludges."
  :group 'melpa-upstream-visit
  :type '(repeat function))

(defcustom muv:url-kludges '(muv::github-kludge
                             muv::wiki-kludge
                             muv::savannah-nongnu-git-kludge
                             muv::savannah-gnu-git-kludge
                             muv::google-code-hg-kludge
                             muv::google-code-kludge
                             muv::gitorious-kludge
                             muv::bitbucket-kludge
                             muv::launchpad-kludge
                             muv::sourceforge-svn-kludge
                             muv::sourceforge-git-kludge
                             muv::repo-or-cz-kludge
                             muv::naquadah-git-kludge
                             muv::jblevins-kludge
                             muv::ryuslash-kludge
                             muv::logilab-kludge
                             muv::joyful-kludge
                             muv::hub-darcs-kludge
                             muv::svn-common-kludge
                             muv::plain-url-kludge)
  "Default Recipe to homepage url translation functions, applied in order.
Unless you know what you are doing, you should not touch this list, but
customize muv:user-url-kludges instead."
  :group 'melpa-upstream-visit
  :type '(repeat function))

(defcustom muv:debug nil
  "Whether or not to print debug messages.
Set this to t if you are having problems and want to help to
solve them!"
  :group 'melpa-upstream-visit
  :type 'boolean)

(defcustom muv:button-location 'top-right
  "Whether or not to enable a 'Visit homepage' button in the
package description."
  :group 'melpa-upstream-visit
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Top-Right" top-right)
                 (const :tag "Make the package name a button" package-name)))

(defcustom muv:completing-read-function 'ido-completing-read
  "Function to be called when requesting input from the user."
  :group 'melpa-upstream-visit
  :type '(radio (function-item completing-read)
                (function :tag "Other")))


;;; Recipe -> URL kludges

(defun muv::as-string (symbol-or-string)
  (etypecase symbol-or-string
      (symbol (symbol-name symbol-or-string))
      (string symbol-or-string)))

(defun muv::compare-fetcher (fetcher1 fetcher2)
  (equal (muv::as-string fetcher1) (muv::as-string fetcher2)))

(defun* muv::github-kludge (_package-name &key fetcher repo &allow-other-keys)
  (and (muv::compare-fetcher fetcher 'github) (format "https://github.com/%s" repo)))

(defun* muv::wiki-kludge (package-name &key fetcher &allow-other-keys)
  (and (muv::compare-fetcher fetcher 'wiki) (format "http://www.emacswiki.org/%s.el" package-name)))

(defun* muv::savannah-nongnu-git-kludge (_package-name &key fetcher url &allow-other-keys)
  (when (muv::compare-fetcher fetcher 'git)
    (let ((matches (s-match "savannah\\.nongnu\\.org/\\([^/]+\\)\\.git" url)))
      (and matches (format "http://savannah.nongnu.org/projects/%s/" (second matches))))))

(defun* muv::savannah-gnu-git-kludge (_package-name &key fetcher url &allow-other-keys)
  (when (muv::compare-fetcher fetcher 'git)
    (let ((matches (s-match "git\\.\\(sv\\|savannah\\)\\.gnu\\.org/\\([^/]+\\)\\.git" url)))
      (and matches (format "http://savannah.gnu.org/projects/%s/" (third matches))))))

(defun* muv::savannah-gnu-bzr-kludge (_package-name &key fetcher url &allow-other-keys)
  (when (muv::compare-fetcher fetcher 'bzr)
    (let ((matches (s-match "bzr\\.\\(sv\\|savannah\\)\\.gnu\\.org/r/\\([^/]+\\)/" url)))
      (and matches (format "http://savannah.gnu.org/projects/%s/" (third matches))))))

(defun* muv::naquadah-git-kludge (_package-name &key url &allow-other-keys)
  (let ((matches (s-match "git://git\\.naquadah\\.org/\\([^/]+\\.git\\)" url)))
    (and matches (format "http://git.naquadah.org/?p=%s;a=summary" (second matches)))))

(defun* muv::google-code-hg-kludge (_package-name &key _fetcher url &allow-other-keys)
  (let ((matches (s-match "^https?://code\\.google\\.com/p/[^/]+/" url)))
    (first matches)))

(defun* muv::google-code-kludge (_package-name &key url &allow-other-keys)
  (let ((matches (s-match "^https?://[^/]+\\.googlecode\\.com/" url)))
    (first matches)))

(defun* muv::gitorious-kludge (_package-name &key url &allow-other-keys)
  (let ((matches (s-match "gitorious\\.org/[^/]+/[^\\.]+" url)))
    (and matches (format "https://%s" (first matches)))))

(defun* muv::bitbucket-kludge (_package-name &key url &allow-other-keys)
  (let ((matches (s-match "bitbucket\\.org/[^/]+/[^/\\?]+" url)))
    (and matches (format "https://%s" (first matches)))))

(defun* muv::launchpad-kludge (_package-name &key url &allow-other-keys)
  (and (s-starts-with-p "lp:" url)
       (s-replace "lp:" "https://launchpad.net/" url)))

(defun* muv::repo-or-cz-kludge (_package-name &key url &allow-other-keys)
  (let ((matches (s-match "repo\\.or\\.cz/r/\\([^/\\.]+\\.git\\)" url)))
    (and matches (format "http://repo.or.cz/w/%s" (second matches)))))

(defun* muv::sourceforge-svn-kludge (_package-name &key url &allow-other-keys)
  (let ((matches (s-match "svn\\.sourceforge\\.\\([^/]+\\)/svnroot/\\([^/]+\\)" url)))
    (and matches (format "http://%s.sourceforge.%s/" (third matches) (second matches)))))

(defun* muv::sourceforge-git-kludge (_package-name &key url &allow-other-keys)
  (let ((matches (s-match "\\([^/\\]+\\)\\.git\\.sourceforge\\.\\([^/]+\\)/gitroot/\\1/\\1" url)))
    (and matches (format "http://%s.sourceforge.%s/" (second matches) (third matches)))))

(defun* muv::jblevins-kludge (_package-name &key url &allow-other-keys)
  (let ((matches (s-match "jblevins\\.org/git/\\([^/]+\\)\\.git" url)))
    (and matches (format "http://jblevins.org/projects/%s" (second matches)))))

(defun* muv::ryuslash-kludge (_package-name &key url &allow-other-keys)
  (let ((matches (s-match "git://ryuslash\\.org/\\([^/]+\\).git" url)))
    (and matches (format "http://ryuslash.org/projects/%s.html" (second matches)))))

(defun* muv::logilab-kludge (_package-name &key url &allow-other-keys)
  (let ((matches (s-match "http://hg\\.logilab\\.org/\\([^/]+\\)$" url)))
    (and matches (format "http://www.logilab.org/projects/%s" (second matches)))))

(defun* muv::joyful-kludge (_package-name &key url &allow-other-keys)
  (let ((matches (s-match "https?://joyful\\.com/repos/[^/]+" url)))
    (and matches url)))

(defun* muv::hub-darcs-kludge (_package-name &key url &allow-other-keys)
  (let ((matches (s-match "https?://hub\\.darcs\\.net/[^/]+/[^/]+" url)))
    (and matches url)))

(defun* muv::svn-common-kludge (_package-name &key fetcher url &allow-other-keys)
  (and (muv::compare-fetcher fetcher 'svn) (replace-regexp-in-string "svn/.*$" "" url)))

(defun* muv::plain-url-kludge (_package-name &key url &allow-other-keys)
  (read-from-minibuffer "Verify url: " url))

(defun muv::recipe-url (package)
  "Returns the melpa recipe URL (github) for PACKAGE."
   (format "http://raw.github.com/milkypostman/melpa/master/recipes/%s" package))


(defun muv::fetch-recipe (package callback)
  "Returns the melpa recipe (as a list) for PACKAGE."
  (url-retrieve (muv::recipe-url package)
                (lambda (s)
                  (let ((request-error (plist-get s :error)))
                    (cond (request-error
                           ;; (signal (car request-error) (cdr request-error)))
                           (error (format "melpa-upstream-visit: Error fetching %s recipe: %s" package
                                          (cdr request-error))))
                          (t (let ((sexp (progn (goto-char (point-min))
                                                (search-forward "(")
                                                (backward-char)
                                                (sexp-at-point))))
                               (funcall callback sexp))))))
                nil t))

(defun muv::first-non-nil-result (function-list &rest args)
  "Applies the functions in FUNCTION-LIST to ARGS in order,
returning the first non nil result."
  (let ((res (ignore-errors (apply (car function-list) args))))
    (cond (res (when muv:debug
                 (message "melpa-upstream-visit: %S returned %S" (car function-list) res))
               res)
          (t (apply 'muv::first-non-nil-result (cdr function-list) args)))))

(defun muv::url-from-recipe(recipe)
  "Tries to guess the homepage URL of the package described by
RECIPE."
  (apply 'muv::first-non-nil-result (append muv:user-url-kludges muv:url-kludges) recipe))


;;;###autoload
(defun muv (package-name)
  "`browse-url's (or at least tries to) the PACKAGE-NAME's homepage."
  (interactive (list (let ((tat (thing-at-point 'symbol))
                           (packages (mapcar (lambda (el)
                                               (symbol-name (car el)))
                                             package-archive-contents)))
                       (funcall muv:completing-read-function
                                "Visit package upstream: "
                                packages
                                nil t nil nil
                                (find tat packages :test 'equal)))))
  (muv::fetch-recipe package-name
                     (lambda (recipe)
                       (let ((url (muv::url-from-recipe recipe)))
                         (if url
                             (browse-url url)
                           (error "No package named '%s' can be found in MELPA." package-name))))))


(defadvice describe-package-1 (after muv-describe-package-button  activate)
  (save-excursion
    (when (not (null muv:button-location))
      (goto-char (point-min))
      (let* ((package-name (thing-at-point 'symbol))
             (window-width (window-width (selected-window)))
             (action (lexical-let ((p package-name))
                       (lambda (&rest args)
                         (interactive)
                         (muv p)))))
        (case muv:button-location
          (package-name (let ((tat (thing-at-point 'symbol)))
                          (make-button (point-min) (1+ (length package-name))
                                       'follow-link t
                                       'face 'muv:button-face
                                       'action action)))
          (top-right (move-end-of-line nil)
                     (insert
                      (propertize " "
                                  'display `(space :align-to
                                                   (- right ,(string-width
                                                              (concat " " muv:button-label))))))
                     (insert-button muv:button-label
                                    'follow-link t
                                    'face 'muv:button-face
                                    'action action)))))))


(provide 'melpa-upstream-visit)

;;; melpa-upstream-visit.el ends here
