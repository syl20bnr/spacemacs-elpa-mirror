;;; dokuwiki.el --- Edit Remote DokuWiki Pages Using XML-RPC

;; Copyright (C) 2017 Juan Karlo Licudine

;; Author: Juan Karlo Licudine <accidentalrebel@gmail.com>
;; URL: http://www.github.com/accidentalrebel/emacs-dokuwiki
;; Package-Version: 20180102.59
;; Version: 1.0.0
;; Keywords: convenience
;; Package-Requires: ((emacs "24.3") (xml-rpc "1.6.8"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides a way to edit a remote Dokuwiki wiki on Emacs.  Uses Dokuwiki's XML-RPC API.

;; Usage:
;; (require 'dokuwiki) ;; unless installed as a package

;;; License:

;; This program is free software; you can redistributfe it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'xml-rpc)
(require 'auth-source)

(defgroup dokuwiki nil
  "Edit remote Dokuwiki pages using XML-RPC"
  :group 'dokuwiki)

(defcustom dokuwiki-xml-rpc-url ""
  "The url pointing to the \"xmlrpc.php\" file in the wiki to be accessed."
  :group 'dokuwiki
  :type 'string)

(defcustom dokuwiki-login-user-name ""
  "The user name to use when logging in to the wiki."
  :group 'dokuwiki
  :type 'string)

(defvar dokuwiki--has-successfully-logged-in nil
  "A variable that is set to true once successfully logged in to a wiki.")

;;;###autoload
(defun dokuwiki-login ()
  "Connects to the dokuwiki."
  (interactive)
  (let* ((xml-rpc-url (dokuwiki--get-xml-rpc-url))
         (credentials (dokuwiki--credentials))
         (login-user-name (plist-get credentials :user))
         (login-password (plist-get credentials :password)))
    (if (not (xml-rpc-method-call xml-rpc-url 'dokuwiki.login login-user-name login-password))
	(error "Login unsuccessful! Check if your dokuwiki-xml-rpc-url or login credentials are correct!")
      (message "Login successful!")
      (setq dokuwiki--has-successfully-logged-in t))))

(defun dokuwiki-open-page (page-name-or-url)
  "Opens a page from the wiki.

PAGE-NAME-OR-URL: The page id or url to open.

To open a page in a particular namespace add the namespace name before
the page-name.  For example, \"namespace:wiki-page\" to open the
\"wiki-page\" page inside the \"namespace\" namespace.

If the specified page does not exist, it creates a new page once the
buffer is saved."
  (interactive "sEnter page name: ")
  (if (not dokuwiki--has-successfully-logged-in)
      (user-error "Login first before opening a page")
    (let* ((page-name (car (last (split-string page-name-or-url "/"))))
	  (page-content (xml-rpc-method-call dokuwiki-xml-rpc-url 'wiki.getPage page-name)))
      (message "Page name is \"%s\"" page-name)
      (if (not page-content)
	  (message "Page not found in wiki. Creating a new buffer with page name \"%s\"" page-name)
	(message "Page exists. Creating buffer for existing page \"%s\"" page-name))
      (get-buffer-create (concat page-name ".dwiki"))
      (switch-to-buffer (concat page-name ".dwiki"))
      (erase-buffer)
      (when page-content
	(insert page-content)))))

(defun dokuwiki-save-page ()
  "Save the current buffer as a page in the wiki.

Uses the buffer name as the page name.  A buffer of \"wiki-page.dwiki\"
is saved as \"wikiurl.com/wiki-page\".  On the other hand, a buffer of
\"namespace:wiki-page.dwiki\" is saved as \"wikiurl.com/namespace:wiki-page\""
  (interactive)
  (if (not dokuwiki--has-successfully-logged-in)
      (user-error "Login first before saving a page")
    (if (not (string-match-p ".dwiki" (buffer-name)))
	(error "The current buffer is not a .dwiki buffer")
      (let ((page-name (replace-regexp-in-string ".dwiki" "" (buffer-name))))
	(if (not (y-or-n-p (concat "Do you want to save the page \"" page-name "\"?")))
	    (message "Cancelled saving of the page."))
	 (let* ((summary (read-string "Summary: "))
		(minor (y-or-n-p "Is this a minor change? "))
		(save-success (xml-rpc-method-call dokuwiki-xml-rpc-url 'wiki.putPage page-name (buffer-string) `(("sum" . ,summary) ("minor" . ,minor)))))
	   (if save-success
	       (message "Saving successful with summary %s and minor of %s." summary minor)
	     (error "Saving unsuccessful!")))))))

(defun dokuwiki-get-wiki-title ()
  "Gets the title of the current wiki."
  (interactive)
  (if (not dokuwiki--has-successfully-logged-in)
      (user-error "Login first before getting the wiki title")
    (let ((dokuwiki-title (xml-rpc-method-call dokuwiki-xml-rpc-url 'dokuwiki.getTitle)))
      (message "The title of the wiki is \"%s\"" dokuwiki-title))))

(defun dokuwiki-list-pages ()
  "Show a selectable list containing pages from the current wiki."
  (interactive)
  (if (not dokuwiki--has-successfully-logged-in)
      (user-error "Login first before listing the pages")
    (let ((page-detail-list (xml-rpc-method-call dokuwiki-xml-rpc-url 'wiki.getAllPages))
	  (wiki-title (dokuwiki-get-wiki-title))
	  (page-list ()))
      (dolist (page-detail page-detail-list)
	(push (cdr (assoc "id" page-detail)) page-list)
	)
      (dokuwiki-open-page (completing-read "Select a page to open: " page-list)))))

;; Helpers
(defun dokuwiki--credentials ()
  "Read dokuwiki credentials either from auth source or from the user input."
  (let ((auth-source-credentials (nth 0 (auth-source-search :max 1 :host (dokuwiki--get-xml-rpc-url) :require '(:user :secret)))))
    (if auth-source-credentials
        (let* ((user (plist-get auth-source-credentials :user))
               (password-raw (plist-get auth-source-credentials :secret))
               (password (if (functionp password-raw) (funcall password-raw) password-raw)))
          (list :user user :password password))
      (let ((user (dokuwiki--get-login-user-name))
            (password (read-passwd "Enter password: ")))
        (list :user user :password password)))))

(defun dokuwiki--get-xml-rpc-url ()
  "Gets the xml-rpc to be used for logging in."
  (if (not (string= dokuwiki-xml-rpc-url ""))
      dokuwiki-xml-rpc-url
    (let ((xml-rpc-url (read-string "Enter wiki URL: ")))
      (message "The entered wiki url is \"%s\"." xml-rpc-url)
      xml-rpc-url)))

(defun dokuwiki--get-login-user-name ()
  "Gets the login user name to be used for logging in."
  (if (not (string= dokuwiki-login-user-name ""))
      dokuwiki-login-user-name
    (let ((login-name (read-string "Enter login user name: ")))
      (message "The entered login user name is \"%s\"." login-name)
      login-name)))


(provide 'dokuwiki)
;;; dokuwiki.el ends here
