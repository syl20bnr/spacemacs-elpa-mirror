;;; gnus-select-account.el --- Select an account before writing a mail in gnus

;; * Header
;; #+BEGIN_EXAMPLE
;; Copyright (C) 2017 Feng Shu

;; Author: Feng Shu  <tumashu@163.com>
;; Homepage: https://github.com/tumashu/gnus-select-account
;; Keywords: convenience
;; Package-Version: 20170722.511
;; Version: 0.10

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
;; #+END_EXAMPLE

;;; Commentary:

;; * What is gnus-select-account                                      :README:
;; gnus-select-account let user select an account before write a email
;; in gnus.

;; ** Installation

;; 1. Config melpa source, please read: [[http://melpa.org/#/getting-started]]
;; 2. M-x package-install RET gnus-select-account RET

;; ** Configure
;; 1. Gnus-select-account configure
;;    #+BEGIN_EXAMPLE
;;    (require 'gnus-select-account)
;;    (gnus-select-account-enable)
;;    #+END_EXAMPLE
;; 2. Add account information to file: "~/.authinfo.gpg" or "~/.authinfo", for example:
;;    #+BEGIN_EXAMPLE
;;    machine smtp.163.com login xxxr@163.com port 465 password PASSWORD user-full-name "XXX" user-mail-address xxx@163.com
;;    machine smtp.qq.com  login xxx@qq.com   port 465 password PASSWORD user-full-name "XXX" user-mail-address xxx@qq.com
;;    #+END_EXAMPLE

;;; Code:

;; * gnus-select-account's code                                                        :code:
(require 'gnus)

(defgroup gnus-select-account nil
  "Select an account before writing a mail in gnus."
  :group 'gnus)

(defcustom gnus-select-account-prefer-sendmail nil
  "Put 'sendmail' item to first place when show accounts list."
  :group 'gnus-select-account
  :type 'boolean)

(defun gnus-select-account--get-account-info ()
  "Get account information from auth-source."
  (mapcar
   (lambda (account)
     (let* ((user (plist-get account :user))
            (host (plist-get account :host))
            (port (plist-get account :port))
            (user-full-name (plist-get account :user-full-name))
            (user-mail-address (plist-get account :user-mail-address))
            (name (or user-mail-address
                      (if (string-match-p "@" user)
                          user
                        (concat user "@" host)))))
       `(,name :type imap :user ,user :port ,port :host ,host
               :user-full-name ,user-full-name
               :user-mail-address ,(or user-mail-address name))))
   (auth-source-search :max 1000 :port '("smtp" "25" "465" "587"))))

;;;###autoload
(defun gnus-select-account ()
  "Select an account before writing a mail in gnus."
  (interactive)
  (let* ((accounts-info
          (if gnus-select-account-prefer-sendmail
              `(("sendmail" :type sendmail)
                ,@(gnus-select-account--get-account-info))
            `(,@(gnus-select-account--get-account-info)
              ("sendmail" :type sendmail))))
         (account-used
          (cdr (assoc (completing-read
                       "Which account will be used to send email? "
                       (mapcar #'car accounts-info))
                      accounts-info)))
         (type (plist-get account-used :type))
         (user (plist-get account-used :user))
         (port (plist-get account-used :port))
         (host (plist-get account-used :host))
         (user-full-name (plist-get account-used :user-full-name))
         (user-mail-address (plist-get account-used :user-mail-address)))
    (when type
      (if (eq type 'sendmail)
          (progn (message-replace-header
                  "X-Message-SMTP-Method" "sendmail" nil t)
                 (message-replace-header "From" "localhost" nil t))
        (message-replace-header
         "X-Message-SMTP-Method"
         (format "smtp %s %s %s" host port user) nil t)
        (if user-full-name
            (message-replace-header
             "From" (format "\"%s\" <%s>" user-full-name user-mail-address) nil t)
          (message-replace-header
           "From" user-mail-address nil t)))
      ;; Sort headers
      (let ((message-header-format-alist
             `((To)
               (Subject)
               (Cc)
               (From)
               (Newsgroups)
               (In-Reply-To)
               (Fcc)
               (Bcc)
               (Date)
               (Organization)
               (Distribution)
               (Lines)
               (Expires)
               (Message-ID)
               (References . message-shorten-references)
               (User-Agent))))
        (message-sort-headers)
        ;; Move cursor to "To: " header
        (message-goto-to)))))

;;;###autoload
(defun gnus-select-account-enable ()
  "Enable gnus-select-account feature."
  (interactive)
  (add-hook 'message-setup-hook #'gnus-select-account))


(provide 'gnus-select-account)

;; * Footer

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; gnus-select-account.el ends here
