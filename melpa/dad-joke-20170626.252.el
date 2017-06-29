;;; dad-joke.el --- Get/display dad jokes  -*- lexical-binding: t -*-
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.2
;; Package-Version: 20170626.252
;; Keywords: games
;; URL: https://github.com/davep/dad-joke.el
;; Package-Requires: ((emacs "24"))

;; dad-joke.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2 or (at your option) any later version.
;; For details see the file COPYING.

;;; Commentary:
;;
;; dad-joke.el is a terrible bit of elisp code inspired by seeing
;; https://goo.gl/NXTJXk and also with https://goo.gl/ji4Viv in mind. With
;; "thanks" to Sue for being responsible for pointing me at the former, and
;; thus reminding me of the latter.

;;; Code:

(require 'url-vars)

(defconst dad-joke-server-url "https://icanhazdadjoke.com/"
  "URL for the dad joke server.")

(defconst dad-joke-user-agent "dad-joke.el (https://github.com/davep/dad-joke.el)"
  "User agent to send to the dad joke server.")

(defun dad-joke-get ()
  "Acquire a dad joke from the dad joke server."
  (let* ((url-mime-accept-string "text/plain")
         (url-request-extra-headers `(("User-Agent" . ,dad-joke-user-agent)))
         (buffer (url-retrieve-synchronously dad-joke-server-url t t)))
    (when buffer
      (with-current-buffer buffer
        (set-buffer-multibyte t)
        (buffer-substring-no-properties (point) (point-max))))))

;;;###autoload
(defun dad-joke (&optional insert)
  "Display a dad joke in the minibuffer.

If INSERT is non-nil the joke will be inserted into the current
buffer rather than shown in the minibuffer."
  (interactive "P")
  (let ((joke (dad-joke-get)))
    (if (zerop (length joke))
        (error "I didn't get the joke! :-(")
      (if insert
          (insert joke)
        (message "%s" joke)))))

(provide 'dad-joke)

;;; dad-joke.el ends here
