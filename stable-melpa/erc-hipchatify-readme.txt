Show hipchat emoticons and render html (along with images) in erc buffers.
Requires Emacs 24.2

(require 'erc-hipchatify)
(add-to-list 'erc-modules 'hipchatify)
(erc-update-modules)


Since this plugin wraps `shr-render-region', it benefits from asynchronous
downloading.  To rescale images, set `shr-max-image-proportion'.

Code:

(require 'erc)
(require 'shr)

(require 's)
(require 'alert) ;; TODO: figure out how to use native erc notifications
(require 'company)
(require 'request)
(require 'flx-ido)

(defgroup erc-hipchatify nil
  "Enable hipchatify."
  :group 'erc)

(defcustom erc-hipchatify-token nil
  "The token to which we make api calls, created at
https://atlassian.hipchat.com/account/api"
  :group 'erc-hipchatify
  :type 'string)

(defcustom erc-hipchatify-server "localhost"
  "The name of the HipChat BitlBee server"
  :group 'erc-hipchatify
  :type 'string)

(defcustom erc-hipchatify-mention-channels nil
  "The name of the HipChat BitlBee channels for @mention replacement"
  :group 'erc-hipchatify
  :type 'list)

(defvar erc-hipchatify--icons nil
  "Private hash table of HipChat emoticons")

(defcustom erc-hipchatify-tags '("html"
                                 "body"
                                 "style"
                                 "script"
                                 "svg"
                                 "sup"
                                 "sub"
                                 "label"
                                 "p"
                                 "div"
                                 "s"
                                 "del"
                                 "b"
                                 "i"
                                 "em"
                                 "strong"
                                 "u"
                                 "tt"
                                 "base"
                                 "a"
                                 "object"
                                 "video"
                                 "img"
                                 "audio"
                                 "pre"
                                 "blockquote"
                                 "dl"
                                 "dt"
                                 "dd"
                                 "ul"
                                 "ol"
                                 "li"
                                 "br"
                                 "span"
                                 "h1"
                                 "h2"
                                 "h3"
                                 "h4"
                                 "h5"
                                 "h6"
                                 "hr"
                                 "title"
                                 "font"
                                 "table")
  "The list of tags supported by shr; unknown tags will be escaped"
  :group 'erc-hipchatify
  :type 'list)

(defun erc-hipchatify--process-request (data)
  (let ((startIndex (assoc-default 'startIndex data))
        (maxResults (assoc-default 'maxResults data))
        (nextUrl    (assoc-default 'next (assoc-default 'links data))))
    (mapcar
     (lambda (x)
       (puthash (assoc-default 'shortcut x) (assoc-default 'url x) erc-hipchatify--icons))
     (assoc-default 'items data))
    (message "Finished downloading HipChat emoticons starting from index %d" startIndex)
    (when nextUrl
      (erc-hipchatify--request-icons nextUrl))))

(defun erc-hipchatify--request-icons (&optional url)
  (request
   (or url "https://api.hipchat.com/v2/emoticon")
   :params `(("auth_token" . ,erc-hipchatify-token)
             ("max-results" . "500"))
   :parser 'json-read
   :error (function* (lambda (&key error-thrown &allow-other-keys&rest _)
                       (message "erc-hipchatify error: %S" error-thrown)))
   :status-code '((500 . (lambda (&rest _) (message "erc-hipchatify got an internal server error (500) from HipChat."))))
   :success (function*
             (lambda (&key data &allow-other-keys)
               (when data
                 (erc-hipchatify--process-request data))))))

(defun erc-hipchatify-connect (server nick)
  (when (and erc-hipchatify-token (string-equal server erc-hipchatify-server))
    (setq erc-hipchatify--icons (make-hash-table :test 'equal))
    ;; apparently these are missing?
    (puthash "thumbsup" "https://dujrsrsgsd3nh.cloudfront.net/img/emoticons/thumbs_up.png" erc-hipchatify--icons)
    (puthash "thumbsdown" "https://dujrsrsgsd3nh.cloudfront.net/img/emoticons/thumbs_down.png" erc-hipchatify--icons)
    (erc-hipchatify--request-icons)))

(defun erc-hipchatify-pre-hook (string)
  "Doesn't display anything from <Link> since it's mostly
garabled html; we'll be rendering most of that stuff ourselves"
  (cond
   ((s-starts-with? "<Link>" string)
    (setq erc-insert-this nil))
   ((s-starts-with? "<Bamboo>" string)
    (setq erc-insert-this nil))))

(defun erc-hipchatify-notify-here ()
  "Check for '@here' in the message and alert the user if the
window isn't in focus / visible"
  (save-excursion
    ;; use the fact that erc leaves the buffer narrowed so we can extract the
    ;; string, we substract 1 from point-max so we don't get an extra newline
    (let* ((origmsg (buffer-substring-no-properties (point-min) (1- (point-max)))))
      (if (s-starts-with? "<" origmsg)
          ;; now, search for the first "> " which indicates the end of the nickname
          ;; and start of the message (adding two which is the length of "> ")
          (let* ((startPos (+ 2 (s-index-of "> " origmsg)))
                 (newStart (+ (point-min) startPos))
                 (msg (substring origmsg startPos))
                 (usr (substring origmsg 1 (- startPos 2))))
            ;; notify for @here
            ;; TODO: figure out how to use erc notify natively
            (when (and (or (s-contains? "@here" msg)
                           (s-contains? "@all" msg))
                     ;; only alert if not in focus
                     (not (eq (current-buffer) (window-buffer (selected-window)))))
                (alert msg :title usr)))))))

(defun erc-hipchatify-render-html ()
  "Modify the buffer to replace (icon) with an html img tag, then
render the whole message. For some text emoticons, such
as (shrug) we just use the actual text-based representation.

Also, skip messages that don't begin with '<' since those are irc
messages."
  (save-excursion
    ;; use the fact that erc leaves the buffer narrowed so we can extract the
    ;; string, we substract 1 from point-max so we don't get an extra newline
    (let* ((oldStart (point-min))
           (oldWidth shr-width)
           (origmsg (buffer-substring-no-properties (point-min) (1- (point-max)))))
      (when (s-starts-with? "<" origmsg)
        ;; now, search for the first "> " which indicates the end of the nickname
        ;; and start of the message (adding two which is the length of "> ")
        (let* ((startPos (+ 2 (s-index-of "> " origmsg)))
               (newStart (+ (point-min) startPos))
               (msg (substring origmsg startPos)))
          ;; before we do anything, escape '<' and '>' on tags that shr doesn't
          ;; understand; e.g. replace '<3', '<-', and such with &lt;
          (goto-char newStart)
          (while (re-search-forward "<\\(/\\)?\\([a-zA-Z0-9-]+\\)" nil t)
            (if (not (member (match-string-no-properties 2) erc-hipchatify-tags))
                (replace-match (concat "&lt;"
                                       (match-string-no-properties 1)
                                       (match-string-no-properties 2)))))
          ;; replace bamboo img tags with hipchat emoticons
          (goto-char newStart)
          (while (search-forward "<img src=\"https://bamboo.bb-inf.net/images/iconsv4/icon-build-queued.png\" alt=\"icon-build-queued.png\">" nil t)
            (replace-match "(continue)"))
          (goto-char newStart)
          (while (search-forward "<img src=\"https://devtools-bamboo.internal.atlassian.com/images/iconsv4/icon-build-successful.png\" alt=\"icon-build-successful.png\" width=\"16\" height=\"16\">" nil t)
            (replace-match "(successful)"))
          (goto-char newStart)
          (while (search-forward "<img src=\"https://devtools-bamboo.internal.atlassian.com/images/iconsv4/icon-build-failed.png\" alt=\"icon-build-failed.png\" width=\"16\" height=\"16\">" nil t)
            (replace-match "(failed)"))
          ;; TODO: replace with customized function
          ;; replace image looking links with an img tag
          ;; imgur
          (goto-char (1- newStart))
          (while (re-search-forward "[^\"]\\(http[s]*://\\(www\\.\\)?imgur\\.com\\)\\(/gallery\\)?/\\([^/\s\n\t]+\\)[\s\n\t$]" nil t)
            (replace-match
             (format " <img alt=\"%s\" src=\"http://imgur.com/download/%s\"/> "
                     (concat (match-string-no-properties 1)
                             (match-string-no-properties 3)
                             "/"
                             (match-string-no-properties 4))
                     (match-string-no-properties 4))))
          ;; link that ends in an image extension
          (goto-char (1- newStart))
          (while (re-search-forward "[^\"]\\(http[^\s\n\t]+\\.\\(png\\|jpg\\|jpeg\\|gif\\|svg\\)[^\s\n\t]*\\)" nil t)
            (replace-match
             (format " <img alt=\"%s\" src=\"%s\"/>"
                     (match-string-no-properties 1)
                     (match-string-no-properties 1))))
          ;; replace hipchat emoticons contained in parentheses
          (when erc-hipchatify--icons
            (goto-char newStart)
            (while (re-search-forward "(\\([a-zA-Z0-9_]+\\))" nil t)
              (let* ((hp-shortcut (match-string-no-properties 1))
                     (hp-link (gethash hp-shortcut erc-hipchatify--icons)))
                (cond
                 ((string-equal hp-shortcut "shrug")
                  (replace-match "¯\\\\_(ツ)_/¯"))
                 ((string-equal hp-shortcut "tableflip")
                  (replace-match "(╯°□°）╯︵ ┻━┻"))
                 ((string-equal hp-shortcut "owlflip")
                  (replace-match "(ʘ∇ʘ)ク 彡 ┻━┻"))
                 (hp-link
                  (replace-match
                   (format "<img alt=\"(%s)\" src=\"%s\" />" hp-shortcut hp-link)))))))
          ;; subtract the length of the username from shr-width so that
          ;; wrapping works
          (setq shr-width (1- (- oldWidth (- newStart oldStart))))
          (shr-render-region newStart (1- (point-max)))
          (setq shr-width oldWidth)
          ;; rendering the region adds two lines before and after?
          (goto-char newStart)
          (while (re-search-forward "\n\n" nil t)
            (replace-match ""))
          (goto-char newStart)
          (while (re-search-forward "\n" nil t)
            (replace-match " "))
          (goto-char newStart)
          (when (char-equal (following-char) ? )
            (delete-char 1))
          (goto-char newStart)
          (when (char-equal (following-char) ?\n)
            (delete-char 1))
          (goto-char (point-max))
          (when (not (char-equal (char-before) ?\n))
            (insert-before-markers "\n")))))))

(defun erc-button-remove-old-buttons ()
  "Seriously, what the hell.
This method is defined in `erc-button' but throws a huge wrench
into `shr-render-region'. Is this method even needed?"
  nil)

(defun erc-hipchatify-icon-company-backend (command &optional arg &rest ignored)
  "A company backend that uses the keys from the icon hash table
and appends ')'"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'erc-hipchatify-icon-company-backend))
    (prefix (and (eq major-mode 'erc-mode)
                 (when (looking-back "([[:alnum:]]*")
                 (match-string 0))))
    (candidates
     (flx-flex-match arg
                     (mapcar
                      (lambda (x) (concat "(" x ")"))
                      (hash-table-keys erc-hipchatify--icons))))))

(defun erc-hipchatify-mode-hook ()
  "Turn on company mode and register our backend"
  (setq-local company-auto-complete-chars '(?\())
  (add-to-list 'company-backends 'erc-hipchatify-icon-company-backend)
  (company-mode-on))

(defun erc-cmd-ANIM (&rest msg)
  (when msg
      (erc-send-message (concat "/anim " (mapconcat 'identity msg " ")))))

(defun erc-cmd-GIF (&rest msg)
  (when msg
      (erc-send-message (concat "/gif " (mapconcat 'identity msg " ")))))

(defun erc-cmd-GIPHY (&rest msg)
  (when msg
      (erc-send-message (concat "/giphy " (mapconcat 'identity msg " ")))))

(defun erc-cmd-IMG (&rest msg)
  (when msg
      (erc-send-message (concat "/img " (mapconcat 'identity msg " ")))))

(defun erc-cmd-MEME (&rest msg)
  (when msg
      (erc-send-message (concat "/meme " (mapconcat 'identity msg " ")))))

(defun erc-cmd-CODE (&rest msg)
  (when msg
      (erc-send-message (concat "/code " (mapconcat 'identity msg " ")))))

(defun erc-cmd-QUOTE (&rest msg)
  (when msg
      (erc-send-message (concat "/quote " (mapconcat 'identity msg " ")))))

(defun erc-hipchatify-mention-send-modify (msg)
  (when (member (buffer-name) erc-hipchatify-mention-channels)
    (setq erc-send-this nil)
    (erc-send-message
     (replace-regexp-in-string "\\b\\([a-zA-Z0-9|_]+\\)\\b"
                               (lambda (s) (save-match-data
                                             (if (gethash (s-downcase s) erc-channel-users)
                                                 (concat "@" s)
                                               s)))
                               msg))))

###autoload
(eval-after-load 'erc
  '(define-erc-module hipchatify nil
     "Show hipchat emoticons and render html"
     ((add-hook 'erc-after-connect 'erc-hipchatify-connect t)
      (add-hook 'erc-insert-pre-hook 'erc-hipchatify-pre-hook)
      (add-hook 'erc-insert-modify-hook 'erc-hipchatify-notify-here)
      (add-hook 'erc-insert-modify-hook 'erc-hipchatify-render-html)
      (add-hook 'erc-send-modify-hook 'erc-hipchatify-render-html)
      (add-hook 'erc-send-pre-hook 'erc-hipchatify-mention-send-modify)
      (add-hook 'erc-mode-hook 'erc-hipchatify-mode-hook))
     ((remove-hook 'erc-after-connect 'erc-hipchatify-connect)
      (remove-hook 'erc-insert-pre-hook 'erc-hipchatify-pre-hook)
      (remove-hook 'erc-insert-modify-hook 'erc-hipchatify-notify-here)
      (remove-hook 'erc-insert-modify-hook 'erc-hipchatify-render-html)
      (remove-hook 'erc-send-modify-hook 'erc-hipchatify-render-html)
      (remove-hook 'erc-send-pre-hook 'erc-hipchatify-mention-send-modify)
      (remove-hook 'erc-mode-hook 'erc-hipchatify-mode-hook))
     t))

(provide 'erc-hipchatify)

erc-hipchatify.el ends here
