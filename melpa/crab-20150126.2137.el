;;; crab.el --- WebSocket server to remotely control a browser

;; Author: Brian McKenna <brian@brianmckenna.org>
;; URL: https://github.com/puffnfresh/crab-emacs
;; Package-Version: 20150126.2137
;; Version: 0.1
;; Package-Requires: ((websocket "1.0") (json "1.2"))

;; Enable crab-minor-mode to get a browsing keymap (crab-mode-map) for
;; that buffer. If you would like to browse anywhere, enable
;; global-crab-mode.

;; The Crab server can be started with (crab-server-start).

(require 'websocket)
(require 'json)

(defcustom crab-port
  2722
  "WebSocket port to listen for browsers on."
  :type 'integer
  :group 'crab)

(defvar crab-server nil "WebSocket server for Crab browsing.")
(defvar crab-client nil "WebSocket client for Crab browsing.")

;; Keys for minor mode
(defvar crab-mode-map nil "Keymap used for Crab browsing.")
(if crab-mode-map
    nil
  (progn
    (setq crab-mode-map (make-sparse-keymap))
    (define-key crab-mode-map (kbd "C-c C-x C-f") 'crab-open-url)
    (define-key crab-mode-map (kbd "C-c f") 'crab-show-link-hints)
    (define-key crab-mode-map (kbd "C-c C-e") 'crab-eval)
    (define-key crab-mode-map (kbd "C-c C-l") 'crab-location)
    (define-key crab-mode-map (kbd "C-c C-f") 'crab-forward)
    (define-key crab-mode-map (kbd "C-c C-b") 'crab-back)
    (define-key crab-mode-map (kbd "C-c C-p") 'crab-previous)
    (define-key crab-mode-map (kbd "C-c C-n") 'crab-next)
    (define-key crab-mode-map (kbd "C-c C-r") 'crab-reload)
    (define-key crab-mode-map (kbd "C-c M-v") 'crab-scroll-up)
    (define-key crab-mode-map (kbd "C-c C-v") 'crab-scroll-down)
    (define-key crab-mode-map (kbd "C-c M-<") 'crab-beginning-of-page)
    (define-key crab-mode-map (kbd "C-c M->") 'crab-end-of-page)))

;;;###autoload
(define-minor-mode crab-mode
  "A minor mode for web browsing using an external browser."
  nil
  nil
  'crab-mode-map)

;;;###autoload
(define-globalized-minor-mode global-crab-mode
  crab-mode
  crab-mode)

;;;###autoload
(define-minor-mode crab-reload-on-save-mode
  "Toggle auto reloading current tab with crab on save."
  nil
  nil
  nil
  (if crab-reload-on-save-mode
      (add-hook 'after-save-hook #'crab-reload)
    (remove-hook 'after-save-hook #'crab-reload)))

;; WebSocket server

;;;###autoload
(defun crab-server-start ()
  (interactive)
  (crab-server-stop)
  (setq websocket-mask-frames nil)
  (setq crab-server
        (websocket-server
         crab-port
         :on-open (lambda (client) (message "Crab browser connected") (crab-set-client client))
         :on-close (lambda (client) (message "Crab browser disconnected") (crab-set-client nil))
         :on-message (lambda (client result) (crab-handle-message (json-read-from-string (websocket-frame-payload result))))))
  (message "Crab server listening on %i" crab-port))

(defun crab-set-client (client)
  (when crab-client
    (websocket-close crab-client))
  (setq crab-client client))

(defun crab-server-stop ()
  (interactive)
  (crab-set-client nil)
  (when crab-server
      (websocket-server-close crab-server)))

(defun crab-handle-message (object)
  (let ((cmd (cdr (assoc 'cmd object))))
    (cond ((equal cmd "result")
           (let ((value (cdr (assoc 'value object))))
             (message "%s" value))))))

;; Crab commands
(defun crab-open-url (url)
  (interactive "MURL: ")
  (websocket-send-text crab-client (json-encode (list :cmd "open-tab" :url url))))

(defun crab-next-tab ()
  (interactive)
  (websocket-send-text crab-client (json-encode (list :cmd "next-tab"))))

(defun crab-prev-tab ()
  (interactive)
  (websocket-send-text crab-client (json-encode (list :cmd "prev-tab"))))

(defun crab-close-tab ()
  (interactive)
  (websocket-send-text crab-client (json-encode (list :cmd "close-tab"))))

(defun crab-eval (js)
  (interactive "MJavaScript: ")
  (websocket-send-text crab-client (json-encode (list :cmd "eval" :code js))))

(defun crab-location (url)
  (interactive "MURL: ")
  (crab-eval (format "window.location = \"%s\";" url)))

(defun crab-show-link-hints (hint)
  (interactive
   (list
    (let ((inhibit-quit t))
      (let ((number
             (with-local-quit
               (websocket-send-text crab-client (json-encode (list :cmd "show-link-hints")))
               (read-number "Hint: "))))
        (unless number
          (websocket-send-text crab-client (json-encode (list :cmd "hide-link-hints"))))
        number))))
  (websocket-send-text crab-client (json-encode (list :cmd "click-link-hint" :index hint))))

(defmacro defcrabcommand (name command)
  `(defun ,name ()
     (interactive)
     (crab-eval ,command)))

(defcrabcommand crab-back "window.history.back();")
(defcrabcommand crab-forward "window.history.forward();")
(defcrabcommand crab-previous "document.body.scrollTop -= 100")
(defcrabcommand crab-next "document.body.scrollTop += 100;")
(defcrabcommand crab-reload "window.location.reload();")
(defcrabcommand crab-scroll-up "document.body.scrollTop -= 1000;")
(defcrabcommand crab-scroll-down  "document.body.scrollTop += 1000;")
(defcrabcommand crab-beginning-of-page "document.body.scrollTop = 0;")
(defcrabcommand crab-end-of-page "document.body.scrollTop = document.body.scrollHeight;")


(provide 'crab)

;;; crab.el ends here
