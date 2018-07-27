;;; proxy-mode.el --- A minor mode to toggle proxy.

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "25"))
;; Package-Version: 20180521.330
;; Package-X-Original-Version: 0.1
;; Keywords: comm proxy
;; homepage: https://github.com/stardiviner/proxy-mode

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; toggle proxy-mode to use proxy.
;;; [M-x proxy-mode RET]

;;; Code:

(require 'url-gw)
(require 'socks)
(require 'cl-lib)

(defgroup proxy-mode nil
  "A minor mode to toggle proxy."
  :prefix "proxy-mode-"
  :group 'proxy)

(defcustom proxy-mode-rules-alist nil
  "A list of rules for proxy."
  :type 'alist
  :group 'proxy-mode)

(defvar proxy-mode-types
  '(("HTTP Proxy" . http) ("Socks Proxy" . socks) ("URL proxy" . url))
  "A list of `proxy-mode' supported proxy types.")

(defvar proxy-mode-proxy-type nil
  "Currently enabled proxy type.")

;; Privoxy
(defcustom proxy-mode-http-proxy "http://localhost:8118"
  "Default HTTP_PROXY environment variable value."
  :type 'string
  :safe #'stringp
  :group 'proxy-mode)

(defcustom proxy-mode-url-proxy '(("http"  . "127.0.0.1:8118")
                                  ("https" . "127.0.0.1:8118")
                                  ("ftp"   . "127.0.0.1:8118")
                                  ;; don't use `localhost', avoid robe server (For Ruby) can't response.
                                  ("no_proxy" . "127.0.0.1")
                                  ("no_proxy" . "^.*\\(baidu\\|sina)\\.com"))
  "A list of rules for `url-proxy-services'."
  :type 'alist
  :safe #'nested-alist-p
  :group 'proxy-mode)

(defcustom proxy-mode-socks-proxy '("Default server" "127.0.0.1" 1080 5)
  "Default `socks-server' value."
  :type 'list
  :safe #'listp
  :group 'proxy-mode)

;;; ------------------------------ HTTP Proxy ---------------------------------------------------

(defun proxy-mode-http-enable ()
  "Enable HTTP proxy."
  (make-local-variable 'process-environment)
  ;; TODO: how to `setenv' buffer locally?
  ;; ( "HTTP_PROXY" process-environment)
  (setenv "HTTP_PROXY"  proxy-mode-http-proxy)
  (setenv "HTTPS_PROXY" proxy-mode-http-proxy)
  (setq-local proxy-mode-proxy-type "http"))

(defun proxy-mode-http-disable ()
  "Disable HTTP proxy."
  (setenv "HTTP_PROXY" nil)
  (setenv "HTTPS_PROXY" nil)
  (setq-local proxy-mode-proxy-type nil))

;;; ------------------------------ URL Proxy --------------------------------------------------

(defun proxy-mode-url-enable ()
  "Enable URL proxy."
  (setq-local url-proxy-services proxy-mode-url-proxy)
  (setq-local proxy-mode-proxy-type "url"))

(defun proxy-mode-url-disable ()
  "Disable URL proxy."
  (setq-local url-proxy-services nil)
  (setq-local proxy-mode-proxy-type nil))

;;; ------------------------------ Socks Proxy --------------------------------------------------

(defun proxy-mode-socks-enable ()
  "Enable Socks proxy."
  (setq-local url-gateway-method 'socks)
  (setq-local socks-noproxy '("localhost"))
  (setq-local socks-server proxy-mode-socks-proxy)
  (setq-local proxy-mode-proxy-type "socks"))

(defun proxy-mode-socks-disable ()
  "Disable Socks proxy."
  (setq-local url-gateway-method 'native)
  (setq-local proxy-mode-proxy-type nil))

;;; ------------------------------------------------------------------------------------------

;;;###autoload
(defun proxy-mode-enable ()
  "Enable proxy-mode."
  (interactive
   (let ((selected (if proxy-mode-proxy-type
                       (message "proxy-mode is already enabled.")
                     (cl-case (cdr (assoc
		                                (completing-read "Select proxy service to enable: " (mapcar 'car proxy-mode-types))
		                                proxy-mode-types))
                       ('http (proxy-mode-http-enable))
                       ('socks (proxy-mode-socks-enable))
                       ('url (proxy-mode-url-enable))))))
     (message "%s proxy selected." selected)
     nil)))

;;;###autoload
(defun proxy-mode-disable ()
  "Disable proxy-mode."
  (interactive)
  (pcase proxy-mode-proxy-type
    ("http" (proxy-mode-http-disable))
    ("url" (proxy-mode-url-disable))
    ("socks" (proxy-mode-socks-disable))))

(defvar proxy-mode-map nil)

;;;###autoload
(define-minor-mode proxy-mode
  "A minor mode to toggle `proxy-mode'."
  :require 'proxy-mode
  :init-value nil
  :lighter " Proxy"
  :group 'proxy-mode
  :keymap proxy-mode-map
  :global nil
  (if proxy-mode
      (call-interactively 'proxy-mode-enable)
    (call-interactively 'proxy-mode-disable)))

;; ;;;###autoload
;; (define-globalized-minor-mode global-proxy-mode proxy-mode proxy-mode)



(provide 'proxy-mode)

;;; proxy-mode.el ends here
