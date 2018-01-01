;;; ob-clojure-literate.el --- Clojure's Org-mode Literate Programming.

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "24.4") (org "9") (cider "0.16.0-snapshot") (dash "2.13.0"))
;; Package-Version: 20171225.244
;; Package-X-Original-Version: 0.1
;; Keywords: tools
;; homepage: https://github.com/stardiviner/ob-clojure-literate

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage:
;;
;; [M-x ob-clojure-literate-mode] to toggle this minor mode.

;;; Code:

(require 'ob-clojure)
(require 'cider)
(require 'dash)

(defgroup ob-clojure-literate nil
  "Clojure's Org-mode Literate Programming."
  :prefix "ob-clojure-literate-"
  :group 'ob-babel)

(defcustom ob-clojure-literate-auto-jackin-p nil
  "Auto jack in ob-clojure project.
Don't auto jack in by default for not rude."
  :type 'boolean
  :group 'ob-clojure-literate)

(defcustom ob-clojure-literate-project-location (concat user-emacs-directory "Org-mode/")
  "The location for `ob-clojure-literate' scaffold project."
  :type 'string
  :group 'ob-clojure-literate)

(defvar ob-clojure-literate-session nil)
(defvar ob-clojure-literate-original-ns nil)
(defvar ob-clojure-literate-session-ns nil)

(defcustom ob-clojure-literate-default-session "*cider-repl ob-clojure*"
  "The default session name for `ob-clojure-literate'."
  :type 'string
  :group 'ob-clojure-literate)

(defun ob-clojure-literate-get-session-list ()
  "Return a list of available started CIDER REPL sessions list."
  (-map 'buffer-name cider-connections))

(defun ob-clojure-literate-set-session ()
  "Set session name for buffer local."
  (if (eq (length (ob-clojure-literate-get-session-list)) 1)
      (setq-local ob-clojure-literate-session ob-clojure-literate-default-session)
    (setq-local ob-clojure-literate-session
                (completing-read "Choose ob-clojure-literate :session : "
                                 (ob-clojure-literate-get-session-list)))
    ))

;;;###autoload
(defun ob-clojure-literate-specify-session-header-argument ()
  "Specify ob-clojure header argument :session with value selected from a list of available sessions."
  (interactive)
  (let ((lang (nth 0 (org-babel-get-src-block-info))))
    (if (and (string= lang "clojure") ; only in clojure src block.
             (car (seq-filter ; only when :session is not specified yet.
                   (lambda (header-argument)
                     (if (eq (car header-argument) :session)
                         (not (null (cdr header-argument)))))
                   (nth 2 (org-babel-get-src-block-info)))))
        (org-babel-insert-header-arg
         "session"
         (format "\"%s\""
                 (completing-read
                  "Choose :session for ob-clojure-literate: "
                  (ob-clojure-literate-get-session-list))))
      (message "This function only used in `clojure' src block.")))
  )

;;; Auto start CIDER REPL session in a complete Leiningen project environment for Org-mode Babel to jack-in.
;;;###autoload
(defun ob-clojure-literate-auto-jackin ()
  "Auto setup ob-clojure-literate scaffold and jack-in Clojure project."
  (interactive)
  (unless (file-directory-p (expand-file-name ob-clojure-literate-project-location))
    (make-directory ob-clojure-literate-project-location t)
    (let ((default-directory ob-clojure-literate-project-location))
      (shell-command "lein new ob-clojure"))
    )
  (unless (and ob-clojure-literate-auto-jackin-p
               (cider-connected-p)
               (seq-contains cider-connections (get-buffer ob-clojure-literate-session)))
    (find-file (expand-file-name (concat ob-clojure-literate-project-location "ob-clojure/src/ob_clojure/core.clj")))
    (with-current-buffer "core.clj"
      (cider-jack-in)))
  )

(defun ob-clojure-literate-cider-do-not-find-ns ()
  "Fix the issue that `cider-current-ns' try to invoke `clojure-find-ns' to extract ns from buffer."
  (setq ob-clojure-literate-original-ns (cider-current-ns))
  (with-current-buffer ob-clojure-literate-session
    (setq ob-clojure-literate-session-ns cider-buffer-ns))
  (setq-local cider-buffer-ns ob-clojure-literate-session-ns))

(defvar ob-clojure-literate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-v M-s") 'ob-clojure-literate-specify-session-header-argument)
    (define-key map (kbd "C-c C-v M-j") 'ob-clojure-literate-auto-jackin)
    (define-key map (kbd "C-x C-e") 'cider-eval-last-sexp)
    (define-key map (kbd "C-c C-d") 'cider-doc)))

;;;###autoload
(defun ob-clojure-literate-enable ()
  "Enable Org-mode buffer locally for `ob-clojure-literate'."
  (unless (local-variable-if-set-p 'org-babel-default-header-args:clojure)
    (make-local-variable 'org-babel-default-header-args:clojure))
  (add-to-list 'org-babel-default-header-args:clojure
               `(:session . ,ob-clojure-literate-session))
  (setq ob-clojure-literate-session (ob-clojure-literate-set-session))
  (ob-clojure-literate-cider-do-not-find-ns)
  (message "ob-clojure-literate minor mode enabled.")
  )

;;;###autoload
(defun ob-clojure-literate-disable ()
  "Disable Org-mode buffer locally for `ob-clojure-literate'."
  (unless (local-variable-if-set-p 'org-babel-default-header-args:clojure)
    (make-local-variable 'org-babel-default-header-args:clojure))
  (setq org-babel-default-header-args:clojure
        (delq t
              (mapcar
               (lambda (cons) (if (eq (car cons) :session) t cons))
               org-babel-default-header-args:clojure)))
  (setq-local cider-buffer-ns ob-clojure-literate-original-ns)
  (message "ob-clojure-literate minor mode disabled.")
  )

;;;###autoload
(define-minor-mode ob-clojure-literate-mode
  "A minor mode to toggle `ob-clojure-literate'."
  :require 'ob-clojure-literate-mode
  :init-value t
  :lighter " clj-lp"
  :group "ob-clojure-literate"
  :keymap ob-clojure-literate-mode-map
  (if ob-clojure-literate-mode
      (ob-clojure-literate-disable)
    (ob-clojure-literate-enable))
  )



(provide 'ob-clojure-literate)

;;; ob-clojure-literate.el ends here
