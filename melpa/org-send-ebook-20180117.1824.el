;;; org-send-ebook.el --- Send org link file to ebook reader.

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5") (seq "2.20"))
;; Package-Version: 20180117.1824
;; Package-X-Original-Version: 0.1
;; Keywords: org link ebook kindle epub mobi
;; homepage: https://github.com/stardiviner/org-send-ebook

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage:
;;
;; [M-x org-send-ebook]

;;; Code:

(require 'cl-lib) ; for `cl-case'
(require 'seq) ; for `seq-filter'
(require 'org)

(defgroup org-send-ebook nil
  "Send org-mode ebook file: link to external devices with corresponding formats.."
  :prefix "org-send-ebook-"
  :group 'org)

(defvar org-send-ebook-target-format nil)

(defcustom org-send-ebook-default-format ".epub"
  "The default target device format used to send."
  :type 'string
  :group 'org-send-ebook)

(defun org-send-ebook--read-device-info ()
  "Detect plugged in device."
  ;; TODO: improve this function.
  (if (seq-filter
       (lambda (usb)
         ;; if USB contains "Amazon Kindle" string.
         (string-match (rx "Amazon Kindle") usb)
         )
       ;; read USB devices info
       (split-string (shell-command-to-string "lsusb") "\n"))
      "kindle"
    (progn
      (warn "unknown device, can't detect device correctly, please report to https://github.com/stardiviner/org-send-ebook/issues")
      "unknown")))

(defun org-send-ebook--detect-format ()
  "Detect plugged in device's ebook format."
  (cl-case (intern (org-send-ebook--read-device-info))
    ('kindle ".mobi")
    (t org-send-ebook-default-format)))

(defun org-send-ebook--mount-path ()
  "Get Linux general mount path."
  (directory-file-name
   (concat "/run/media/" (getenv "USER"))))

(defun org-send-ebook--detect-directory ()
  "Detect plugged in device directory of saving ebook."
  (cl-case (intern (org-send-ebook--read-device-info))
    ('kindle
     (concat (org-send-ebook--mount-path) "/Kindle/documents"))
    (t
     (read-directory-name "Send to device directory: "))))

;;;###autoload
(defun org-send-ebook ()
  "Send `org-mode' ebook file: link to external devices with corresponding formats."
  (interactive)
  ;; get the file path under org-mode link.
  (when (string= (org-element-property :type (org-element-context)) "file")
    (let* ((source-file (expand-file-name (org-link-unescape (org-element-property :path (org-element-context)))))
           (target-file-name (file-name-nondirectory
                              (concat (file-name-sans-extension source-file) (org-send-ebook--detect-format))))
           (default-directory (temporary-file-directory))
           (target-file (concat (temporary-file-directory) target-file-name))
           (device-directory (org-send-ebook--detect-directory)))
      ;; convert ebook to device compatible format.
      (unless (string= (file-name-extension source-file) (file-name-extension target-file-name))
        (shell-command (concat "ebook-convert" " " (shell-quote-argument source-file) " " (shell-quote-argument target-file))))
      ;; send converted file to device
      (copy-file target-file device-directory)
      )))



(provide 'org-send-ebook)

;;; org-send-ebook.el ends here
