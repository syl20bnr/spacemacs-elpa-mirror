;;; editorconfig-custom-majormode.el --- Decide major-mode from EditorConfig

;; Author: 10sr <8slashes+el [at] gmail [dot] com>
;; URL: https://github.com/10sr/editorconfig-custom-major-mode-el
;; Package-Version: 20160729.24
;; Version: 0.0.1
;; Package-Requires: ((editorconfig "0.6.0"))
;; Keywords: editorconfig util

;; This file is not part of GNU Emacs.

;; This is free and unencumbered software released into the public domain.

;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.

;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; For more information, please refer to <http://unlicense.org>


;;; Commentary:

;; An EditorConfig extension that defines a property to specify which
;; Emacs major-mode to use for files.

;; For example, add emacs_mode to your .editorconfig as follows, and
;; `nginx-mode' will be always enabled when visiting *.conf files.

;; [*.conf]
;; emacs_mode = nginx

;; To enable this plugin, add `editorconfig-custom-majormode' to
;; `editorconfig-custom-hooks':

;; (add-hook 'editorconfig-custom-hooks
;;           'editorconfig-custom-majormode)

;;; Code:

(defun editorconfig-custom-majormode--is-a-mode-p (target want)
  "Return non-nil if major mode TARGET is a major mode WANT."
  (or (eq target
          want)
      (let ((parent (get target 'derived-mode-parent)))
        (and parent
             (editorconfig-custom-majormode--is-a-mode-p parent want)))))

;;;###autoload
(defun editorconfig-custom-majormode (hash)
  "Get emacs_mode property from HASH and set major mode.

If `package' is installed on your Emacs and the major mode specified is
installable, this plugin asks whether you want to install and enable it
automatically."
  (let* ((mode-str (gethash 'emacs_mode
                            hash))
         (mode (and mode-str
                    (not (string= mode-str
                                  ""))
                    (intern (concat mode-str
                                    "-mode")))))
    (when (and mode
               (not (editorconfig-custom-majormode--is-a-mode-p major-mode
                                                                mode)))
      (if (fboundp mode)
          (funcall mode)
        (if (and (eval-and-compile (require 'package nil t))
                 (assq mode
                       package-archive-contents)
                 (yes-or-no-p (format "editorconfig-custom-majormode: Major-mode `%S' not found but available as a package. Install?"
                                      mode)))
            (progn
              (package-install mode)
              (require mode)
              (funcall mode))
          (display-warning :error (format "Major-mode `%S' not found"
                                          mode)))))))

(provide 'editorconfig-custom-majormode)

;;; editorconfig-custom-majormode.el ends here
