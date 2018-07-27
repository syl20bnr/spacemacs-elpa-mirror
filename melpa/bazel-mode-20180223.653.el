;;; bazel-mode.el --- A major mode for editing Bazel files

;; Version: 1.0.0
;; Package-Version: 20180223.653
;; Author: Neri Marschik
;; Url: https://github.com/codesuki/bazel-mode
;; Keywords: languages, bazel
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:
;;
;; Basic Bazel support for Emacs
;;
;; For now this mode gives you python syntax highlighting for `WORKSPACE`, `BUILD.bazel` and `.bzl` files.
;; Formatting is supported by running `buildifier`.
;;
;; ## Installing buildifier
;; Buildifier needs Go to compile and install. Follow the directions in [1] or install by running the following command.
;;
;; ```
;; go get -u github.com/bazelbuild/buildtools/buildifier
;; ```
;;
;; [1] https://github.com/bazelbuild/buildtools/blob/master/buildifier/README.md
;;
;; ## Formatting Bazel files manually
;; `C-c C-f` runs `bazel-format` on the current buffer.
;;
;; ## Formatting Bazel files automatically before saving
;; Add the following to your Emacs config.
;; ```
;; (add-hook 'bazel-mode-hook (lambda () (add-hook 'before-save-hook #'bazel-format nil t)))
;; ```

;;; Code:
(defgroup bazel nil
  "Major mode for editing Bazel files."
  :prefix "bazel-"
  :group 'languages)

(defcustom bazel-format-command "buildifier"
  "The command to run to format gn files in place."
  :group 'bazel
  :type 'string)

(defun bazel-format ()
  "Run 'buildifier' on the buffer."
  (interactive)
  (let ((current-buffer (current-buffer))
        (oldpoint (point))
        (result-buffer (get-buffer-create "*bazel-format*")))
    (with-current-buffer result-buffer (erase-buffer))
    (if (zerop (call-process-region (point-min) (point-max) bazel-format-command nil result-buffer nil))
        (progn
          (with-current-buffer current-buffer (delete-region (point-min) (point-max)))
          (with-current-buffer current-buffer (insert-buffer-substring result-buffer))
          (goto-char oldpoint))
      (message "bazel-format failed"))
    (kill-buffer result-buffer)))

(defvar bazel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-f" 'bazel-format)
    map))

;;;###autoload
(define-derived-mode bazel-mode python-mode "Bazel"
  "Major mode for editing Bazel files."
  :group 'bazel

  (setq-local comment-use-syntax t)
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (setq-local indent-tabs-mode nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bazel\\'" . bazel-mode))
(add-to-list 'auto-mode-alist '("\\.bzl\\'" . bazel-mode))
(add-to-list 'auto-mode-alist '("WORKSPACE\\'" . bazel-mode))

(provide 'bazel-mode)
;;; bazel-mode.el ends here
