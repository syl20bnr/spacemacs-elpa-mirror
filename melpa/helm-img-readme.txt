TODO initializeとactionに任意の値を指定できるように
TODO 画像の取得をdeferredで非同期化

Code

(require 'helm)
(require 'json)
(require 'cl-macs)

(defgroup helm-img nil
  "Utilities for making image sources for helm."
  :group 'helm
  :group 'image)

(defcustom helm-img-thumbnail-height 100
  "Thumbnail height"
  :type 'integer
  :group 'helm-img)

(defvar helm-img-query nil)

(defun helm-img-url-p (path)
  (if (string-match "^https*://" path)
      t
    nil))

(defun helm-img-extract-body (response-buffer)
  "Extract body from HTTP response buffer."
  (with-current-buffer response-buffer
    (goto-char (point-min))
    (search-forward "\n\n")
    (buffer-substring-no-properties (point) (point-max))))

(defun helm-img-create-image-from-url (url)
  (let* ((response (url-retrieve-synchronously url))
         (body (helm-img-extract-body response)))
    (create-image body 'imagemagick t :height helm-img-thumbnail-height)))

(defun helm-img-create-image-from-file (path)
  (create-image path 'imagemagick nil :height helm-img-thumbnail-height))

(defun helm-img-create-image (path)
  "Create image object from URL or path."
  (if (helm-img-url-p path)
      (helm-img-create-image-from-url path)
    (helm-img-create-image-from-file path)))

(defun helm-img-make-string-with-image (image)
  "Create string with image object."
  (with-temp-buffer
    (insert-image image)
    (buffer-substring (point-min) (point-max))))

(cl-defmacro helm-img-define-source (name &key candidates)
  `(set (intern (concat "helm-img-source-" ,name))
        (helm-build-sync-source ,name
          :candidates (lambda () (mapcar (lambda (x)
                                           (let ((thumb (if (listp x) (cdr (assoc 'thumb x)) x))
                                                 (path (if (listp x) (cdr (assoc 'full x)) x)))
                                             (cons
                                              (helm-img-make-string-with-image (helm-img-create-image thumb))
                                              path)))
                                         (funcall (symbol-function ,candidates) helm-img-query)))
          :volatile t
          :action '(("Kill URL" . (lambda (url)
                                    (let ((x-select-enable-clipboard t))
                                      (kill-new url))
                                    (message url)))
                    ("Browse URL" . (lambda (url) (browse-url url)))))))

(provide 'helm-img)
helm-img.el ends here
