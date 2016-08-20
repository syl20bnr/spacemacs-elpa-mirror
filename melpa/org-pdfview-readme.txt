Add support for org links from pdfview buffers like docview.

To enable this automatically, use:
    (eval-after-load 'org '(require 'org-pdfview))

If you want, you can also configure the org-mode default open PDF file function.
    (add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdfview-open))
    (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . org-pdfview-open))
