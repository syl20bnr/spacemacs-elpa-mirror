The following Emacs Lisp will enable lsp-clangd after lsp-mode is
loaded.

   (with-eval-after-load 'lsp-mode
     (require 'lsp-clangd)
     (add-hook 'c-mode--hook #'lsp-clangd-c-enable)
     (add-hook 'c++-mode-hook #'lsp-clangd-c++-enable)
     (add-hook 'objc-mode-hook #'lsp-clangd-objc-enable))

See `lsp-clangd-executable' to customize the path to clangd.
