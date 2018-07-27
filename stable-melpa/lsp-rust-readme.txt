lsp-mode client for the Rust Language Server (RLS).
See https://github.com/rust-lang-nursery/rls

# Setup

You can load lsp-rust after lsp-mode by adding the following to your init
file:

   (with-eval-after-load 'lsp-mode
     (require 'lsp-rust)
     (add-hook 'rust-mode-hook #'lsp-rust-enable))

You may want to customize the command that lsp-rust uses to launch the RLS.
See `lsp-rust-rust-command'.
