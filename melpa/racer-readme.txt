You will need to configure Emacs to find racer:

(setq racer-rust-src-path "<path-to-rust-srcdir>/src/")
(setq racer-cmd "<path-to-racer>/target/release/racer")

To activate racer in Rust buffers, run:

(add-hook 'rust-mode-hook #'racer-mode)

You can also use racer to find definition at point via
`racer-find-definition', bound to `M-.' by default.

Finally, you can also use Racer to show the signature of the
current function in the minibuffer:

(add-hook 'racer-mode-hook #'eldoc-mode)
