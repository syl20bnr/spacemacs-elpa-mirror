This Flycheck extension configures Flycheck automatically for the current
Cargo project.

# Setup

    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

# Usage

Just use Flycheck as usual in your Rust/Cargo projects.

Note: You must run `cargo build` initially to install all dependencies.  If
you add new dependencies to `Cargo.toml` you need to run `cargo build`
again. Otherwise you will see spurious errors about missing crates.

This extension also provides a convenience function for looking up
explanations of the compiler error under point
(`flycheck-rust-explain-error') that is not bound by default.
