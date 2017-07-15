This Flycheck extension provides a `Lint.jl' integration for flycheck (see
URL `https://github.com/tonyhffong/Lint.jl') to check Julia buffers for
errors.

# Setup

Add the following to your init file:

     ;; Enable Flycheck checker
     (flycheck-julia-setup)

# Usage

Just use Flycheck as usual in julia-mode buffers. Flycheck will
automatically use the `flycheck-julia` syntax checker.
