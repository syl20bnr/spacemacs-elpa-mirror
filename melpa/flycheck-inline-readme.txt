Provide an error display function to show Flycheck errors inline, directly
below their location in the buffer.

# Setup

(with-eval-after-load 'flycheck
  (flycheck-inline-mode))
