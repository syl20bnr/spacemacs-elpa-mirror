himp-mode is a minor mode for hiding imports/comments/documentation
at beginning of buffer.

To enable this mode when specific mode is activated:
(add-hook 'python-mode-hook 'himp-mode)
(add-hook 'java-mode-hook 'himp-mode)

Now imports at beginning of buffer will be hidden when himp-mode is active.

Currently, python and java modes are supported, but the package can
easily be extended to support other languages.  See documentation for
variable `himp-matchers'.
