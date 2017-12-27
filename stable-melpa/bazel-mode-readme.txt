Basic Bazel support for Emacs

For now this mode gives you python syntax highlighting for `WORKSPACE`, `BUILD.bazel` and `.bzl` files.
Formatting is supported by running `buildifier`.

## Installing buildifier
Buildifier needs Go to compile and install. Follow the directions in [1] or install by running the following command.

```
go get -u github.com/bazelbuild/buildtools/buildifier
```

[1] https://github.com/bazelbuild/buildtools/blob/master/buildifier/README.md

## Formatting Bazel files manually
`C-c C-f` runs `bazel-format` on the current buffer.

## Formatting Bazel files automatically before saving
Add the following to your Emacs config.
```
(add-hook 'bazel-mode-hook (lambda () (add-hook 'before-save-hook #'bazel-format nil t)))
```
