This is a library for showing inline contextual docs above or below.

You can use this library function `inline-docs` in packages like
https://github.com/stardiviner/eldoc-overlay-mode.

```eldoc
(setq eldoc-message-function #'inline-docs)
```

```elisp
(inline-docs "FORMATED-STRING")
(inline-docs "STRING")
```
