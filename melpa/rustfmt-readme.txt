*** OBSOLETE PACKAGE - This has been merged into rust-mode ***

Call `rustfmt-format-buffer' to format the current buffer using rustfmt. It is
convenient to bind it to a key, such as:

   (define-key rust-mode-map (kbd "C-c C-f") #'rustfmt-format-buffer)

Alternatively, run rustfmt before saving rust buffers:

   (add-hook 'rust-mode-hook #'rustfmt-enable-on-save)

Errors and warnings will be visible in the `*rustfmt*' buffer.
