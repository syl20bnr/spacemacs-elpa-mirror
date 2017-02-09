Edit markdown code block in a separate buffer like `org-edit-src-code'.

Usage:

You need to place the cursor inside a code block i.e. surrounded by triple
backticks (```), and call \\[markdown-edit-indirect]. Alternatively you can
add the following key binding:

    (eval-after-load 'markdown-mode
      '(define-key markdown-mode-map (kbd "C-c '") 'markdown-edit-indirect))
