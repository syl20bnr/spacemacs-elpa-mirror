Inferior process for spim.

If you use `asm-mode', you can configure like this:

(require 'asm-mode)
(define-key asm-mode-map (kbd "C-`") 'inferior-run-spim)
(define-key asm-mode-map (kbd "C-c C-z") 'inferior-switch-to-spim)
(define-key asm-mode-map (kbd "C-c C-b") 'inferior-spim-send-buffer)
(define-key asm-mode-map (kbd "C-c C-l") 'inferier-spim-load-file)
(define-key asm-mode-map (kbd "C-c i") 'inferior-spim-send-reinitialize)
(define-key asm-mode-map (kbd "C-c r") 'inferior-spim-send-run)
