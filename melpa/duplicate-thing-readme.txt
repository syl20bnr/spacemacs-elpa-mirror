1. Duplicate current line.
2. Duplicate a selection when selection is active.
3. Only C-u, replicate, comment out the range.
4. Numerical prefix is specified as 'C-u 5': do multiple times repeatedly.

(require 'duplicate-thing)
(global-set-key (kbd "M-c") 'duplicate-thing)
