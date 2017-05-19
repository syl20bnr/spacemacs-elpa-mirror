To use this package, do

(all-the-icons-ivy-setup)

Or if you prefer to only set transformers
for a subset of ivy commands:

(require 'all-the-icons-ivy)
(ivy-set-display-transformer 'ivy-switch-buffer 'all-the-icons-ivy-buffer-transformer)
