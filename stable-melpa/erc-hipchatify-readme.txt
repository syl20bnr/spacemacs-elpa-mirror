Show hipchat emoticons and render html (along with images) in erc buffers.
Requires Emacs 24.4

(require 'erc-hipchatify)
(add-to-list 'erc-modules 'hipchatify)
(erc-update-modules)


Since this plugin wraps `shr-render-region', it benefits from asynchronous
downloading.  To rescale images, set `shr-max-image-proportion'.
