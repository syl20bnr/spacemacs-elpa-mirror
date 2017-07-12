This package defines poly-ruby-mode, which introduces polymode for
here-documents in a ruby script.

Currently editing actions against sexps does not work properly in
polymode, so it is advised you turn this mode on only when
necessary.

  (define-key ruby-mode-map (kbd "C-c m") 'toggle-poly-ruby-mode)
