Usage
-----

To toggle the mode locally:
M-x vi-tilde-fringe-mode

To toggle the mode globally:
M-x global-vi-tilde-fringe-mode

To turn it on automatically only for programming modes:
(add-hook 'prog-mode-hook 'vi-tilde-fringe-mode)

Customization
-------------

Open the customization group buffer:
M-x customize-group RET vi-tilde-fringe RET

There you can change the bitmap array or the face of the symbol drawn in
the fringe. By default the symbol is a tilde :-) and its face simply
inherits from `default'.
