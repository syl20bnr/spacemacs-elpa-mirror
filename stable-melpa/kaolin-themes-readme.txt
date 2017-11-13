Kaolin is a set of eye pleasing themes for GNU Emacs
With support a large number of modes and external packages.
Kaolin themes are based on the pallete that was originally
inspired by Sierra.vim with adding some extra colors.

=======  This package includes the following themes  =======

 * kaolin-dark - a dark jade variant inspired by Sierra.vim
 * kaolin-light - light variant of the original kaolin-dark
 * kaolin-eclipse - a dark purple variant
 * kaolin-ocean - dark blue variant


=======  Configuration example  =======

(require 'kaolin-themes)

(load-theme 'kaolin-dark)

=======  Custom theme settings  =======

 ;; The following set to t by default
 (setq kaolin-bold t       ; If nil, disable the bold style.
       kaolin-italic t     ; If nil, disable the italic style.
       kaolin-underline t) ; If nil, disable the underline style.

=======  Some extra theme features, disabled by default  =======

 ;; If t, use the wave underline style instead of regular underline.
 (setq kaolin-wave t)

 ;; When t, will display colored hl-line style instead dim gray
 (setq kaolin-hl-line-colored t)


                          The end of the path is the beginning.
