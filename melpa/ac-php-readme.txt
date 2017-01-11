Auto Completion source for php.
support  Linux and OSX,  but windows need more test
More info and **example** at : https://github.com/xcwen/ac-php

============================================== For add
add to .emacs:

(add-hook 'php-mode-hook '(lambda ()
                           (auto-complete-mode t)
                           (require 'ac-php)
                           (setq ac-sources  '(ac-source-php ) )
                           (yas-global-mode 1)

                           (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
                           (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back   ) ;go back
                           ))


============================================== For test
save it as .emacs  then restart emacs  and open php file to test

(setq package-archives
     '(("melpa" . "https://melpa.org/packages/")) )

(package-initialize)
(unless (package-installed-p 'ac-php )
 (package-refresh-contents)
 (package-install 'ac-php )
 )
(require 'cl)
(require 'php-mode)
(add-hook 'php-mode-hook '(lambda ()
                           (auto-complete-mode t)
                           (require 'ac-php)
                           (setq ac-sources  '(ac-source-php ) )
                           (yas-global-mode 1)

                           (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
                           (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back   ) ;go back
                           ))
