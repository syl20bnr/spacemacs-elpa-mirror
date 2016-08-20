Commentary:
 company source for php.
Only support  Linux and OSX , not support Windows
More info and **example** at : https://github.com/xcwen/ac-php

(add-hook 'php-mode-hook
         '(lambda ()
            (require 'company-php)
            (company-mode t)
            (add-to-list 'company-backends 'company-ac-php-backend )))
