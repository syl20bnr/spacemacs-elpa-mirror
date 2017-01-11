 company source for php.
support  Linux and OSX,  but windows need more test
More info and **example** at : https://github.com/xcwen/ac-php

(add-hook 'php-mode-hook
         '(lambda ()
            (require 'company-php)
            (company-mode t)
            (add-to-list 'company-backends 'company-ac-php-backend )))
