Python smart-operator mode aims to insert spaces around operators
when it is required. It was develop especially for python and requires
python-mode.

Add this file to your emacs path, and this lines to emacs initialization script
(require 'py-smart-operator)
(add-hook 'python-mode-hook 'py-smart-operator-mode)
