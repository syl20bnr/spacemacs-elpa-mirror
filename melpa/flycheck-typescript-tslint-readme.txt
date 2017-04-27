This is extension for Flycheck.

This extension displays TSLint errors while working on Typescript project

TSLint:
https://github.com/palantir/tslint

Flycheck:
http://www.flycheck.org/
https://github.com/flycheck/flycheck

For more information about this Flycheck extension:
https://github.com/Simplify/flycheck-typescript-tslint


Setup

Install TSLint:
npm install -g tslint
npm install -g typescript

Install flycheck-typescript-tslint using package.el.

Add this into your init.el file:
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-typescript-tslint-setup))

Make sure to have tslint.conf in your project directory!
