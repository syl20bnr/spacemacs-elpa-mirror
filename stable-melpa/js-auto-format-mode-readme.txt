# Setup

Install `Node.js' and `ESLint':

   npm install -g eslint

Put `~/.eslintrc.json'. See `http://eslint.org/'.

Enable `js-auto-format-mode':

   (add-hook 'js-mode-hook 'js-auto-format-mode)

# Change settings

   M-x customize-group RET js-auto-format RET

# Disabled in any directories

   M-x add-dir-local-variable RET js-mode RET js-auto-format-disabled RET t
