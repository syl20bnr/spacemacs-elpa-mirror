This is a thin wrapper of [cssfmt](https://github.com/morishitter/cssfmt)

Installation:
1. install cssfmt. If you have installed npm, just type `npm install -g cssfmt`
2 Add your init.el
  (load "path/to/cssfmt.el)
  ;optional
  (add-hook 'css-mode-hook 'cssfmt-enable-on-save)
