`magic-filetype' parse Vim-style file header.
For example, in executable JavaScript(node) file is...

  #!/usr/bin/env node
  // vim:set ft=javascript:
  (function(){
      "use strict";
       ....

put into your own .emacs file (init.el)

  (enable-vim-filetype)

`vim-filetype-mode-alist' have dummy filename that is delegate of major-mode.
