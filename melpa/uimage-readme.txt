Uimage is a iimange like minor mode but could displays url images

** Display images in *Info* buffer.

(add-hook 'info-mode-hook 'uimage-mode)

.texinfo:   @file{file://foo.png}
.texinfo:   @file{http://xxx.com/foo.png}
.texinfo:   @file{https://xxx.com/foo.png}
.info:      `file://foo.png'
.info:      `http://xxx.com/foo.png'
.info:      `https://xxx.com/foo.png'

** Display images in Wiki buffer.

(add-hook 'wiki-mode-hook 'uimage-mode)

wiki-file:   [[file://foo.png]]
wiki-file:   [[http://xxx.com/foo.png]]
wiki-file:   [[https://xxx.com/foo.png]]
