x-dict.el provides an Emacs interface for the following dictionaries:
 URL            language
 dict.leo.org : english <-> german
 www.dict.cc  : english <-> german

x-dict.el needs my python script 'x-dict' to interact with the web interface
x-dict can be found here: http://www.xsteve.at/prg/python

The latest version of x-dict.el can be found at:
http://www.xsteve.at/prg/emacs/

Here is a nice tip from Harald Maier to select a specific coding
system for the x-dict interaction:
(add-to-list 'process-coding-system-alist '("x-dict" . latin-9))

Comments and/or patches are very welcome!
