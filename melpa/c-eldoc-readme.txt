To enable: put the following in your .emacs file:

(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

Chinmay Kamat -- made changes to the regular expression to make sure that function calls in macros do not
override actual function definitions while searching
v0.6 20/05/2010

Nathaniel has submitted a caching patch to make this workable on large projects "like the emacs
codebase"
v0.5 01/02/2010

Provides helpful description of the arguments to C functions.
Uses child process grep and preprocessor commands for speed.
v0.4 01/16/2005

Your improvements are appreciated: I am no longer maintaining this code
m_strange at mail dot utexas dot edu.  Instead, direct all requests to
flat0103@gmail.com
