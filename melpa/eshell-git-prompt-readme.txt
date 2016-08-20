Usage:
You can call `eshell-git-prompt-use-theme' to pick up a theme then launch
Eshell.

Add the following to your initialization file to let Eshell to use it every
time:
  (eshell-git-prompt-use-theme 'robbyrussell)

TODO
- [ ] For `eshell-prompt-regexp' hack, replace '$' with '' ('\x06')
- [ ] Make it easier to make new theme (that is, improve API)
- [ ] Test with recent version Emacs and in text-base Emacs

Note
1 You must kill all Eshell buffers and re-enter Eshell to make your new
  prompt take effect.
2 You must set `eshell-prompt-regexp' to FULLY match your Eshell prompt,
  sometimes it is impossible, especially when you don't want use any special
  characters (e.g., '$') to state the end of your Eshell prompt
3 If you set `eshell-prompt-function' or `eshell-prompt-regexp' incorrectly,
  Eshell may crashes, if it happens, you can M-x `eshell-git-prompt-use-theme'
  to revert to the default Eshell prompt, then read Note #1.
