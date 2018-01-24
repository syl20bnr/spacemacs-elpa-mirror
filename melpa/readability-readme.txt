Get Readability's reading list, and read each article on Emacs.

Requirement
+ Emacs 24.3
+ Readability Account   https://readability.com/
+ FontAwesome           http://fortawesome.github.io/Font-Awesome/
+ oauth.el              https://github.com/psanford/emacs-oauth)
+ ov.el                 https://github.com/ShingoFukuyama/ov.el

Get Started
(add-to-list 'load-path "/your/path/to/emacs-readability")
(require 'readability)
1. `M-x readability-get-reading-list`.
   Your default browser will present Readability's login page
   (if you have not been logged in yet).
2. After logged in, authorize this app by clicking "Allow" button.
3. Copy request token on the browser.
4. Paste request token to Emacs mini buffer.
5. Emacs will start fetching a reading list.
6. Press "RET" key on any title to show its contents.
Once authorization successed, you don't need to login from then on.
If you would like to logout, just do `M-x readability-delete-token-and-file`.

Readability API v1 document:
+ https://www.readability.com/developers/api
