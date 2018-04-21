This is a simple package for quickly insert template strings.

Bind the key you prefer to the command muban-apply:
(local-set-key (kbd "somekey") 'muban-apply)

A simple example:
Save the following content in ~/.emacs.muban:
#muban-begin exam#0ple
#0<img src=@url@>#0

Then you can insert
<img src="url">
...other 8 times...
<img src="url">
simply by typing exam10ple at the insertion point
and execute 'muban-apply (better to bind some key).
Use TAB to quickly modify the content of "url".

For detailed explanations and more examples see the homepage:
https://github.com/jiahaowork/muban.el
