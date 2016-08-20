# Description

This package provides a replacement for `comment-dwim', `comment-dwim-2',
which includes more comment commands than its predecessor and allows to
comment / uncomment / insert comment / kill comment / indent comment
depending on the context. The command can be repeated several times to
switch between the different possible behaviors.

# Demonstration

Go to the github page to see an animated demonstration of the command.
https://github.com/remyferre/comment-dwim-2

# How to use

As the command is unbound, you need to set up you own keybinding first,
for instance:

  (global-set-key (kbd "M-;") 'comment-dwim-2)

# Customization

Contrary to `comment-dwim', `comment-dwim-2' will by default kill an
inline comment if it encounters one when being repeated. If you prefer
the `comment-dwim' behavior (which is to reindent the inline comment),
set comment-dwim-2--inline-comment-behavior to 'reindent-comment.

  (setq comment-dwim-2--inline-comment-behavior 'reindent-comment)

Whatever you choose between killing or reindenting, the other behavior
is still made available by calling `comment-dwim-2' with a prefix
argument.
