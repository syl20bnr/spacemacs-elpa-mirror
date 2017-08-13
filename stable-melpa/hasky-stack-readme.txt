This is an Interface to the Stack Haskell development tool.  Bind just
two commands, like this:

    (global-set-key (kbd "<next> h e") #'hasky-stack-execute)
    (global-set-key (kbd "<next> h i") #'hasky-stack-new)

* `hasky-stack-execute' opens a popup with a collection of stack commands
  you can run.  Many commands have their own popups like in Magit.

* `hasky-stack-new' allows to create a new project in current directory
  using a Stack template.
