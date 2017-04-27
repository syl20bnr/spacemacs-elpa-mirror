Add following block to your Emacs configuration

    (virtualenv-mode)

Now you are available to specify virtualenv installation path

    M-x virtualenv-mode-activate

So when you run inferior python with

    M-x run-python

process will start inside specified python installation.  You can
deactivate current virtualenv with

    M-x virtualenv-mode-deactivate
