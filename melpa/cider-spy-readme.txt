Get visibility on CIDER nREPL sessions and help developers in teams
to share information, send code snippets and text exchanges to each
other etc.

# Installation

## Prerequisites

You need the [`CIDER-SPY-NREPL`](https://github.com/jonpither/cider-spy-nrepl)
middleware. See installation instructions there.

## Basic configuration

It's available on [melpa](http://melpa.milkbox.net/):

    M-x package-install cider-spy

You can also install the dependencies on your own, and just dump
clj-refactor in your path somewhere:

 - <a href="https://github.com/magnars/dash.el">dash.el</a>
 - <a href="https://github.com/clojure-emacs/cider">cider</a>

# Setup

    (require 'cider-spy)

All actions in `CIDER-SPY` are triggered from the `CIDER-SPY` summary page.
To access the summary page:

    M-x cider-spy-summary

It can be useful to setup a global binding for the summary page for frequent
access, such as <kbd>C-c C-s</kbd>.

## Configuration for the HUB

If you want the developer interactivity behavours then you need a run a `CIDER-SPY-HUB`.
See the documentation for how to set one up.


# Keyboard Shortcuts

These shortcuts are available on the `CIDER-SPY` summary buffer:

 - `g` : Refresh the `*cider-spy*` buffer
 - `r` : Reset the tracking data underpinning the `*cider-spy*` buffer
 - `n` : Goto to next section
 - `p` : Goto to previous section
 - `a` : Set `CIDER-SPY-HUB` alias
 - `s` : Send message to another dev (when cursor is on a dev)
 - `w` : Watch another devs REPL session (when cursor is on a dev)
 - `d` : Disconnect from the `CIDER-SPY-HUB`
 - `RETURN` : Visit section
 - `TAB` : Toggle section visibility
