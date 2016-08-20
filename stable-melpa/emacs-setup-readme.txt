emacs-setup is an emacs package that is meant to help make maintaining your
emacs setup easier. Through the use of M-x customize, the following can be
setup through emacs-setup:

Add/remove directories to the load path.
Add/remove directories to the environment PATH.
Add/remove packages to require, including any accompanying setup elisp code.
Set/unset and save keybindings.

Installation:

The package can be installed with M-x package-list-packages (requires
package.el, which is included for emacs 24 but availble for 23). The MELPA
repository must be added to your packages setup first. Instructions can be found
at http://melpa.milkbox.net/

Alternatively, the source can be pulled direclty from github:
https://github.com/echosa/emacs-setup

If you install via package-list-packages, revive.el will be installed for you.
If you do not, you will need to manually get, install, and load revive.el.
You can find it at http://www.gentei.org/~yuuji/software/revive.el

Usage:

In your .emacs, load emacs-setup:

(load-file "~/path/to/emacs-setup/emacs-setup.el")

Once loaded, you can use M-x customize-group emacs-setup to setup your
environment.

emacs-setup is broken down into several parts, which can each be customized
individually:

emacs-setup - This is the main part of emacs-setup. You can set your base
              directory (your .emacs.d or equivalent), directories to ignore
              when recursively adding to load path, and various list of
              s-expressions (base, pre, post, etc.) The s-expression lists
              can be used to setup things that would normally be in your
              .emacs, but are not customizable options. For instance,
              (set-frame-font), (set-background-color), (transient-mark-mode),
              etc. I'm not going to try an support every option of emacs.
              Instead, simply add these configuration lines (one sexp per line)
              to the appropriate sexp group, depending on when they need to run.
              When emacs-setup-base is run, the last thing it does is run all
              the s-expressions in emacs-setup-base-sexp. When emacs-setup is
              run, it runs in this order:
              - emacs-setup-pre-sexp
              - require pacakges via emacs-setup-require
              - emacs-setup-post-sexp
              - bind keys in emacs-setup-keys

emacs-setup-keys - This part of emacs-setup allows you to have your keybindings
                   all in one place via customize. You can manually add and
                   remove keybindings, or you can use the functions
                   emacs-setup-bind-key,
                   emacs-seutp-unbind-key-by-key, or
                   emacs-setup-unbind-key-by-functions
                   to interactively bind or unbind keys, which are saved to
                   customize for you.

emacs-setup-require - This is ths part of emacs-setup where you can tell it
                      which packages to load, and give setup s-expressions.
                      You can customize the load-path and env-path, whether or
                      not to loade elpa and where your package.el is (if not
                      using emacs 24). Customizing the variable
                      emacs-setup-require-list
                      is where you can add which packages should be load, in
                      the order you supply them, as well as any configuration
                      for each package after it is loaded.
                      When emacs-setup is run, if any pacakges fail to load, a
                      buffer called *invalid-packages* will be displayed telling
                      you which failed.

emacs-setup is written and maintained by Brian Zwahr <echosa@gmail.com>
