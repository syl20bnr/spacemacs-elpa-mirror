netrunner.el fetches data from NetrunnerDB.com, making it easy to create
Android: Netrunner decklists in Emacs (like any hacker should). This package
adds a backend for Company mode, adds "card info" links into org-mode, and
allows searching for cards with Helm.

In an org-mode buffer, run `netrunner-toggle-netrunner-buffer' (or add
"# -*- netrunner-buffer: t; -*-" to the top of the buffer). Now the company
backend should work for completing card names. You could also use
`helm-netrunner' to list cards.

If you to wish to preview card images in Helm, the images has to be
downloaded locally.  The images will be placed in `netrunner-image-dir' (a
directory called netrunner-images inside your `user-emacs-directory' by
default). Use `netrunner-download-all-images' to download images.
