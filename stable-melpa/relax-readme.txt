Interact with CouchDB databases from within Emacs, with ease!

M-x relax to get started. From there hit C-h b.

Installation:

Use ELPA. (http://tromey.com/elpa/install.html)

If installing by hand for some reason, place on your load path and
put this in your init file:

(autoload 'relax "relax" "Connect to the CouchDB database at db-url." t)

It depends on json.el and js.el. These are available
through ELPA or if you need to get them manually download from
http://edward.oconnor.cx/elisp/json.el and
http://download.savannah.gnu.org/releases-noredirect/espresso/espresso.el

Tested with CouchDB 0.9.
