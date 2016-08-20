Dim autoload cookies.

Unlike the built-in font-lock keyword which only changes the
appearance of the "autoload" substring of the autoload cookie
line and repurposes the warning face also used elsewhere, the
keyword added here changes the appearance of the complete line
using a dedicated face.

That face is intended for dimming.  While you are making sure
your library contains all the required autoload cookies you can
just turn it off.

To install the keywords add this to your init file:

   (global-dim-autoload-cookies-mode 1)

You might even want to dim the cookie lines some more by using
a foreground color in `dim-autoload-cookies-line' that is very
close to the `default' background color.
