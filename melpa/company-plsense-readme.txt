Company backend for Perl.
Analysis is done through PlSense located at <https://github.com/aki2o/plsense>.
Note that aki2o already has an Emacs autocompletion backend for autocomple.el
located at <https://github.com/aki2o/emacs-plsense>
To use this package you must download PlSense and go through the setup.

If there are libraries your Perl code uses that are not part of @INC make
sure they are either part of lib-path in `company-plsense-config-path' (Note
that this variable can only take one path) or they are part of the $PERL5LIB
within the Emacs environment. If you make changes to $PERL5LIB you will need
to restart the PlSense server to apply the changes.
