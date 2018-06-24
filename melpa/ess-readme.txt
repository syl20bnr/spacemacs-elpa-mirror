PURPOSE

Interface to the S, SAS, and XLisp dialects of statistical
programming languages, with potential extensions to other
languages.   Designed to be extendable to most other interactive
statistical programming situations.

BRIEF OVERVIEW

Supports structured editing of S, SAS, and XLisp (statistics
programming languages) functions that are integrated with a
running process in a buffer.

THE ESS MAILING LIST

There is an informal mailing list for discussions of ESS.  Alpha
and beta releases of ESS are also announced here.  Send mail
to ess-help-request@r-project.org to join.

OVERVIEW OF ESS

S is a statistics programming language developed at Bell Labs
particularly suited for descriptive and exploratory statistics.
s-mode is built on top of comint (the general command interpreter
mode written by Olin Shivers), and so comint.el (or comint.elc)
should be either loaded or in your load path when you invoke it.

Aside from the general features offered by comint such as
command history editing and job control, inferior S mode
allows you to dump and load S objects into and from external
files, and to display help on functions.  It also provides
name completion while you do these.  For more detailed
information see the documentation strings for inferior-ess,
inferior-ess-mode, ess-mode, and comint-mode.  There are also
many variables and hooks available for customizing (see
the variables below that have document strings that start
with an "*").

INSTALLATION
See README and S-site for details.

GETTING RELEASES OF ESS
===> http://ess.r-project.org


CREDITS.
Thanks to shiba@shun.isac.co.jp (Ken'ichi "Modal" Shibayama) for
  the indenting code.
Thanks also to maechler@stat.math.ethz.ch (Martin Maechler) for
  suggestions and bug fixes.
ess-eval-line-and-step is based on a function by Rod Ball
  (rod@marcam.dsir.govt.nz)
Also thanks from David Smith to the previous authors for all their
help and suggestions.
And thanks from Richard M. Heiberger, Kurt Hornik, Martin
Maechler, and A.J. Rossini to David Smith.

BUG REPORTS
Please report bugs to ess-bugs@r-project.org
Comments, suggestions, words of praise and large cash donations
are also more than welcome, but should generally be split between
all authors :-).
