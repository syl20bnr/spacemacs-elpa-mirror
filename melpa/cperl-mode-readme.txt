This version of the file contains support for the syntax added by
the MooseX::Declare CPAN module, as well as Perl 5.10 keyword
support.

The latest version is available from
http://github.com/jrockway/cperl-mode

(perhaps in the moosex-declare branch)

You can either fine-tune the bells and whistles of this mode or
bulk enable them by putting

(setq cperl-hairy t)

in your .emacs file.  (Emacs rulers do not consider it politically
correct to make whistles enabled by default.)

DO NOT FORGET to read micro-docs (available from `Perl' menu)   <<<<<<
or as help on variables `cperl-tips', `cperl-problems',         <<<<<<
`cperl-praise', `cperl-speed'.				   <<<<<<

The mode information (on C-h m) provides some customization help.
If you use font-lock feature of this mode, it is advisable to use
either lazy-lock-mode or fast-lock-mode.  I prefer lazy-lock.

Faces used now: three faces for first-class and second-class keywords
and control flow words, one for each: comments, string, labels,
functions definitions and packages, arrays, hashes, and variable
definitions.  If you do not see all these faces, your font-lock does
not define them, so you need to define them manually.

This mode supports font-lock, imenu and mode-compile.  In the
hairy version font-lock is on, but you should activate imenu
yourself (note that mode-compile is not standard yet).  Well, you
can use imenu from keyboard anyway (M-x imenu), but it is better
to bind it like that:

(define-key global-map [M-S-down-mouse-3] 'imenu)

Font lock bugs as of v4.32:

The following kinds of Perl code erroneously start strings:
\$`  \$'  \$"
$opt::s  $opt_s  $opt{s}  (s => ...)  /\s+.../
likewise with m, tr, y, q, qX instead of s
