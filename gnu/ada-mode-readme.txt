Emacs Ada mode version 5.3.1

Ada mode provides auto-casing, fontification, navigation, and
indentation for Ada source code files.

Cross-reference information output by the compiler is used to provide
powerful code navigation (jump to definition, find all uses, etc). By
default, only the AdaCore GNAT compiler is supported; other compilers
can be supported. Ada mode uses gpr_query to query compiler-generated
cross reference information. gpr_query is provided as Ada source code
that must be compiled and installed; see ada-mode.info section
Installation for instructions.

Ada mode will be automatically loaded when you open a file
with a matching extension (default *.ads, *.adb).

Ada mode uses project files to define large (multi-directory)
projects, and to define casing exceptions.

Ada mode uses a parser implemented in elisp, which can be slow on
large files. In addition, the parser does not recover from syntax
errors. There is support for running the AdaCore GPS indentation
engine in a subprocess, either as a backup when the elisp parser
fails, or as the primary indentation engine. The GPS indentation
engine must be compiled; see ada-mode.info section ada-gps for
instructions.
   
See ada-mode.info for help on using and customizing Ada mode, and
notes for Ada mode developers.

