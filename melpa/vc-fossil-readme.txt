This file contains a VC backend for the fossil version control
system.

Installation:

1. Put this file somewhere in the Emacs load-path.
2. Tell Emacs to load it when needed:
   (autoload 'vc-fossil-registered "vc-fossil")
3. Add Fossil to the list of supported backends:
   (add-to-list 'vc-handled-backends 'Fossil)

Implemented Functions
BACKEND PROPERTIES
* revision-granularity
STATE-QUERYING FUNCTIONS
* registered (file)
* state (file) - 'up-to-date 'edited 'needs-patch 'needs-merge
* dir-status-files (dir files uf)
* workfile-version (file)
* checkout-model (file)
- workfile-unchanged-p (file)
STATE-CHANGING FUNCTIONS
* register (file &optional rev comment)
* checkin (file comment &optional rev)
* find-version (file rev buffer)
* checkout (file &optional editable rev)
* revert (file &optional contents-done)
* pull (prompt)
- responsible-p (file)
HISTORY FUNCTIONS
* print-log (file &optional buffer)
* diff (file &optional rev1 rev2 buffer async)
MISCELLANEOUS
- delete-file (file)
- rename-file (old new)
