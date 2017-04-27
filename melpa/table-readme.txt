-------------
Introduction:
-------------

This package provides text based table creation and editing
feature.  With this package Emacs is capable of editing tables that
are embedded inside a text document, the feature similar to the
ones seen in modern WYSIWYG word processors.  A table is a
rectangular text area consisting from a surrounding frame and
content inside the frame.  The content is usually subdivided into
multiple rectangular cells, see the actual tables used below in
this document.  Once a table is recognized, editing operation
inside a table cell is confined into that specific cell's
rectangular area.  This means that typing and deleting characters
inside a cell do not affect any outside text but introduces
appropriate formatting only to the cell contents.  If necessary for
accommodating added text in the cell, the cell automatically grows
vertically and/or horizontally.  The package uses no major mode nor
minor mode for its implementation because the subject text is
localized within a buffer.  Therefore the special behaviors inside
a table cells are implemented by using local-map text property
instead of buffer wide mode-map.  Also some commonly used functions
are advised so that they act specially inside a table cell.


-----------
Background:
-----------

Paul Georgief is one of my best friends.  He became an Emacs
convert after I recommended him trying it several years ago.  Now
we both are devoted disciples of Emacsism and elisp cult.  One day
in his Emacs exploration he asked me "Tak, what is a command to
edit tables in Emacs?".  This question started my journey of this
table package development.  May the code be with me!  In the
software world Emacs is probably one of the longest lifetime record
holders.  Amazingly there have been no direct support for WYSIWYG
table editing tasks in Emacs.  Many people must have experienced
manipulating existing overwrite-mode and picture-mode for this task
and only dreamed of having such a lisp package which supports this
specific task directly.  Certainly, I have been one of them.  The
most difficult part of dealing with table editing in Emacs probably
is how to realize localized rectangular editing effect.  Emacs has
no rectangular narrowing mechanism.  Existing rect package provides
basically kill, delete and yank operations of a rectangle, which
internally is a mere list of strings.  A simple approach for
realizing the localized virtual rectangular operation is combining
rect package capability with a temporary buffer.  Insertion and
deletion of a character to a table cell can be trapped by a
function that copies the cell rectangle to a temporary buffer then
apply the insertion/deletion to the temporary contents.  Then it
formats the contents by filling the paragraphs in order to fit it
into the original rectangular area and finally copy it back to the
original buffer.  This simplistic approach has to bear with
significant performance hit.  As cell grows larger the copying
rectangle back and forth between the original buffer and the
temporary buffer becomes expensive and unbearably slow.  It was
completely impractical and an obvious failure.  An idea has been
borrowed from the original Emacs design to overcome this
shortcoming.  When the terminal screen update was slow and
expensive Emacs employed a clever algorithm to reduce actual screen
update by removing redundant redrawing operations.  Also the actual
redrawing was done only when there was enough idling time.  This
technique significantly improved the previously mentioned
undesirable situation.  Now the original buffer's rectangle is
copied into a cache buffer only once.  Any cell editing operation
is done only to the cache contents.  When there is enough idling
time the original buffer's rectangle is updated with the current
cache contents.  This delayed operation is implemented by using
Emacs's timer function.  To reduce the visual awkwardness
introduced by the delayed effect the cursor location is updated in
real-time as a user types while the cell contents remains the same
until the next idling time.  A key to the success of this approach
is how to maintain cache coherency.  As a user moves point in and
out of a cell the table buffer contents and the cache buffer
contents must be synchronized without a mistake.  By observing user
action carefully this is possible however not easy.  Once this
mechanism is firmly implemented the rest of table features grew in
relatively painless progression.  Those users who are familiar with
Emacs internals appreciate this table package more.  Because it
demonstrates how extensible Emacs is by showing something that
appears like a magic.  It lets you re-discover the potential of
Emacs.


-------------
Entry Points:
-------------

If this is the first time for you to try this package, go ahead and
load the package by M-x `load-file' RET.  Specify the package file
name "table.el".  Then switch to a new test buffer and issue the
command M-x `table-insert' RET.  It'll ask you number of columns,
number of rows, cell width and cell height.  Give some small
numbers for each of them.  Play with the resulted table for a
while.  If you have menu system find the item "Table" under "Tools"
and "Table" in the menu bar when the point is in a table cell.
Some of them are pretty intuitive and you can easily guess what
they do.  M-x `describe-function' and get the documentation of
`table-insert'.  The document includes a short tutorial.  When you
are tired of guessing how it works come back to this document
again.

To use the package regularly place this file in the site library
directory and add the next expression in your .emacs file.  Make
sure that directory is included in the `load-path'.

  (require 'table)

Have the next expression also, if you want always be ready to edit
tables inside text files.  This mechanism is analogous to
fontification in a sense that tables are recognized at editing time
without having table information saved along with the text itself.

  (add-hook 'text-mode-hook 'table-recognize)

Following is a table of entry points and brief description of each
of them.  The tables below are of course generated and edited by
using this package.  Not all the commands are bound to keys.  Many
of them must be invoked by "M-x" (`execute-extended-command')
command.  Refer to the section "Keymap" below for the commands
available from keys.

+------------------------------------------------------------------+
|                    User Visible Entry Points                     |
+-------------------------------+----------------------------------+
|           Function            |           Description            |
+-------------------------------+----------------------------------+
|`table-insert'                 |Insert a table consisting of grid |
|                               |of cells by specifying the number |
|                               |of COLUMNS, number of ROWS, cell  |
|                               |WIDTH and cell HEIGHT.            |
+-------------------------------+----------------------------------+
|`table-insert-row'             |Insert row(s) of cells before the |
|                               |current row that matches the      |
|                               |current row structure.            |
+-------------------------------+----------------------------------+
|`table-insert-column'          |Insert column(s) of cells before  |
|                               |the current column that matches   |
|                               |the current column structure.     |
+-------------------------------+----------------------------------+
|`table-delete-row'             |Delete row(s) of cells.  The row  |
|                               |must consist from cells of the    |
|                               |same height.                      |
+-------------------------------+----------------------------------+
|`table-delete-column'          |Delete column(s) of cells.  The   |
|                               |column must consist from cells of |
|                               |the same width.                   |
+-------------------------------+----------------------------------+
|`table-recognize'              |Recognize all tables in the       |
|`table-unrecognize'            |current buffer and                |
|                               |activate/inactivate them.         |
+-------------------------------+----------------------------------+
|`table-recognize-region'       |Recognize all the cells in a      |
|`table-unrecognize-region'     |region and activate/inactivate    |
|                               |them.                             |
+-------------------------------+----------------------------------+
|`table-recognize-table'        |Recognize all the cells in a      |
|`table-unrecognize-table'      |single table and                  |
|                               |activate/inactivate them.         |
+-------------------------------+----------------------------------+
|`table-recognize-cell'         |Recognize a cell.  Find a cell    |
|`table-unrecognize-cell'       |which contains the current point  |
|                               |and activate/inactivate that cell.|
+-------------------------------+----------------------------------+
|`table-forward-cell'           |Move point to the next Nth cell in|
|                               |a table.                          |
+-------------------------------+----------------------------------+
|`table-backward-cell'          |Move point to the previous Nth    |
|                               |cell in a table.                  |
+-------------------------------+----------------------------------+
|`table-span-cell'              |Span the current cell toward the  |
|                               |specified direction and merge it  |
|                               |with the adjacent cell.  The      |
|                               |direction is right, left, above or|
|                               |below.                            |
+-------------------------------+----------------------------------+
|`table-split-cell-vertically'  |Split the current cell vertically |
|                               |and create a cell above and a cell|
|                               |below the point location.         |
+-------------------------------+----------------------------------+
|`table-split-cell-horizontally'|Split the current cell            |
|                               |horizontally and create a cell on |
|                               |the left and a cell on the right  |
|                               |of the point location.            |
+-------------------------------+----------------------------------+
|`table-split-cell'             |Split the current cell vertically |
|                               |or horizontally.  This is a       |
|                               |wrapper command to the other two  |
|                               |orientation specific commands.    |
+-------------------------------+----------------------------------+
|`table-heighten-cell'          |Heighten the current cell.        |
+-------------------------------+----------------------------------+
|`table-shorten-cell'           |Shorten the current cell.         |
+-------------------------------+----------------------------------+
|`table-widen-cell'             |Widen the current cell.           |
+-------------------------------+----------------------------------+
|`table-narrow-cell'            |Narrow the current cell.          |
+-------------------------------+----------------------------------+
|`table-fixed-width-mode'       |Toggle fixed width mode.  In the  |
|                               |fixed width mode, typing inside a |
|                               |cell never changes the cell width,|
|                               |while in the normal mode the cell |
|                               |width expands automatically in    |
|                               |order to prevent a word being     |
|                               |folded into multiple lines.  Fixed|
|                               |width mode reverses video or      |
|                               |underline the cell contents for   |
|                               |its indication.                   |
+-------------------------------+----------------------------------+
|`table-query-dimension'        |Compute and report the current    |
|                               |cell dimension, current table     |
|                               |dimension and the number of       |
|                               |columns and rows in the table.    |
+-------------------------------+----------------------------------+
|`table-generate-source'        |Generate the source of the current|
|                               |table in the specified language   |
|                               |and insert it into a specified    |
|                               |buffer.                           |
+-------------------------------+----------------------------------+
|`table-insert-sequence'        |Travel cells forward while        |
|                               |inserting a specified sequence    |
|                               |string into each cell.            |
+-------------------------------+----------------------------------+
|`table-capture'                |Convert plain text into a table by|
|                               |capturing the text in the region. |
+-------------------------------+----------------------------------+
|`table-release'                |Convert a table into plain text by|
|                               |removing the frame from a table.  |
+-------------------------------+----------------------------------+
|`table-justify'                |Justify the contents of cell(s).  |
+-------------------------------+----------------------------------+
|`table-disable-advice'         |Disable all table advice by       |
|                               |removing them.                    |
+-------------------------------+----------------------------------+
|`table-enable-advice'          |Enable table advice.              |
+-------------------------------+----------------------------------+
|`table-version'                |Show the current table package    |
|                               |version.                          |
+-------------------------------+----------------------------------+


*Note*

You may find that some of commonly expected table commands are
missing such as copying a row/column and yanking it.  Those
functions can be obtained through existing Emacs text editing
commands.  Rows are easily manipulated with region commands and
columns can be copied and pasted through rectangle commands.  After
all a table is still a part of text in the buffer.  Only the
special behaviors exist inside each cell through text properties.

`table-generate-html' which appeared in earlier releases is
deprecated in favor of `table-generate-source'.  Now HTML is
treated as one of the languages used for describing the table's
logical structure.


-------
Keymap:
-------

Although this package does not use a mode it does use its own
keymap inside a table cell by way of keymap text property.  Some of
the standard basic editing commands bound to certain keys are
replaced with the table specific version of corresponding commands.
This replacement combination is listed in the constant alist
`table-command-replacement-alist' declared below.  This alist is
not meant to be user configurable but mentioned here for your
better understanding of using this package.  In addition, table
cells have some table specific bindings for cell navigation and
cell reformation.  You can find these additional bindings in the
constant `table-cell-bindings'.  Those key bound functions are
considered as internal functions instead of normal commands,
therefore they have special prefix, *table-- instead of table-, for
symbols.  The purpose of this is to make it easier for a user to
use command name completion.  There is a "normal hooks" variable
`table-cell-map-hook' prepared for users to override the default
table cell bindings.  Following is the table of predefined default
key bound commands inside a table cell.  Remember these bindings
exist only inside a table cell.  When your terminal is a tty, the
control modifier may not be available or applicable for those
special characters.  In this case use "C-cC-c", which is
customizable via `table-command-prefix', as the prefix key
sequence.  This should preceding the following special character
without the control modifier.  For example, use "C-cC-c|" instead
of "C-|".

+------------------------------------------------------------------+
|                Default Bindings in a Table Cell                  |
+-------+----------------------------------------------------------+
|  Key  |                      Function                            |
+-------+----------------------------------------------------------+
|  TAB  |Move point forward to the beginning of the next cell.     |
+-------+----------------------------------------------------------+
| "C->" |Widen the current cell.                                   |
+-------+----------------------------------------------------------+
| "C-<" |Narrow the current cell.                                  |
+-------+----------------------------------------------------------+
| "C-}" |Heighten the current cell.                                |
+-------+----------------------------------------------------------+
| "C-{" |Shorten the current cell.                                 |
+-------+----------------------------------------------------------+
| "C--" |Split current cell vertically. (one above and one below)  |
+-------+----------------------------------------------------------+
| "C-|" |Split current cell horizontally. (one left and one right) |
+-------+----------------------------------------------------------+
| "C-*" |Span current cell into adjacent one.                      |
+-------+----------------------------------------------------------+
| "C-+" |Insert row(s)/column(s).                                  |
+-------+----------------------------------------------------------+
| "C-!" |Toggle between normal mode and fixed width mode.          |
+-------+----------------------------------------------------------+
| "C-#" |Report cell and table dimension.                          |
+-------+----------------------------------------------------------+
| "C-^" |Generate the source in a language from the current table. |
+-------+----------------------------------------------------------+
| "C-:" |Justify the contents of cell(s).                          |
+-------+----------------------------------------------------------+

*Note*

When using `table-cell-map-hook' do not use `local-set-key'.

  (add-hook 'table-cell-map-hook
    (function (lambda ()
      (local-set-key [<key sequence>] '<function>))))

Above code is well known ~/.emacs idiom for customizing a mode
specific keymap however it does not work for this package.  This is
because there is no table mode in effect.  This package does not
use a local map therefor you must modify `table-cell-map'
explicitly.  The correct way of achieving above task is:

  (add-hook 'table-cell-map-hook
    (function (lambda ()
      (define-key table-cell-map [<key sequence>] '<function>))))

-----
Menu:
-----

If a menu system is available a group of table specific menu items,
"Table" under "Tools" section of the menu bar, is globally added
after this package is loaded.  The commands in this group are
limited to the ones that are related to creation and initialization
of tables, such as to insert a table, to insert rows and columns,
or recognize and unrecognize tables.  Once tables are created and
point is placed inside of a table cell a table specific menu item
"Table" appears directly on the menu bar.  The commands in this
menu give full control on table manipulation that include cell
navigation, insertion, splitting, spanning, shrinking, expansion
and unrecognizing.  In addition to above two types of menu there is
a pop-up menu available within a table cell.  The content of pop-up
menu is identical to the full table menu.  [mouse-3] is the default
button, defined in `table-cell-bindings', to bring up the pop-up
menu.  It can be reconfigured via `table-cell-map-hook'.  The
benefit of a pop-up menu is that it combines selection of the
location (which cell, where in the cell) and selection of the
desired operation into a single clicking action.


---------------------------------
Function Advising (Modification):
---------------------------------

Some functions that are desired to run specially inside a table
cell are modified by way of function advice mechanism instead of
using key binding replacement.  The reason for this is that they
are such primitive that they may often be used as a building blocks
of other commands which are not known to this package, i.e. user
defined commands in a .emacs file.  To make sure the correct
behavior of them in a table cell, those functions are slightly
modified.  When the function executes, it checks if the point is
located in a table cell.  If so, the function behaves in a slightly
modified fashion otherwise executes normally.  The drawback of this
mechanism is there is a small overhead added to these functions for
testing if the location is within a table cell or not.  Due to the
limitation of advice mechanism those built-in subr functions in a
byte compiled package are out of reach from this package.

In general, redefining (or advising) an Emacs primitive is
discouraged.  If you think those advising in this package are not
safe enough or you simply do not feel comfortable with having them
you can set the variable `table-disable-advising' to non-nil before
loading this package for the first time.  This will disable the
advising all together.

The next table lists the functions that are advised to act
specially when used in a table cell.

+------------------------------------------------------------------+
|                        Advised Functions                         |
+---------------------+--------------------------------------------+
|      Function       |             Advice Description             |
+---------------------+--------------------------------------------+
|`kill-region'        |Kill between point and mark.  When both     |
|                     |point and mark reside in a same table cell  |
|                     |the text in the region within the cell is   |
|                     |deleted and saved in the kill ring.         |
|                     |Otherwise it retains the original behavior. |
+---------------------+--------------------------------------------+
|`delete-region'      |Delete the text between point and mark.     |
|                     |When both point and mark reside in a same   |
|                     |table cell the text in the region within the|
|                     |cell is deleted.  Otherwise it retains the  |
|                     |original behavior.                          |
+---------------------+--------------------------------------------+
|`copy-region-as-kill'|Save the region as if killed, but don't kill|
|                     |it.  When both point and mark reside in a   |
|                     |same table cell the text in the region      |
|                     |within the cell is saved.  Otherwise it     |
|                     |retains the original behavior.              |
+---------------------+--------------------------------------------+
|`kill-line'          |Kill the rest of the current line within a  |
|                     |table cell when point is in an active table |
|                     |cell.  Otherwise it retains the original    |
|                     |behavior.                                   |
+---------------------+--------------------------------------------+
|`yank'               |Reinsert the last stretch of killed text    |
|                     |within a cell when point resides in a       |
|                     |cell.  Otherwise it retains the original    |
|                     |behavior.                                   |
+---------------------+--------------------------------------------+
|`beginning-of-line'  |Move point to beginning of current line     |
|                     |within a cell when current point resides in |
|                     |a cell.  Otherwise it retains the original  |
|                     |behavior.                                   |
+---------------------+--------------------------------------------+
|`end-of-line'        |Move point to end of current line within a  |
|                     |cell when current point resides in a cell.  |
|                     |Otherwise it retains the original behavior. |
+---------------------+--------------------------------------------+
|`forward-word'       |Move point forward word(s) within a cell    |
|                     |when current point resides in a cell.       |
|                     |Otherwise it retains the original behavior. |
+---------------------+--------------------------------------------+
|`backward-word'      |Move point backward word(s) within a cell   |
|                     |when current point resides in a cell.       |
|                     |Otherwise it retains the original behavior. |
+---------------------+--------------------------------------------+
|`forward-paragraph'  |Move point forward paragraph(s) within a    |
|                     |cell when current point resides in a cell.  |
|                     |Otherwise it retains the original behavior. |
+---------------------+--------------------------------------------+
|`backward-paragraph' |Move point backward paragraph(s) within a   |
|                     |cell when current point resides in a cell.  |
|                     |Otherwise it retains the original behavior. |
+---------------------+--------------------------------------------+
|`center-line'        |Center the line point is on within a cell   |
|                     |when current point resides in a             |
|                     |cell. Otherwise it retains the original     |
|                     |behavior.                                   |
+---------------------+--------------------------------------------+
|`center-region'      |Center each non-blank line between point and|
|                     |mark.  When both point and mark reside in a |
|                     |same table cell the text in the region      |
|                     |within the cell is centered.  Otherwise it  |
|                     |retains the original behavior.              |
+---------------------+--------------------------------------------+


-------------------------------
Definition of tables and cells:
-------------------------------

There is no artificial-intelligence magic in this package.  The
definition of a table and the cells inside the table is reasonably
limited in order to achieve acceptable performance in the
interactive operation under Emacs lisp implementation.  A valid
table is a rectangular text area completely filled with valid
cells.  A valid cell is a rectangle text area, which four borders
consist of valid border characters.  Cells can not be nested one to
another or overlapped to each other except sharing the border
lines.  A valid character of a cell's vertical border is either
table-cell-vertical-char `|' or table-cell-intersection-char `+'.
A valid character of a cell's horizontal border is either
table-cell-horizontal-char `-' or table-cell-intersection-char `+'.
A valid character of the four corners of a cell must be
table-cell-intersection-char `+'.  A cell must contain at least one
character space inside.  There is no restriction about the contents
of a table cell, however it is advised if possible to avoid using
any of the border characters inside a table cell.  Normally a few
boarder characters inside a table cell are harmless.  But it is
possible that they accidentally align up to emulate a bogus cell
corner on which software relies on for cell recognition.  When this
happens the software may be fooled by it and fail to determine
correct cell dimension.

Following are the examples of valid tables.

+--+----+---+     +-+     +--+-----+
|  |    |   |     | |     |  |     |
+--+----+---+     +-+     |  +--+--+
|  |    |   |             |  |  |  |
+--+----+---+             +--+--+  |
                          |     |  |
                          +-----+--+

The next five tables are the examples of invalid tables.  (From
left to right, 1. nested cells 2. overlapped cells and a
non-rectangle cell 3. non-rectangle table 4. zero width/height
cells 5. zero sized cell)

+-----+    +-----+       +--+    +-++--+    ++
|     |    |     |       |  |    | ||  |    ++
| +-+ |    |     |       |  |    | ||  |
| | | |    +--+  |    +--+--+    +-++--+
| +-+ |    |  |  |    |  |  |    +-++--+
|     |    |  |  |    |  |  |    | ||  |
+-----+    +--+--+    +--+--+    +-++--+

Although the program may recognizes some of these invalid tables,
results from the subsequent editing operations inside those cells
are not predictable and will most likely start destroying the table
structures.

It is strongly recommended to have at least one blank line above
and below a table.  For a table to coexist peacefully with
surrounding environment table needs to be separated from unrelated
text.  This is necessary for the left table to grow or shrink
horizontally without breaking the right table in the following
example.

                         +-----+-----+-----+
 +-----+-----+           |     |     |     |
 |     |     |           +-----+-----+-----+
 +-----+-----+           |     |     |     |
                         +-----+-----+-----+


-------------------------
Cell contents formatting:
-------------------------

The cell contents are formatted by filling a paragraph immediately
after characters are inserted into or deleted from a cell.  Because
of this, cell contents always remain fit inside a cell neatly.  One
drawback of this is that users do not have full control over
spacing between words and line breaking.  Only one space can be
entered between words and up to two spaces between sentences.  For
a newline to be effective the new line must form a beginning of
paragraph, otherwise it'll automatically be merged with the
previous line in a same paragraph.  To form a new paragraph the
line must start with some space characters or immediately follow a
blank line.  Here is a typical example of how to list items within
a cell.  Without a space at the beginning of each line the items
can not stand on their own.

+---------------------------------+
|Each one of the following three  |
|items starts with a space        |
|character thus forms a paragraph |
|of its own.  Limitations in cell |
|contents formatting are:         |
|                                 |
| 1. Only one space between words.|
| 2. Up to two spaces between     |
|sentences.                       |
| 3. A paragraph must start with  |
|spaces or follow a blank line.   |
|                                 |
|This paragraph stays away from   |
|the item 3 because there is a    |
|blank line between them.         |
+---------------------------------+

In the normal operation table cell width grows automatically when
certain word has to be folded into the next line if the width had
not been increased.  This normal operation is useful and
appropriate for most of the time, however, it is sometimes useful
or necessary to fix the width of table and width of table cells.
For this purpose the package provides fixed width mode.  You can
toggle between fixed width mode and normal mode by "C-!".

Here is a simple example of the fixed width mode.  Suppose we have
a table like this one.

+-----+
|     |
+-----+

In normal mode if you type a word "antidisestablishmentarianism" it
grows the cell horizontally like this.

+----------------------------+
|antidisestablishmentarianism|
+----------------------------+

 In the fixed width mode the same action produces the following
 result.  The folded locations are indicated by a continuation
 character (`\' is the default).  The continuation character is
 treated specially so it is recommended to choose a character that
 does not appear elsewhere in table cells.  This character is
 configurable via customization and is kept in the variable
 `table-word-continuation-char'.  The continuation character is
 treated specially only in the fixed width mode and has no special
 meaning in the normal mode however.

+-----+
|anti\|
|dise\|
|stab\|
|lish\|
|ment\|
|aria\|
|nism |
+-----+


-------------------
Cell Justification:
-------------------

By default the cell contents are filled with left justification and
no vertical justification.  A paragraph can be justified
individually but only horizontally.  Paragraph justification is for
appearance only and does not change any structural information
while cell justification affects table's structural information.
For cell justification a user can select horizontal justification
and vertical justification independently.  Horizontal justification
must be one of the three 'left, 'center or 'right.  Vertical
justification can be 'top, 'middle, 'bottom or 'none.  When a cell
is justified, that information is recorded as a part of text
property therefore the information is persistent as long as the
cell remains within the Emacs world.  Even copying tables by region
and rectangle manipulation commands preserve this information.
However, once the table text is saved as a file and the buffer is
killed the justification information vanishes permanently.  To
alleviate this shortcoming without forcing users to save and
maintain a separate attribute file, the table code detects
justification of each cell when recognizing a table.  This
detection is done by guessing the justification by looking at the
appearance of the cell contents.  Since it is a guessing work it
does not guarantee the perfectness but it is designed to be
practically good enough.  The guessing algorithm is implemented in
the function `table--detect-cell-alignment'.  If you have better
algorithm or idea any suggestion is welcome.


-----
Todo: (in the order of priority, some are just possibility)
-----

Fix compatibilities with other input method than quail
Resolve conflict with flyspell
Use mouse for resizing cells
A mechanism to link cells internally
Consider the use of variable width font under Emacs 21
Consider the use of `:box' face attribute under Emacs 21
Consider the use of `modification-hooks' text property instead of
rebinding the keymap
Maybe provide complete XEmacs support in the future however the
"extent" is the single largest obstacle lying ahead, read the
document in Emacs info.
(eval '(progn (require 'info) (Info-find-node "elisp" "Not Intervals")))


---------------
Acknowledgment:
---------------

Table would not have been possible without the help and
encouragement of the following spirited contributors.

Paul Georgief <pgeorgie@doctordesign.com> has been the best tester
of the code as well as the constructive criticizer.

Gerd Moellmann <gerd@gnu.org> gave me useful suggestions from Emacs
21 point of view.

Richard Stallman <rms@gnu.org> showed the initial interest in this
attempt of implementing the table feature to Emacs.  This greatly
motivated me to follow through to its completion.

Kenichi Handa <handa@etl.go.jp> kindly guided me through to
overcome many technical issues while I was struggling with quail
related internationalization problems.

Christoph Conrad <christoph.conrad@gmx.de> suggested making symbol
names consistent as well as fixing several bugs.

Paul Lew <paullew@cisco.com> suggested implementing fixed width
mode as well as multi column width (row height) input interface.

Michael Smith <smith@xml-doc.org> a well-informed DocBook user
asked for CALS table source generation and helped me following
through the work by offering valuable suggestions and testing out
the code.  Jorge Godoy <godoy@conectiva.com> has also suggested
supporting for DocBook tables.

Sebastian Rahtz <sebastian.rahtz@computing-services.oxford.ac.uk>
contributed by implementing TEI (Text Encoding Initiative XML/SGML
DTD) table source generation into `table-generate-source' group
functions.

And many other individuals who reported bugs and suggestions.
