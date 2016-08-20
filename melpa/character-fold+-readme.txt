 Extensions to Isearch character folding.


 NOTE: This library is NOT UP-TO-DATE WRT EMACS 25.  The vanilla
       Emacs library `character-fold.el', which this library
       extends, was changed in incompatible ways after this library
       was written.  I have not yet had a chance to update this
       (and am waiting for Emacs 25 to be released to do so).
       Sorry about that.


 Choose One-Way or Symmetric Character Folding
 ---------------------------------------------

 Non-nil option `char-fold-symmetric' means that char folding is
 symmetric: When you search for any of an equivalence class of
 characters you find all of them.  This behavior applies to
 query-replacing also - see option `replace-character-fold'.

 The default value of `char-fold-symmetric' is `nil', which gives
 the same behavior as vanilla Emacs: you find all members of the
 equivalence class only when you search for the base character.

 For example, with a `nil' value you can search for "e" (a base
 character) to find "é", but not vice versa.  With a non-`nil'
 value you can search for either, to find itself and the other
 members of the equivalence class - the base char is not treated
 specially.

 Example non-`nil' behavior:

   Searching for any of these characters and character compositions
   in the search string finds all of them.  (Use `C-u C-x =' with
   point before a character to see complete information about it.)

     e 𝚎 𝙚 𝘦 𝗲 𝖾 𝖊 𝕖 𝔢 𝓮 𝒆 𝑒 𝐞 ｅ ㋎ ㋍ ⓔ ⒠
     ⅇ ℯ ₑ ẽ ẽ ẻ ẻ ẹ ẹ ḛ ḛ ḙ ḙ ᵉ ȩ ȩ ȇ ȇ
     ȅ ȅ ě ě ę ę ė ė ĕ ĕ ē ē ë ë ê ê é é è è

   An example of a composition is "é".  Searching for that finds
   the same matches as searching for "é" or searching for "e".

 If you also use library `isearch+.el' then you can toggle option
 `char-fold-symmetric' anytime during Isearch, using `M-s ='
 (command `isearchp-toggle-symmetric-char-fold').


 NOTE:

   To customize option `char-fold-symmetric', use either Customize
   or a Lisp function designed for customizing options, such as
   `customize-set-variable', that invokes the necessary `:set'
   function.


 CAVEAT:

   Be aware that character-fold searching can be much slower when
   symmetric - there are many more possibilities to search for.
   If, for example, you search only for a single "e"-family
   character then every "e" in the buffer is a search hit (which
   means lazy-highlighting them all, by default).  Searching with a
   longer search string is much faster.

   If you also use library `isearch+.el' then you can turn off lazy
   highlighting using the toggle key `M-s h L'.  This can vastly
   improve performance when character folding is symmetric.


 Customize the Ad Hoc Character Foldings
 ---------------------------------------

 In addition to the standard equivalence classes of a base
 character and its family of diacriticals, vanilla Emacs includes a
 number of ad hoc character foldings, e.g., for different quote
 marks.

 Option `char-fold-ad-hoc' lets you customize this set of ad hoc
 foldings.  The default value is the same set provided by vanilla
 Emacs.



 Options defined here:

   `char-fold-ad-hoc', `char-fold-symmetric'.

 Non-interactive functions defined here:

   `update-char-fold-table'.

 Internal variables defined here:

   `char-fold-decomps'.


 ***** NOTE: The following function defined in `mouse.el' has
             been ADVISED HERE:

   `character-fold-to-regexp'.
