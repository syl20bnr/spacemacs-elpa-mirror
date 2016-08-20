Interface for IETF RFC document.

This package use some code from `rfcview.el'.
Thanks "Neil W.  Van Dyke"!

The features this package provide:

* Format RFC document for easy reading.
* Single keystroke for fast view.
* Render status switch.
* Smart table and content switch.
* Visit RFC link around point.
* Jump to RFC reference around point.
* Download RFC document *asynchronous*.

Below are commands you can use:

`irfc-render-toggle'         Toggle render status with RFC buffer.
`irfc-quit'                  Quit RFC buffer.
`irfc-visit'                 Ask for RFC number and visit document.
`irfc-reference-goto'        Ask for RFC reference and jump to it.
`irfc-head-goto'             Ask for heading name and jump to it.
`irfc-head-number-goto'      Ask for heading number and jump to it.
`irfc-follow'                Visit RFC document around point.
`irfc-table-jump'            Switch between table and content.
`irfc-page-goto'             Goto page.
`irfc-page-next'             Jump next page.
`irfc-page-prev'             Jump previous page.
`irfc-page-first'            Jump first page.
`irfc-page-last'             Jump last page.
`irfc-page-table'            Jump table page.
`irfc-head-next'             Jump next heading.
`irfc-head-prev'             Jump previous heading.
`irfc-rfc-link-next'         Jump next RFC link.
`irfc-rfc-link-prev'         Jump previous RFC link.
`irfc-scroll-up-one-line'    Scroll up one line.
`irfc-scroll-down-one-line'  Scroll down one line.

Tips:

You can use command `irfc-render-toggle' to toggle render status.

Command `irfc-table-jump' can switch between table and content,
example you stay cursor at *table*, and type "G" will jump corresponding
content in buffer, alike, you can stay at any content and type "G"
will jump corresponding table item.

Command `irfc-follow' will visit RFC document around point,
example you stay cursor at "[RFC3986]", and type "o" will
open rfc3986.txt in storage directory.  If have not found
this file in directory, will download from `http://www.ietf.org/rfc/'
and open it when download complete.

And command ‘irfc-follow’ can also use at title of RFC document.
Example rfc3986.txt contain “Obsoletes: 2732, 2396, 1808” at title,
you can move cursor to “2732” and type “o” will visit RFC 2732 document.
‘irfc-follow’ support below keywords in title:

       “Request for Comments:”
       “Updates:”
       “Obsoletes:”

You can use command `irfc-rfc-link-next' or `irfc-rfc-link-prev'
to jump next or previous RFC link in document.

Command `irfc-visit' will ask the user for a RFC number and will
visit that document, either from `irfc-directory', if exists, or by
downloading it.  This command can serve as entry point for Irfc,
to go to a RFC without having to visit the file or remember
whether it is already in `irfc-directory'.
And if you visit same document with your previous type, so just
hit RET, and don't need type RFC document number.

Command `irfc-reference-goto' will ask the user for a reference
number and will jump to that citation in the Normative
References/Informative References heading.

Command `irfc-head-goto' will ask the user for a heading name and
will jump to that heading.  Completion list in minibuffer is
available.

Command `irfc-head-number-goto' will ask the user for a heading
number and will jump to that heading.  Completion list in minibuffer
is available.



Installation:

Put irfc.el to your load-path.
The load-path is usually ~/elisp/.
It's set in your ~/.emacs like this:
(add-to-list 'load-path (expand-file-name "~/elisp"))

And the following to your ~/.emacs startup file.

(require 'irfc)

Setup your storage directory for RFC documents.

(setq irfc-directory "YourStorageDirectory")

If you want make RFC document load `irfc-mode' automatically,
setup like below:

(setq irfc-assoc-mode t)
