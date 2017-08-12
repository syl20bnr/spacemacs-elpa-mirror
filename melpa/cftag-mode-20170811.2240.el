;;; cftag-mode.el --- Emacs mode for editing tag-based CFML files

;; Copyright 2017 Andrew Myers

;; Author: Andrew Myers <am2605@gmail.com>
;; URL: https://github.com/am2605/cftag-mode
;; Package-Version: 20170811.2240
;; Version: 1.0.0
;; Package-Requires: ((emacs "25"))

;;{{{ GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;}}}

;;; Commentary:

;; This file redefines html-mode with appropriate adjustments for CFML.

;;; Code:

(require 'sgml-mode)

(defcustom cftag-mode-hook nil
  "Hook run by command `cftag-mode'.
`text-mode-hook' and `sgml-mode-hook' are run first."
  :group 'sgml
  :type 'hook
  :options '(cftag-autoview-mode))

;; This file contains definitions of CFML submode classes.

(defvar cftag-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap "HTML")))
    (set-keymap-parent map  sgml-mode-map)
    map)
  "Keymap for commands for use in HTML mode.")

(defvar cftag-face-tag-alist
  '((bold . "b")
    (italic . "i")
    (underline . "u")
    (mode-line . "rev"))
  "Value of `sgml-face-tag-alist' for HTML mode.")

(defvar cftag-tag-face-alist
  '(("b" . bold)
    ("big" . bold)
    ("blink" . highlight)
    ("cite" . italic)
    ("em" . italic)
    ("h1" bold underline)
    ("h2" bold-italic underline)
    ("h3" italic underline)
    ("h4" . underline)
    ("h5" . underline)
    ("h6" . underline)
    ("i" . italic)
    ("rev"  . mode-line)
    ("s" . underline)
    ("small" . default)
    ("strong" . bold)
    ("title" bold underline)
    ("tt" . default)
    ("u" . underline)
    ("var" . italic))
  "Value of `sgml-tag-face-alist' for HTML mode.")

(defvar cftag-display-text
  '((img . "[/]")
    (hr . "----------")
    (li . "o "))
  "Value of `sgml-display-text' for HTML mode.")


(defvar cftag-tag-alist
  (let* ((1-7 '(("1") ("2") ("3") ("4") ("5") ("6") ("7")))
	 (1-9 `(,@1-7 ("8") ("9")))
	 (align '(("align" ("left") ("center") ("right"))))
         (ialign '(("align" ("top") ("middle") ("bottom") ("left")
                    ("right"))))
	 (valign '(("top") ("middle") ("bottom") ("baseline")))
	 (rel '(("next") ("previous") ("parent") ("subdocument") ("made")))
	 (href '("href" ("ftp:") ("file:") ("finger:") ("gopher:") ("http:")
		 ("mailto:") ("news:") ("rlogin:") ("telnet:") ("tn3270:")
		 ("wais:") ("/cgi-bin/")))
	 (name '("name"))
	 (link `(,href
		 ("rel" ,@rel)
		 ("rev" ,@rel)
		 ("title")))
	 (list '((nil \n ("List item: " "<li>" str
                          (if sgml-xml-mode "</li>") \n))))
         (shape '(("shape" ("rect") ("circle") ("poly") ("default"))))
	 (cell `(t
		 ,@align
		 ("valign" ,@valign)
		 ("colspan" ,@1-9)
		 ("rowspan" ,@1-9)
		 ("nowrap" t)))
         (cellhalign '(("align" ("left") ("center") ("right")
                        ("justify") ("char"))
                       ("char") ("charoff")))
         (cellvalign '(("valign" ("top") ("middle") ("bottom")
                        ("baseline")))))
    ;; put ,-expressions first, else byte-compile chokes (as of V19.29)
    ;; and like this it's more efficient anyway
    `(("a" ,name ,@link)
      ("area" t ,@shape ("coords") ("href") ("nohref" "nohref") ("alt")
       ("tabindex") ("accesskey") ("onfocus") ("onblur"))
      ("base" t ,@href)
      ("col" t ,@cellhalign ,@cellvalign ("span") ("width"))
      ("colgroup" \n ,@cellhalign ,@cellvalign ("span") ("width"))
      ("dir" ,@list)
      ("figcaption")
      ("figure" \n)
      ("font" nil "size" ("-1") ("+1") ("-2") ("+2") ,@1-7)
      ("form" (\n _ \n "<input type=\"submit\" value=\"\""
	       (if sgml-xml-mode " />" ">"))
       ("action" ,@(cdr href)) ("method" ("get") ("post")))
      ("h1" ,@align)
      ("h2" ,@align)
      ("h3" ,@align)
      ("h4" ,@align)
      ("h5" ,@align)
      ("h6" ,@align)
      ("hr" t ("size" ,@1-9) ("width") ("noshade" t) ,@align)
      ("iframe" \n ,@ialign ("longdesc") ("name") ("src")
       ("frameborder" ("1") ("0")) ("marginwidth") ("marginheight")
       ("scrolling" ("yes") ("no") ("auto")) ("height") ("width"))
      ("img" t ("align" ,@valign ("texttop") ("absmiddle") ("absbottom"))
       ("src") ("alt") ("width" "1") ("height" "1")
       ("border" "1") ("vspace" "1") ("hspace" "1") ("ismap" t))
      ("input" t ,name ("accept") ("alt") ("autocomplete" ("on") ("off"))
       ("autofocus" t) ("checked" t) ("dirname") ("disabled" t) ("form")
       ("formaction")
       ("formenctype" ("application/x-www-form-urlencoded")
        ("multipart/form-data") ("text/plain"))
       ("formmethod" ("get") ("post"))
       ("formnovalidate" t)
       ("formtarget" ("_blank") ("_self") ("_parent") ("_top"))
       ("height") ("inputmode") ("list") ("max") ("maxlength") ("min")
       ("minlength") ("multiple" t) ("pattern") ("placeholder")
       ("readonly" t) ("required" t) ("size") ("src") ("step")
       ("type" ("hidden") ("text") ("search") ("tel") ("url") ("email")
        ("password") ("date") ("time") ("number") ("range") ("color")
        ("checkbox") ("radio") ("file") ("submit") ("image") ("reset")
        ("button"))
       ("value") ("width"))
      ("link" t ,@link)
      ("menu" ,@list)
      ("ol" ,@list ("type" ("A") ("a") ("I") ("i") ("1")))
      ("p" t ,@align)
      ("select" (nil \n
		     ("Text: "
		      "<option>" str (if sgml-xml-mode "</option>") \n))
       ,name ("size" ,@1-9) ("multiple" t))
      ("table" (nil \n
		    ((completing-read "Cell kind: " '(("td") ("th"))
				      nil t "t")
		     "<tr><" str ?> _
		     (if sgml-xml-mode (concat "<" str "></tr>")) \n))
       ("border" t ,@1-9) ("width" "10") ("cellpadding"))
      ("tbody" \n ,@cellhalign ,@cellvalign)
      ("td" ,@cell)
      ("textarea" ,name ("rows" ,@1-9) ("cols" ,@1-9))
      ("tfoot" \n ,@cellhalign ,@cellvalign)
      ("th" ,@cell)
      ("thead" \n ,@cellhalign ,@cellvalign)
      ("ul" ,@list ("type" ("disc") ("circle") ("square")))

      ,@sgml-tag-alist

      ("abbr")
      ("acronym")
      ("address")
      ("array" (nil \n
		    ("Item: " "<item>" str (if sgml-xml-mode "</item>") \n))
       "align")
      ("article" \n)
      ("aside" \n)
      ("au")
      ("audio" \n
       ("src") ("crossorigin" ("anonymous") ("use-credentials"))
       ("preload" ("none") ("metadata") ("auto"))
       ("autoplay" "autoplay") ("mediagroup") ("loop" "loop")
       ("muted" "muted") ("controls" "controls"))
      ("b")
      ("bdi")
      ("bdo" nil ("lang") ("dir" ("ltr") ("rtl")))
      ("big")
      ("blink")
      ("blockquote" \n ("cite"))
      ("body" \n ("background" ".gif") ("bgcolor" "#") ("text" "#")
       ("link" "#") ("alink" "#") ("vlink" "#"))
      ("box" (nil _ "<over>" _ (if sgml-xml-mode "</over>")))
      ("br" t ("clear" ("left") ("right")))
      ("button" nil ("name") ("value")
       ("type" ("submit") ("reset") ("button"))
       ("disabled" "disabled")
       ("tabindex") ("accesskey") ("onfocus") ("onblur"))
      ("canvas" \n ("width") ("height"))
      ("caption" ("valign" ("top") ("bottom")))
      ("center" \n)
      ("cite")
      ("code" \n)
      ("datalist" \n)
      ("dd" ,(not sgml-xml-mode))
      ("del" nil ("cite") ("datetime"))
      ("dfn")
      ("div")
      ("dl" (nil \n
		 ( "Term: "
		   "<dt>" str (if sgml-xml-mode "</dt>")
                   "<dd>" _ (if sgml-xml-mode "</dd>") \n)))
      ("dt" (t _ (if sgml-xml-mode "</dt>")
             "<dd>" (if sgml-xml-mode "</dd>") \n))
      ("em")
      ("embed" t ("src") ("type") ("width") ("height"))
      ("fieldset" \n)
      ("fn" "id" "fn")  ;; Footnotes were deprecated in HTML 3.2
      ("footer" \n)
      ("frame" t ("longdesc") ("name") ("src")
       ("frameborder" ("1") ("0")) ("marginwidth") ("marginheight")
       ("noresize" "noresize") ("scrolling" ("yes") ("no") ("auto")))
      ("frameset" \n ("rows") ("cols") ("onload") ("onunload"))
      ("head" \n)
      ("header" \n)
      ("hgroup" \n)
      ("html" (\n
	       "<head>\n"
	       "<title>" (setq str (read-string "Title: ")) "</title>\n"
	       "</head>\n"
	       "<body>\n<h1>" str "</h1>\n" _
	       "\n<address>\n<a href=\"mailto:"
	       user-mail-address
	       "\">" (user-full-name) "</a>\n</address>\n"
	       "</body>"
		))
      ("i")
      ("ins" nil ("cite") ("datetime"))
      ("isindex" t ("action") ("prompt"))
      ("kbd")
      ("label" nil ("for") ("accesskey") ("onfocus") ("onblur"))
      ("lang")
      ("legend" nil ("accesskey"))
      ("li" ,(not sgml-xml-mode))
      ("main" \n)
      ("map" \n ("name"))
      ("mark")
      ("math" \n)
      ("meta" t ("http-equiv") ("name") ("content") ("scheme"))
      ("meter" nil ("value") ("min") ("max") ("low") ("high")
       ("optimum"))
      ("nav" \n)
      ("nobr")
      ("noframes" \n)
      ("noscript" \n)
      ("object" \n ("declare" "declare") ("classid") ("codebase")
       ("data") ("type") ("codetype") ("archive") ("standby")
       ("height") ("width") ("usemap") ("name") ("tabindex"))
      ("optgroup" \n ("name") ("size") ("multiple" "multiple")
       ("disabled" "disabled") ("tabindex") ("onfocus") ("onblur")
       ("onchange"))
      ("option" t ("value") ("label") ("selected" t))
      ("output" nil ("for") ("form") ("name"))
      ("over" t)
      ("param" t ("name") ("value")
       ("valuetype" ("data") ("ref") ("object")) ("type"))
      ("person") ;; Tag for person's name tag deprecated in HTML 3.2
      ("pre" \n)
      ("progress" nil ("value") ("max"))
      ("q" nil ("cite"))
      ("rev")
      ("rp" t)
      ("rt" t)
      ("ruby")
      ("s")
      ("samp")
      ("script" nil ("charset") ("type") ("src") ("defer" "defer"))
      ("section" \n)
      ("small")
      ("source" t ("src") ("type") ("media"))
      ("span" nil
	("class"
	 ("builtin")
	 ("comment")
	 ("constant")
	 ("function-name")
	 ("keyword")
	 ("string")
	 ("type")
	 ("variable-name")
	 ("warning")))
      ("strong")
      ("style" \n ("type") ("media") ("title"))
      ("sub")
      ("summary")
      ("sup")
      ("time" nil ("datetime"))
      ("title")
      ("tr" t)
      ("track" t
       ("kind" ("subtitles") ("captions") ("descriptions")
        ("chapters") ("metadata"))
       ("src") ("srclang") ("label") ("default"))
      ("tt")
      ("u")
      ("var")
      ("video" \n
       ("src") ("crossorigin" ("anonymous") ("use-credentials"))
       ("poster") ("preload" ("none") ("metadata") ("auto"))
       ("autoplay" "autoplay") ("mediagroup") ("loop" "loop")
       ("muted" "muted") ("controls" "controls") ("width") ("height"))
      ("wbr" t)))
  "Value of `sgml-tag-alist' for HTML mode.")

(defvar cftag-tag-help
  `(,@sgml-tag-help
    ("a" . "Anchor of point or link elsewhere")
    ("abbr" . "Abbreviation")
    ("acronym" . "Acronym")
    ("address" . "Formatted mail address")
    ("area" . "Region of an image map")
    ("array" . "Math array")
    ("article" . "An independent part of document or site")
    ("aside" . "Secondary content related to surrounding content (e.g. page or article)")
    ("au" . "Author")
    ("audio" . "Sound or audio stream")
    ("b" . "Bold face")
    ("base" . "Base address for URLs")
    ("bdi" . "Text isolated for bidirectional formatting")
    ("bdo" . "Override text directionality")
    ("big" . "Font size")
    ("blink" . "Blinking text")
    ("blockquote" . "Indented quotation")
    ("body" . "Document body")
    ("box" . "Math fraction")
    ("br" . "Line break")
    ("button" . "Clickable button")
    ("canvas" . "Script generated graphics canvas")
    ("caption" . "Table caption")
    ("center" . "Centered text")
    ("changed" . "Change bars")
    ("cite" . "Citation of a document")
    ("code" . "Formatted source code")
    ("col" . "Group of attribute specifications for table columns")
    ("colgroup" . "Group of columns")
    ("datalist" . "A set of predefined options")
    ("dd" . "Definition of term")
    ("del" . "Deleted text")
    ("dfn" . "Defining instance of a term")
    ("dir" . "Directory list (obsolete)")
    ("div" . "Generic block-level container")
    ("dl" . "Definition list")
    ("dt" . "Term to be defined")
    ("em" . "Emphasized")
    ("embed" . "Embedded data in foreign format")
    ("fieldset" . "Group of related controls and labels")
    ("fig" . "Figure")
    ("figa" . "Figure anchor")
    ("figcaption" . "Caption for a figure")
    ("figd" . "Figure description")
    ("figt" . "Figure text")
    ("figure" . "Self-contained content, often with a caption")
    ("fn" . "Footnote")  ;; No one supports special footnote rendering.
    ("font" . "Font size")
    ("footer" . "Footer of a section")
    ("form" . "Form with input fields")
    ("frame" . "Frame in which another HTML document can be displayed")
    ("frameset" . "Container for frames")
    ("group" . "Document grouping")
    ("h1" . "Most important section headline")
    ("h2" . "Important section headline")
    ("h3" . "Section headline")
    ("h4" . "Minor section headline")
    ("h5" . "Unimportant section headline")
    ("h6" . "Least important section headline")
    ("head" . "Document header")
    ("header" . "Header of a section")
    ("hgroup" . "Group of headings - h1-h6 elements")
    ("hr" . "Horizontal rule")
    ("html" . "HTML Document")
    ("i" . "Italic face")
    ("iframe" . "Inline frame with a nested browsing context")
    ("img" . "Graphic image")
    ("input" . "Form input field")
    ("ins" . "Inserted text")
    ("isindex" . "Input field for index search")
    ("kbd" . "Keyboard example face")
    ("label" . "Caption for a user interface item")
    ("lang" . "Natural language")
    ("legend" . "Caption for a fieldset")
    ("li" . "List item")
    ("link" . "Link relationship")
    ("main" . "Main content of the document body")
    ("map" . "Image map (a clickable link area")
    ("mark" . "Highlighted text")
    ("math" . "Math formula")
    ("menu" . "List of commands")
    ("meta" . "Document properties")
    ("meter" . "Scalar measurement within a known range")
    ("mh" . "Form mail header")
    ("nav" . "Group of navigational links")
    ("nextid" . "Allocate new id")
    ("nobr" . "Text without line break")
    ("noframes" . "Content for user agents that don't support frames")
    ("noscript" . "Alternate content for when a script isn't executed")
    ("object" . "External resource")
    ("ol" . "Ordered list")
    ("optgroup" . "Group of options")
    ("option" . "Selection list item")
    ("output" . "Result of a calculation or user action")
    ("over" . "Math fraction rule")
    ("p" . "Paragraph start")
    ("panel" . "Floating panel")
    ("param" . "Parameters for an object")
    ("person" . "Person's name")
    ("pre" . "Preformatted fixed width text")
    ("progress" . "Completion progress of a task")
    ("q" . "Quotation")
    ("rev" . "Reverse video")
    ("rp" . "Fallback text for when ruby annotations aren't supported")
    ("rt" . "Ruby text component of a ruby annotation")
    ("ruby" . "Ruby annotation")
    ("s" . "Strikeout")
    ("samp" . "Sample text")
    ("script" . "Executable script within a document")
    ("section" . "Section of a document")
    ("select" . "Selection list")
    ("small" . "Font size")
    ("source" . "Media resource for media elements")
    ("sp" . "Nobreak space")
    ("span" . "Generic inline container")
    ("strong" . "Standout text")
    ("style" . "Style information")
    ("sub" . "Subscript")
    ("summary" . "Summary, caption, or legend")
    ("sup" . "Superscript")
    ("table" . "Table with rows and columns")
    ("tb" . "Table vertical break")
    ("tbody" . "Table body")
    ("td" . "Table data cell")
    ("textarea" . "Form multiline edit area")
    ("tfoot" . "Table foot")
    ("th" . "Table header cell")
    ("thead" . "Table head")
    ("time" . "Content with optional machine-readable timestamp")
    ("title" . "Document title")
    ("tr" . "Table row separator")
    ("track" . "Timed text track for media elements")
    ("tt" . "Typewriter face")
    ("u" . "Underlined text")
    ("ul" . "Unordered list")
    ("var" . "Math variable face")
    ("video" . "Video or movie")
    ("wbr" . "Enable <br> within <nobr>"))
  "Value of variable `sgml-tag-help' for HTML mode.")

(defvar outline-regexp)
(defvar outline-heading-end-regexp)
(defvar outline-level)

(defun cftag-current-defun-name ()
  "Return the name of the last HTML title or heading, or nil."
  (save-excursion
    (if (re-search-backward
	 (concat
	  "<[ \t\r\n]*"
	  "\\(?:[hH][0-6]\\|title\\|TITLE\\|Title\\)"
	  "[^>]*>"
	  "[ \t\r\n]*"
	  "\\([^<\r\n]*[^ <\t\r\n]+\\)")
	 nil t)
	(match-string-no-properties 1))))

(defvar cftag--buffer-classes-cache nil
  "Cache for `cftag-current-buffer-classes'.
When set, this should be a cons cell where the CAR is the
buffer's tick counter (as produced by `buffer-modified-tick'),
and the CDR is the list of class names found in the buffer.")
(make-variable-buffer-local 'cftag--buffer-classes-cache)

(defvar cftag--buffer-ids-cache nil
  "Cache for `cftag-current-buffer-ids'.
When set, this should be a cons cell where the CAR is the
buffer's tick counter (as produced by `buffer-modified-tick'),
and the CDR is the list of class names found in the buffer.")
(make-variable-buffer-local 'cftag--buffer-ids-cache)

(defun cftag-current-buffer-classes ()
  "Return a list of class names used in the current buffer.
The result is cached in `cftag--buffer-classes-cache'."
  (let ((tick (buffer-modified-tick)))
    (if (eq (car cftag--buffer-classes-cache) tick)
        (cdr cftag--buffer-classes-cache)
      (let* ((dom (libxml-parse-cftag-region (point-min) (point-max)))
             (classes
              (seq-mapcat
               (lambda (el)
                 (when-let (class-list
                            (cdr (assq 'class (dom-attributes el))))
                   (split-string class-list)))
               (dom-by-class dom ""))))
        (setq-local cftag--buffer-classes-cache (cons tick classes))
        classes))))

(defun cftag-current-buffer-ids ()
  "Return a list of IDs used in the current buffer.
The result is cached in `cftag--buffer-ids-cache'."
  (let ((tick (buffer-modified-tick)))
    (if (eq (car cftag--buffer-ids-cache) tick)
        (cdr cftag--buffer-ids-cache)
      (let* ((dom
              (libxml-parse-cftag-region (point-min) (point-max)))
             (ids
              (seq-mapcat
               (lambda (el)
                 (when-let (id-list
                            (cdr (assq 'id (dom-attributes el))))
                   (split-string id-list)))
               (dom-by-id dom ""))))
        (setq-local cftag--buffer-ids-cache (cons tick ids))
        ids))))


;;;###autoload
(define-derived-mode cftag-mode sgml-mode '(sgml-xml-mode "XHTML" "CFML")
  "Major mode based on SGML mode for editing HTML documents.
This allows inserting skeleton constructs used in hypertext documents with
completion.  See below for an introduction to HTML.  Use
\\[browse-url-of-buffer] to see how this comes out.  See also `sgml-mode' on
which this is based.

Do \\[describe-variable] cftag- SPC and \\[describe-variable] sgml- SPC to see available variables.

To write fairly well formatted pages you only need to know few things.  Most
browsers have a function to read the source code of the page being seen, so
you can imitate various tricks.  Here's a very short HTML primer which you
can also view with a browser to see what happens:

<title>A Title Describing Contents</title> should be on every page.  Pages can
have <h1>Very Major Headlines</h1> through <h6>Very Minor Headlines</h6>
<hr> Parts can be separated with horizontal rules.

<p>Paragraphs only need an opening tag.  Line breaks and multiple spaces are
ignored unless the text is <pre>preformatted.</pre>  Text can be marked as
<b>bold</b>, <i>italic</i> or <u>underlined</u> using the normal M-o or
Edit/Text Properties/Face commands.

Pages can have <a name=\"SOMENAME\">named points</a> and can link other points
to them with <a href=\"#SOMENAME\">see also somename</a>.  In the same way <a
href=\"URL\">see also URL</a> where URL is a filename relative to current
directory, or absolute as in `http://www.cs.indiana.edu/elisp/w3/docs.html'.

Images in many formats can be inlined with <img src=\"URL\">.

If you mainly create your own documents, `sgml-specials' might be
interesting.  But note that some HTML 2 browsers can't handle `&apos;'.
To work around that, do:
   (eval-after-load \"sgml-mode\" \\='(aset sgml-char-names ?\\=' nil))

\\{cftag-mode-map}"
  (setq-local sgml-display-text cftag-display-text)
  (setq-local sgml-tag-face-alist cftag-tag-face-alist)
  (setq-local sgml-tag-alist cftag-tag-alist)
  (setq-local sgml-face-tag-alist cftag-face-tag-alist)
  (setq-local sgml-tag-help cftag-tag-help)
  (setq-local outline-regexp "^.*<[Hh][1-6]\\>")
  (setq-local outline-heading-end-regexp "</[Hh][1-6]>")
  (setq-local outline-level
	      (lambda () (char-before (match-end 0))))
  (setq-local add-log-current-defun-function #'cftag-current-defun-name)
  (setq-local sentence-end-base "[.?!][]\"'”)}]*\\(<[^>]*>\\)*")

  (when (fboundp 'libxml-parse-cftag-region)
    (defvar css-class-list-function)
    (setq-local css-class-list-function #'cftag-current-buffer-classes)
    (defvar css-id-list-function)
    (setq-local css-id-list-function #'cftag-current-buffer-ids))

  (setq imenu-create-index-function 'cftag-imenu-index)

  (setq-local sgml-empty-tags
	      ;; From HTML-4.01's loose.dtd, parsed with
	      ;; `sgml-parse-dtd', plus manual addition of "wbr".
	      '("area" "base" "basefont" "br" "col" "frame" "hr" "img" "input"
		"isindex" "link" "meta" "param" "wbr"
                "cfdump" "cfset" "cfinclude" "cfargument" "cfqueryparam" "cfparam" "cfsetting"))
  (setq-local sgml-unclosed-tags
	      ;; From HTML-4.01's loose.dtd, parsed with `sgml-parse-dtd'.
	      '("body" "colgroup" "dd" "dt" "head" "html" "li" "option"
		"p" "tbody" "td" "tfoot" "th" "thead" "tr"
                "cfelse" "cfelseif"))
  ;; It's for the user to decide if it defeats it or not  -stef
  ;; (make-local-variable 'imenu-sort-function)
  ;; (setq imenu-sort-function nil) ; sorting the menu defeats the purpose
  )

(defvar cftag-imenu-regexp
  "\\s-*<h\\([1-9]\\)[^\n<>]*>\\(<[^\n<>]*>\\)*\\s-*\\([^\n<>]*\\)"
  "A regular expression matching a head line to be added to the menu.
The first `match-string' should be a number from 1-9.
The second `match-string' matches extra tags and is ignored.
The third `match-string' will be the used in the menu.")

(defun cftag-imenu-index ()
  "Return a table of contents for an HTML buffer for use with Imenu."
  (let (toc-index)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward cftag-imenu-regexp nil t)
	(setq toc-index
	      (cons (cons (concat (make-string
				   (* 2 (1- (string-to-number (match-string 1))))
				   ?\s)
				  (match-string 3))
			  (line-beginning-position))
		    toc-index))))
    (nreverse toc-index)))

(define-minor-mode cftag-autoview-mode
  "Toggle viewing of HTML files on save (HTML Autoview mode).
With a prefix argument ARG, enable HTML Autoview mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

HTML Autoview mode is a buffer-local minor mode for use with
`cftag-mode'.  If enabled, saving the file automatically runs
`browse-url-of-buffer' to view it."
  nil nil nil
  :group 'sgml
  (if cftag-autoview-mode
      (add-hook 'after-save-hook 'browse-url-of-buffer nil t)
    (remove-hook 'after-save-hook 'browse-url-of-buffer t)))


(define-skeleton cftag-href-anchor
  "HTML anchor tag with href attribute."
  "URL: "
  ;; '(setq input "http:")
  "<a href=\"" str "\">" _ "</a>")

(define-skeleton cftag-name-anchor
  "HTML anchor tag with name attribute."
  "Name: "
  "<a name=\"" str "\""
  (if sgml-xml-mode (concat " id=\"" str "\""))
  ">" _ "</a>")

(define-skeleton cftag-headline-1
  "HTML level 1 headline tags."
  nil
  "<h1>" _ "</h1>")

(define-skeleton cftag-headline-2
  "HTML level 2 headline tags."
  nil
  "<h2>" _ "</h2>")

(define-skeleton cftag-headline-3
  "HTML level 3 headline tags."
  nil
  "<h3>" _ "</h3>")

(define-skeleton cftag-headline-4
  "HTML level 4 headline tags."
  nil
  "<h4>" _ "</h4>")

(define-skeleton cftag-headline-5
  "HTML level 5 headline tags."
  nil
  "<h5>" _ "</h5>")

(define-skeleton cftag-headline-6
  "HTML level 6 headline tags."
  nil
  "<h6>" _ "</h6>")

(define-skeleton cftag-horizontal-rule
  "HTML horizontal rule tag."
  nil
  (if sgml-xml-mode "<hr />" "<hr>") \n)

(define-skeleton cftag-image
  "HTML image tag."
  "Image URL: "
  "<img src=\"" str "\" alt=\"" _ "\""
  (if sgml-xml-mode " />" ">"))

(define-skeleton cftag-line
  "HTML line break tag."
  nil
  (if sgml-xml-mode "<br />" "<br>") \n)

(define-skeleton cftag-ordered-list
  "HTML ordered list tags."
  nil
  "<ol>" \n
  "<li>" _ (if sgml-xml-mode "</li>") \n
  "</ol>")

(define-skeleton cftag-unordered-list
  "HTML unordered list tags."
  nil
  "<ul>" \n
  "<li>" _ (if sgml-xml-mode "</li>") \n
  "</ul>")

(define-skeleton cftag-list-item
  "HTML list item tag."
  nil
  (if (bolp) nil '\n)
  "<li>" _ (if sgml-xml-mode "</li>"))

(define-skeleton cftag-paragraph
  "HTML paragraph tag."
  nil
  (if (bolp) nil ?\n)
  "<p>" _ (if sgml-xml-mode "</p>"))

(define-skeleton cftag-checkboxes
  "Group of connected checkbox inputs."
  nil
  '(setq v1 nil
	 v2 nil)
  ("Value: "
   "<input type=\"" (identity "checkbox") ; see comment above about identity
   "\" name=\"" (or v1 (setq v1 (skeleton-read "Name: ")))
   "\" value=\"" str ?\"
   (when (y-or-n-p "Set \"checked\" attribute? ")
     (funcall skeleton-transformation-function
	      (if sgml-xml-mode " checked=\"checked\"" " checked")))
   (if sgml-xml-mode " />" ">")
   (skeleton-read "Text: " (capitalize str))
   (or v2 (setq v2 (if (y-or-n-p "Newline after text? ")
		       (funcall skeleton-transformation-function
                                (if sgml-xml-mode "<br />" "<br>"))
		     "")))
   \n))

(define-skeleton cftag-radio-buttons
  "Group of connected radio button inputs."
  nil
  '(setq v1 nil
	 v2 (cons nil nil))
  ("Value: "
   "<input type=\"" (identity "radio") ; see comment above about identity
   "\" name=\"" (or (car v2) (setcar v2 (skeleton-read "Name: ")))
   "\" value=\"" str ?\"
   (when (and (not v1) (setq v1 (y-or-n-p "Set \"checked\" attribute? ")))
     (funcall skeleton-transformation-function
	      (if sgml-xml-mode " checked=\"checked\"" " checked")))
   (if sgml-xml-mode " />" ">")
   (skeleton-read "Text: " (capitalize str))
   (or (cdr v2) (setcdr v2 (if (y-or-n-p "Newline after text? ")
			       (funcall skeleton-transformation-function
                                        (if sgml-xml-mode "<br />" "<br>"))
			     "")))
   \n))

(define-skeleton cftag-navigational-links
  "Group of navigational links."
  nil
  "<nav>" \n
  "<ul>" \n
  "<li><a href=\"" (skeleton-read "URL: " "#") "\">"
  (skeleton-read "Title: ") "</a>"
  (if sgml-xml-mode (if sgml-xml-mode "</li>")) \n
  "</ul>" \n
  "</nav>")

(define-skeleton cftag-html5-template
  "Initial HTML5 template"
  nil
  "<!DOCTYPE html>" \n
  "<html lang=\"en\">" \n
  "<head>" \n
  "<meta charset=\"utf-8\">" \n
  "<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">" \n
  "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">" \n
  "<title>" (skeleton-read "Page Title: ") "</title>" \n
  "</head>" \n
  "<body>" \n
  "<div id=\"app\"></div>" \n
  "</body>" \n
  "</html>")

(provide 'cftag-mode)

;;; cftag-mode.el ends here
