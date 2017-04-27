If you are working with XML documents, the parsed data structure
returned by the XML parser (xml.el) may be enough for you: Lists of
lists, symbols, strings, plus a number of accessor functions.

If you want a more elaborate data structure to work with your XML
document, you can create a document object model (DOM) from the XML
data structure using dom.el.

You can create a DOM from XML using `dom-make-document-from-xml'
with the input from `libxml-parse-xml-region'. See function documentation
below for an example

On Interfaces and Classes

The elisp DOM implementation uses the dom-node structure to store all
attributes.  The various interfaces consist of sets of functions to
manipulate these dom-nodes.  The functions of a certain interface
share the same prefix.
