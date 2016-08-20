Allows opening and closing prosjekt projects through anything, as well as
selection of files within a project.

To activate, add anything-prosjekt.el to your load path and

  (require 'anything-prosjekt)

to your .emacs. You can then use the following sources

 `anything-c-source-prosjekt-files'
   Search for files in the current prosjekt.
 `anything-c-source-prosjekt-projects'
   Open or close prosjekt projects.

like this

  (add-to-list 'anything-sources 'anything-c-source-prosjekt-files t)
  (add-to-list 'anything-sources 'anything-c-source-prosjekt-projects t)

Anything: http://www.emacswiki.org/emacs/Anything

License:

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use, copy,
modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
