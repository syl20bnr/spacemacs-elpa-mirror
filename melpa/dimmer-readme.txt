This module provides a subtle visual indication which window is
currently active by dimming the faces on the others.  It does this
nondestructively, and computes the dimmed faces dynamically such
that your overall color scheme is shown in a muted form without
requiring you to define the "dim" versions of every face.

The *percentage* of dimming is user configurable.

The *direction* of dimming is computed on the fly.  For instance,
if you have a dark theme then the dimmed face is darker, and if you
have a light theme the dimmed face is lighter.

Unlike the 'hiwin' module which has a similar goal, this module
does *not* change the color of the background in any way.  It only
adjusts foregrounds.  In the underlying implementation we do not
use overlays, and therefore we avoid some of the visual problems
the hiwin module exhibits when highlighting interactive shells
and/or repls.

Caveats:

This module makes use of the `face-remap-*` APIs in Emacs and these
APIs work on buffers rather than windows.  This means anytime you
have multiple windows displaying the same buffer they will dim or
undim together.  In my configuration I combine this package with
`global-hl-line-mode` so that it's also clear which window is
active.

Users of light themes may need to increase `dimmer-fraction` in
order to see the effect.

Usage:

     (require 'dimmer) ; unless installed as a package
     (dimmer-mode)

Customization:

`dimmer-fraction` controls the degree to which unselected buffers
are dimmed.  Range is 0.0 - 1.0, and default is 0.20.  Increase
value if you like the other buffers to be more dim.

Use `dimmer-exclusion-regexp` to describe patterns for buffer names
that should never be dimmed, for example, you could match buffers
created by helm.



This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
