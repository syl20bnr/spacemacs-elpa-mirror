This module provides a subtle visual indication which window is
currently active by dimming the faces on the others. It does this
nondestructively, and computes the dimmed faces dynamically such
that your overall color scheme is shown in a muted form without
requiring you to define the "dim" versions of every face.
The percentage of dimming is user configurable.

Unlike the 'hiwin' module which has a similar goal, this module
does *not* change the color of the background in any way. It only
adjusts foregrounds. In the underlying implementation we do not
use overlays, and therefore we avoid some of the visual problems
the hiwin module exhibits when highlighting interactive shells
and/or repls.

To use

    (require 'dimmer)
    (dimmer-activate)



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
