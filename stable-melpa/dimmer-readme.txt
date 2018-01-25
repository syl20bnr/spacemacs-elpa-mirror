This module provides a minor mode that indicates which buffer is
currently active by dimming the faces in the other buffers.  It
does this nondestructively, and computes the dimmed faces
dynamically such that your overall color scheme is shown in a muted
form without requiring you to define what is a "dim" version of
every face.

The `default` background color is the target for all dimming
calculations.  If your default background is "white" then faces
will be made brighter when "dimmed".  If your default background is
a dark blue, then faces will be shifted "darker" and "more blue"
when buffers are dimmed.

Usage:

     (require 'dimmer) ; unless installed as a package
     (dimmer-mode)

Customization:

`dimmer-fraction` controls the degree to which buffers are dimmed.
Range is 0.0 - 1.0, and default is 0.20.  Increase value if you
like the other buffers to be more dim.

`dimmer-exclusion-regexp` can be used to specify buffers that
should never be dimmed.  If the buffer name matches this regexp
then `dimmer.el` will not dim that buffer.

`dimmer-use-colorspace` allows you to specify what color space the
dimming calculation is performed in.  In the majority of cases you
won't need to touch this setting.  See the docstring below for more
information.



This program is free software: you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
