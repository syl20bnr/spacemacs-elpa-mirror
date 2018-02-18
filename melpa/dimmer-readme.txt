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
