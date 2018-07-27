Highlight selected place-value positions in numbers.

The `digit-groups` Emacs package adds the `digit-groups-mode`
minor mode, which makes it easier to read large numbers by
highlighting digits at selected place-value positions (e.g.,
thousands place, millions place, billions place, etc.).

The default configuration formats in bold every other group of
three digits.  So, for example, in `9876543210.123456789`, the
digits 3, 4, 5 and 9 are highlighted both before and after the
decimal (`.`).  This makes it easy to find the place-value
positions for thousands, millions, billions, and so forth.

To use this package, enable the `digit-groups-mode` minor mode in
the buffers in which you wish to use it, or to enable it for all
buffers, customize `digit-groups-global-mode` to `t`.

The default configuration highlights digits by making them bold.
This can be changed by customizing `digit-groups-default-face`,
or you can highlight different positions with different faces by
customizing `digit-groups-groups`.

The default configuration highlights every other group of three
digits between the novemdecillionths (10^-60) position and the
novemdecillions (10^60) position with the exception of the
units (10^0) position.  This can be changed by customizing
`digit-groups-groups`.

Changes to the configuration take effect only when the
`digit-groups-mode` minor mode is being turned on.  Thus, you may
need to toggle the mode off and on again in affected buffers
before you see the effect of any configuration changes.
