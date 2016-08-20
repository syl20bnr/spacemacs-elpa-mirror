Add `company-tern' to allowed `company-mode' backends list

    (add-to-list 'company-backends 'company-tern)

If you don't like circles after object's own properties consider less
annoying marker for that purpose.

    (setq company-tern-property-marker "")

You can trim too long function signatures to the frame width.

    (setq company-tern-meta-as-single-line t)

If you doesn't like inline argument annotations appear with
corresponding identifiers, then you can to set up the company align
option.

    (setq company-tooltip-align-annotations t)
