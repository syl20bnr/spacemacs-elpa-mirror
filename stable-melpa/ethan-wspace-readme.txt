For more information on the design of this package, please see the
README that came with it.

Editing whitespace. I don't like to forcibly clean[1] whitespace
because when doing diffs/checkins, it pollutes changesets. However,
I prefer clean whitespace -- often I've deleted whitespace by
accident and been unable to "put it back" for purposes of diffing.

Therefore, my approach is hybrid -- maintain clean whitespace where
possible, and avoid disturbing messy whitespce when I come across
it. I add a find-file-hook to track whether the whitespace of a
given file was clean to begin with, and add a before-save-hook that
uses this information to forcibly clean whitespace only if it was
clean at start.

Viewing whitespace. Originally I had a font-lock-mode hook that
always called show-trailing-whitespace and show-tabs, but this
turns out to be annoying for a lot of reasons. Emacs internal
buffers like *Completions* and *Shell* would get highlit. So I
decided that a more sophisticated approach was called for.

1) If a buffer does not correspond to a file, I almost certainly do
not care whether the whitespace is clean. I'm not going to be
saving it, after all.

2) If a buffer corresponds to a file that was originally clean, I
still don't really care about the whitespace, since my hook (above)
would ensure that it was clean going forward.

3) If a buffer corresponds to a file that was not originally clean,
I do care about seeing whitespace, because I do not want to edit it
by accident.

There are a few exceptions -- if I'm looking at a patch, or hacking
a Makefile, whitespace is different. More about those later.

This file adds hooks to make this stuff happen -- check whether the
whitespace is clean when a file is first found, and preserve that
cleanliness if so, and highlight that dirtiness if not. It does
this by setting show-trailing-whitespace when necessary.

NOTE: take out any customizations like this:
'(show-trailing-whitespace t)
show-trailing-whitespace will be turned on by ethan-wspace.

Also disable '(require-final-newlines t); ethan-wspace will handle
the final newlines. You might have to add
'(mode-require-final-newlines nil).
