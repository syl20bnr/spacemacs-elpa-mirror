Easily allows you to resize windows. Rather than guessing that you
want `C-u 17 C-x {`, you could just press FFff, which enlarges 5
lines, then 5 lines, then one and then one. The idea is that the
normal motions n,p,f,b along with r for reset and w for cycling
windows allows for super simple resizing of windows. All of this is
inside of a while loop so that you don't have to invoke more chords
to resize again, but just keep using standard motions until you are
happy.

All of the work is done inside of resize-window. Its just a while
loop that keeps looping over character input until it doesn't
recognize an option or an allowable capital. The dispatch alist has
a character code to look for, a function to invoke, a string for
display and whether to match against capital letters. If so, it is
invoked with the default capital argument rather than the default
argument.
