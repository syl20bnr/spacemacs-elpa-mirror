Toggle checking of let assignments.  If point is inside a let form,
the variables in this let block are checked and if you reference a
previously defined variable in this let binding, it is highlight
with warning face, because you can't reference it.  You then need
to change the let into let*.

See github readme at https://github.com/Fuco1/letcheck
